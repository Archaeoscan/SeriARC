# ======================= mod_export.R =======================
# Export functions for scientific reproducibility (Siegmund 2020)
# Data export, CSV compatibility, quality assurance
# PHASE 2: Integration with UI components

# Helper source for standardized templates
source("helpers/download_components.R", local = TRUE)

mod_export_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)
  NULL # UI defined in ui.R
}

mod_export_server <- function(filtered_data, meta_data, ca_result, kmeans_result, seriation_result, input, output, session, tr = function(x) x) {

    # Filtered data export (scientific reproducibility) - TEMPLATE INTEGRATED ✅
    output$export_filtered_data <- create_excel_download_handler(
      data_sheets_func = function() {
        req(filtered_data())
        mat <- filtered_data()
        list(
          "Gefilterte_Matrix" = matrix_to_excel_format(mat, "Entity"),
          "Statistiken" = create_statistics_sheet(mat, tr("export.sheet.data"))
        )
      },
      base_filename = tr("export.sheet.data"),
      export_type = tr("export.sheet.matrix.repro")
    )

    # Metadata export (Entity management) - TEMPLATE INTEGRATED ✅
    output$export_meta_data <- create_excel_download_handler(
      data_sheets_func = function() {
        req(meta_data$data)
        list(
          "Sites_Selection" = if(!is.null(meta_data$sites)) meta_data$sites else data.frame(Note = "No sites selection"),
          "Types_Selection" = if(!is.null(meta_data$types)) meta_data$types else data.frame(Note = "No types selection")
        )
      },
      base_filename = tr("export.sheet.metadata"),
      export_type = tr("export.sheet.entity")
    )

    # ✅ INTEGRATED: CA export via create_ca_excel_sheets() (Greenacre 2007)
    # ✅ INTEGRATED: K-Means export via create_kmeans_excel_sheets()
    # ✅ INTEGRATED: Seriation export via create_seriation_excel_sheets() (Ihm 1983)
    # REDUCTION: ~150+ lines of legacy code replaced by standardized templates

    # Complete export (all analyses)
    output$export_all_data <- downloadHandler(
      filename = function() sprintf(tr("export.file.complete"), Sys.Date()),
      content = function(file) {
        data_list <- list("SeriARC_Info" = data.frame(
          Parameter = c(tr("export.label.software"), tr("export.label.version"), tr("export.label.type"), tr("export.label.date")),
          Wert = c("SeriARC", "v1.0.0", "Komplett-Export", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        ))

        # Add data
        if (!is.null(filtered_data())) {
          mat <- filtered_data()
          data_list[["Gefilterte_Matrix"]] <- data.frame(Entity = rownames(mat), mat, check.names = FALSE)
        }
        if (!is.null(ca_result())) data_list <- c(data_list, create_ca_excel_sheets(ca_result(), filtered_data()))
        if (!is.null(kmeans_result())) data_list <- c(data_list, create_kmeans_excel_sheets(kmeans_result()))
        if (!is.null(seriation_result())) data_list <- c(data_list, create_seriation_excel_sheets(seriation_result()))

        writexl::write_xlsx(data_list, file)
        showNotification(tr("export.success.complete"), type = "message", duration = 4)
      }
    )

    # Seriation export (chronological order)
    output$export_seriation_complete <- downloadHandler(
      filename = function() sprintf(tr("export.file.seriation"), Sys.Date()),
      content = function(file) {
        data_list <- list("SeriARC_Info" = data.frame(
          Parameter = c(tr("export.label.software"), tr("export.label.version"), tr("export.label.analysis"), tr("export.label.exported")),
          Wert = c("SeriARC", "v1.0.0", "Seriation", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        ))
        if (!is.null(filtered_data())) {
          mat <- filtered_data()
          data_list[["Gefilterte_Matrix"]] <- data.frame(Entity = rownames(mat), mat, check.names = FALSE)
        }
        if (!is.null(seriation_result())) data_list <- c(data_list, create_seriation_excel_sheets(seriation_result()))
        writexl::write_xlsx(data_list, file)
        showNotification(tr("export.success.seriation"), type = "message")
      }
    )

    # CSV export for seriation (compatibility) - TEMPLATE INTEGRATED ✅
    output$export_seriation_table_csv <- create_csv_download_handler(
      data_func = function() {
        if (!is.null(seriation_result())) {
          permuted_mat <- as.matrix(seriation_result()$permuted_matrix)
          matrix_to_excel_format(permuted_mat, "Entity")
        } else {
          data.frame(Entity = "No_Seriation", Info = "Run seriation first")
        }
      },
      base_filename = "Seriation",
      add_header = TRUE
    )

    # Return for other modules (integrated functions)
    return(list(
      create_ca_export = create_ca_excel_sheets,
      create_kmeans_export = create_kmeans_excel_sheets,
      create_seriation_export = create_seriation_excel_sheets
    ))
}
