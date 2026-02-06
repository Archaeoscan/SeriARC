# ui_import_content.R - Import Tab UI Content

# IMPORT TAB UI MODULE
# Only render data summary after upload (welcome & loading are static in ui_main.R)
output$import_main_content <- renderUI({
  # Only show data summary when user uploaded a file
  if (!is_example_data() && !is.null(data_raw())) {
    data_summary_ui(tr = tr)
  } else {
    NULL  # Show nothing (welcome screen is already static in ui_main.R)
  }
})
