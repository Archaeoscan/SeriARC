# ui_import_content.R - Import Tab UI Content

# IMPORT TAB UI MODULE
output$import_main_content <- renderUI({
  # Show welcome screen (format info) when no user file uploaded
  # Use is_example_data() reactive value from import module

  # If example dataset active (no user upload): show welcome_data_ui (format info)
  # If user uploaded own file: show data_summary_ui
  if (is_example_data()) {
    welcome_data_ui(tr = tr)  # Show format info
  } else {
    data_summary_ui(tr = tr)  # Data summary after upload
  }
})
