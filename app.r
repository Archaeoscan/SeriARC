# SeriARC v2.0 - modular launcher
source("global.R", local = TRUE)
source("ui_main.R", local = TRUE)
if (dir.exists("R")) {
  for (f in list.files("R", pattern="\\.[rR]$", full.names = TRUE))
    source(f, local = TRUE)
}
source("server_main.R", local = TRUE)
# Set session timeout to 8 hours (prevents automatic closing on inactivity)
options(
  shiny.sessiontimeout = 28800000   # 8h in milliseconds
)
ui <- function(request) {
  query <- parseQueryString(request$QUERY_STRING %||% "")
  lang <- query$lang %||% DEFAULT_LANGUAGE
  if (!lang %in% AVAILABLE_LANGUAGES) lang <- DEFAULT_LANGUAGE

  tr <- make_tr(lang)

  # Build UI with translations
  create_ui(tr, lang)
}
shinyApp(ui = ui, server = server)
