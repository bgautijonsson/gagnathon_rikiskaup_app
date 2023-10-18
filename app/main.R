box::use(
  shiny[navbarPage, tabPanel, navbarMenu, div, moduleServer, NS, renderUI, tags, uiOutput, icon],
  bslib[nav_spacer, nav_item, page_navbar, nav_panel, bs_theme],
  thematic[thematic_shiny],
  ggplot2[theme_set]
)

box::use(
  app/view/opnirreikningar,
  app/view/rikisreikningur,
  app/view/landingpage,
  app/view/map
)

box::use(
  app/logic/ui_utils,
  app/logic/theme
)


theme_set(theme$ggplot_theme())
thematic_shiny()

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    theme = theme$shiny_theme(),
    title = "Gagnaþon Ríkiskaupa",
    nav_panel(
      title = "Opnir Reikningar",
      opnirreikningar$ui(ns("opnirreikningar"))
    ),
    nav_panel(
      title = "Landfræðileg Greining",
      map$ui(ns("map"))
    ),
    nav_spacer(),
    nav_item(ui_utils$link_github()),
    nav_item(ui_utils$link_tolfraedi_hi())
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    map$server("map")
    opnirreikningar$server("opnirreikningar")
  })
}
