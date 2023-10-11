box::use(
  shiny[navbarPage, tabPanel, navbarMenu, div, moduleServer, NS, renderUI, tags, uiOutput, icon],
  bslib[nav_spacer, nav_item, page_navbar, nav_panel]
)

box::use(
  app/view/opnirreikningar,
  app/view/rikisreikningur,
  app/view/landingpage
)

box::use(
  app/logic/ui_utils
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    title = "Gagnaþon Ríkiskaupa",
    nav_panel(
      "Heim",
      landingpage$ui("landingpage")
    ),
    nav_panel(
      title = "Opnir Reikningar",
      opnirreikningar$ui("opnirreikningar")
    ),
    nav_panel(
      title = "Ríkisreikningur",
      rikisreikningur$ui("rikisreikningur")
    ),
    nav_spacer(),
    nav_item(ui_utils$link_github()),
    nav_item(ui_utils$link_tolfraedi_hi())
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    landingpage$server("landingpage")
    opnirreikningar$server("opnirreikningar")
    rikisreikningur$server("rikisreikningur")
  })
}
