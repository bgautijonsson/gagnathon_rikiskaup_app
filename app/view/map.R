box::use(
  shiny[moduleServer, NS],
  bslib[page_sidebar, sidebar]
)

box::use(
  app/logic/map_utils
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      width = 300,
      map_utils$select_seller("seller")
    )
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}
