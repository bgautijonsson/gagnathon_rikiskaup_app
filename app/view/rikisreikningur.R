# app/view/opnirreikningar.R

box::use(
  shiny[h3, moduleServer, NS],
  bslib[page_sidebar, sidebar]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  page_sidebar(
    sidebar = sidebar()
    
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}