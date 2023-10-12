# app/view/opnirreikningar.R

box::use(
  shiny[moduleServer, NS, tags, plotOutput, h1],
  bslib[page_fluid, layout_column_wrap, card, card_header, card_body],
  lorem
)

box::use(
  app/logic/landingpage_utils[vbs]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  
  page_fluid(
    h1("Titill á þessu appi/verkefni/etc"),
    layout_column_wrap(
      width = 1/3,
      heights_equal = "row",
      !!!vbs
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}