# app/view/opnirreikningar.R

box::use(
  shiny[moduleServer, NS, tags, plotOutput, h1],
  bslib[page_fluid, layout_column_wrap, card, card_header, card_body],
  lorem
)

card1 <- card(
  card_header("Velkomin"),
  lapply(
    lorem$ipsum(paragraphs = 3, sentences = c(5, 5, 5)),
    tags$p
  )
)

card2 <- card(
  card_header("Nothing much here"),
  "This is it."
)

card3 <- card(
  full_screen = TRUE,
  card_header("Filling content"),
  card_body(
    class = "p-0",
    plotOutput("p")
  )
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  page_fluid(
    h1("Hvað er dót?"),
    layout_column_wrap(
      width = 1/3, height = 300,
      heights_equal = "row",
      card1, card2, card3
    )
    
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}