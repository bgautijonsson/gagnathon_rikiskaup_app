box::use(
  shiny[moduleServer, NS, checkboxInput, p],
  bslib[page_sidebar, sidebar],
  leaflet[renderLeaflet, leafletOutput],
  shinyWidgets[switchInput, materialSwitch]
)

box::use(
  app/logic/map_utils
)

box::use(
  app/logic/data[food_data, coord_data],
  app/logic/sidebar_utils
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      width = 300,
      map_utils$select_seller(ns("seller")),
      materialSwitch(
        ns("cluster"),
        "Hópa saman nálægum stofnunum?",
        status = "primary"
      ),
      sidebar_utils$sidebar_info
    ),
    leafletOutput(ns("map"))
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$map <- renderLeaflet({
      map_utils$make_map(food_data, coord_data, input)
    })
  })
}
