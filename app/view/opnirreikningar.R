# app/view/opnirreikningar.R

box::use(
  shiny[h3, moduleServer, NS, selectInput, renderUI, uiOutput, actionButton, textOutput, mainPanel, bindEvent, need, validate],
  bslib[page_sidebar, sidebar],
  dplyr[distinct, collect, pull, filter, count, arrange, desc, slice, mutate],
  plotly[renderPlotly, ggplotly, plotlyOutput],
  ggplot2[ggplot, aes, geom_line, geom_point, scale_y_continuous, labs],
  lubridate[floor_date]
)

box::use(
  app/logic/data[or_data],
  app/logic/or_utils
)




#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      width = 300,
      selectInput(
        inputId = ns("kaupandi"),
        label = "Kaupandi",
        choices = or_data |> or_utils$get_unique(kaupandi)
      ),
      uiOutput(ns("birgi")),
      actionButton(
        inputId = ns("go"),
        label = "Sækja gögn"
      )
    ),
    plotlyOutput(ns("line_plot"))
    
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$birgi <- renderUI({
      
      or_utils$select_birgi(or_data, input, ns("birgi"))
    })
    
    output$line_plot <- renderPlotly({
      p <- or_utils$line_plot(or_data, input)
    }) |> 
      bindEvent(
        input$go
      )
  })
}