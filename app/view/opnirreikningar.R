# app/view/opnirreikningar.R

box::use(
  shiny[
    moduleServer, NS, 
    selectInput, actionButton,
    renderUI, uiOutput,
    renderText, textOutput,
    renderPlot, plotOutput,
    mainPanel, 
    bindEvent,
    h2, h3, h4, p
  ],
  bslib[page_sidebar, sidebar, card, layout_columns],
  gt[render_gt, gt_output]
)

box::use(
  app/logic/data[or_data],
  app/logic/or_utils,
  app/logic/landingpage_utils,
  app/logic/sidebar_utils
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      width = 300,
      selectInput(
        inputId = ns("kaupandi"),
        label = "Birgir",
        choices = or_data |> or_utils$get_unique(kaupandi),
        selected = "Advania Ísland ehf."
      ),
      uiOutput(ns("birgi")),
      sidebar_utils$sidebar_info
    ),
    layout_columns(
      col_widths = c(4, 4, 4, 8, 4), 
      row_heights = c(1, 4),
      !!!or_utils$vbs(
        text1 = textOutput(ns("text1"), container = h3),
        text2 = textOutput(ns("text2"), container = p),
        text3 = textOutput(ns("text3"), container = h3)
      ),
      plotOutput(ns("line_plot"), height = "600px"),
      gt_output(ns("table")),
      fillable = FALSE,
      fill = FALSE
    )
    
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$birgi <- renderUI({
      or_utils$select_birgi(or_data, input, ns("birgi"))
    })
    
    output$line_plot <- renderPlot({
      or_utils$line_plot(or_data, input)
    }) 
    
    output$table <- render_gt({
      or_utils$table(or_data, input)
    })
    
    output$text1 <- renderText({
      or_utils$num_sellers(or_data, input)
    })
    
    output$text2 <- renderText({
      or_utils$most_selling(or_data, input)
    })
    
    output$text3 <- renderText({
      or_utils$total_value(or_data, input)
    })
  })
}