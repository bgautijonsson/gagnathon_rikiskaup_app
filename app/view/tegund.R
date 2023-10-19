# app/view/tegund.R

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
  app/logic/data[tegund_data],
  app/logic/tegund_utils,
  app/logic/sidebar_utils
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      width = 300,
      selectInput(
        inputId = ns("tegund"),
        label = "Útgjaldategund",
        choices = tegund_utils$get_tegundir(),
        selected = "Hugbúnaður"
      ),
      uiOutput(ns("kaupendur")),
      sidebar_utils$sidebar_info
    ),
    layout_columns(
      col_widths = c(4, 4, 4, 8, 4), 
      row_heights = c(1, 4),
      !!!tegund_utils$vbs(
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
    output$kaupendur <- renderUI({
      
      tegund_utils$select_kaupendur(tegund_data, input, ns("kaupendur"))
    })
    
    output$line_plot <- renderPlot({
      tegund_utils$line_plot(tegund_data, input)
    }) 
    
    output$table <- render_gt({
      tegund_utils$table(tegund_data, input)
    })
    
    output$text1 <- renderText({
      tegund_utils$num_sellers(tegund_data, input)
    })
    
    output$text2 <- renderText({
      tegund_utils$most_selling(tegund_data, input)
    })
    
    output$text3 <- renderText({
      tegund_utils$total_value(tegund_data, input)
    })
  })
}