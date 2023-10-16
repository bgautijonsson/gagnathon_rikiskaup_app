# app/logic/or_utils.R

box::use(
  dplyr[distinct, filter, count, collect, mutate, summarise, pull, arrange, desc, slice],
  ggplot2[ggplot, aes, geom_line, geom_point, scale_y_continuous, labs, expansion],
  shiny[selectInput],
  visitalaneysluverds[vnv_convert],
  lubridate[floor_date],
  plotly[ggplotly],
  scales[label_dollar, cut_short_scale]
)

box::use(
  app/logic/ggplot_utils
)

#' @export
get_unique <- function(data, var) {
  data |> distinct({{ var }}) |> collect() |> pull({{ var }})
}

#' @export
line_plot <- function(data, input) {
  plot_dat <- data |> 
    filter(
      kaupandi %in% !!input$kaupandi,
      birgi %in% !!input$birgi
    ) |> 
    count(kaupandi, birgi, dags_greidslu, wt = upphaed_linu, name = "kr") |> 
    collect() |> 
    mutate(
      kr = vnv_convert(kr, obs_date = dags_greidslu),
      .by = birgi
    ) |>
    mutate(
      dags_greidslu = floor_date(dags_greidslu, "year")
    ) |> 
    summarise(
      kr = sum(kr),
      .by = c(kaupandi, birgi, dags_greidslu)
    )
  
  p <- plot_dat |> 
    ggplot(aes(dags_greidslu, kr, group = birgi, col = birgi)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(
      labels = label_dollar(prefix = "", suffix = " kr", scale_cut = ggplot_utils$cut_isk_scale()),
      limits = 1.05 * c(0, max(plot_dat$kr)),
      expand = expansion()
    ) +
    labs(
      title = "Útgjöld síðasta ár",
      x = NULL,
      y = NULL
    )
  
    ggplotly(
      p
    )
}


#' @export
select_birgi <- function(data, input, ns) {
  
  choices <- data |> 
    filter(kaupandi == !!input$kaupandi) |> 
    count(birgi, wt = upphaed_linu) |> 
    arrange(desc(n)) |> 
    collect() |> 
    slice(1:30) |>  
    pull(birgi)
  
  selectInput(
    inputId = ns,
    label = "Birgi",
    choices = choices, 
    selected = choices[1:3],
    selectize = TRUE, 
    multiple = TRUE
  )
  
}