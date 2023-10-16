# app/logic/or_utils.R

box::use(
  dplyr[distinct, filter, count, collect, mutate, summarise, pull, arrange, desc, slice],
  ggplot2[ggplot, aes, geom_line, geom_point, scale_y_continuous, labs, expansion],
  shiny[selectInput, p, req, div],
  visitalaneysluverds[vnv_convert],
  lubridate[floor_date],
  plotly[ggplotly],
  scales[label_dollar, cut_short_scale],
  bslib[value_box],
  bsicons[bs_icon],
  gt[gt, tab_header, cols_label, fmt_currency, opt_interactive]
)

box::use(
  app/logic/ggplot_utils,
  app/logic/theme
)

#' @export
get_unique <- function(data, var) {
  data |> distinct({{ var }}) |> collect() |> pull({{ var }})
}


# Data Logic --------------------------------------------------------------
#' @export
num_sellers <- function(data, input) {
  data |> 
    filter(
      kaupandi %in% !!input$kaupandi
    ) |> 
    distinct(birgi) |> 
    collect() |> 
    nrow()
}

#' @export
most_selling <- function(data, input) {
  data |> 
    filter(
      kaupandi %in% !!input$kaupandi
    ) |> 
    count(birgi, wt = upphaed_linu) |> 
    arrange(desc(n)) |> 
    collect() |> 
    slice(1) |> 
    pull(birgi)
}

#' @export
total_value <- function(data, input) {
  data |> 
    filter(
      kaupandi %in% !!input$kaupandi
    ) |> 
    summarise(
      total = sum(upphaed_linu)
    ) |> 
    collect() |> 
    pull(total) |> 
    ggplot_utils$isk()
}


# Tables ------------------------------------------------------------------
#' @export
table <- function(data, input) {
  data |> 
    filter(
      kaupandi == input$kaupandi
    ) |> 
    count(birgi, wt = upphaed_linu, name = "kr") |> 
    filter(kr != 0) |> 
    collect() |> 
    arrange(desc(kr)) |> 
    gt() |> 
    tab_header(
      title = "Heildarútgjöld tímabils eftir birgi"
    ) |> 
    cols_label(
      birgi = "Birgir",
      kr = "Heildarútgjöld"
    ) |> 
    fmt_currency(
      kr, 
      currency = "ISK",
      placement = "right"
    ) |> 
    opt_interactive(
      use_sorting = T,
      use_search = T
    )
}

# Plots -------------------------------------------------------------------


#' @export
line_plot <- function(data, input) {
  req(input$birgi)
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
      labels = ggplot_utils$label_isk(),
      limits = 1.05 * c(0, max(plot_dat$kr)),
      expand = expansion()
    ) +
    labs(
      title = "Árleg þróun útgjalda",
      x = NULL,
      y = NULL,
      color = NULL
    )
  
  p
}


# Inputs ------------------------------------------------------------------


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


# UI Elements -------------------------------------------------------------
card_height <- "160px"
#' @export
vbs <- function(text1, text2, text3) {
  list(
      value_box(
        title = "Fjöldi viðskiptavina",
        value = text1,
        showcase = bs_icon("bank"),
        theme = "primary",
        height = card_height,
        max_height = card_height,
        style = theme$vbox_style(),
        fill = TRUE,
      ),
    value_box(
      title = "Mest útgjöld til",
      value = text2,
      showcase = bs_icon("building"),
      theme = "secondary",
      height = card_height,
      max_height = card_height,
      style = theme$vbox_style(),
      fill = TRUE
    ),
    value_box(
      title = "Samtals útgjöld",
      value = text3,
      showcase = bs_icon("cash"),
      theme = "success",
      height = card_height,
      max_height = card_height,
      style = theme$vbox_style(),
      fill = TRUE
    )
  )
}
