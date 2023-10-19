# app/logic/or_utils.R

box::use(
  dplyr[distinct, filter, count, collect, mutate, summarise, pull, arrange, desc, slice, group_by, tibble, inner_join, join_by, left_join],
  ggplot2[ggplot, aes, geom_line, geom_point, scale_y_continuous, labs, expansion, scale_color_identity],
  RColorBrewer[brewer.pal],
  shiny[selectizeInput, p, req, div],
  lubridate[floor_date],
  plotly[ggplotly],
  scales[label_dollar, cut_short_scale],
  bslib[value_box],
  bsicons[bs_icon],
  gt[gt, tab_header, cols_label, fmt_currency, opt_interactive],
  gtExtras[gt_highlight_rows],
  forcats[fct_reorder]
)

box::use(
  app/logic/ggplot_utils,
  app/logic/theme
)

#' @export
get_tegundir <- function() {
  c(
    "Húsaleiga",
    "Hugbúnaður",
    "Sérfræðiþjónusta",
    "Tölvur, tölvuvinnsla og rekstur",
    "Ræsting",
    "Matur og drykkur",
    "Bifreiðar (Leigðar)",
    "Bifreiðar (Eign)",
    "Vatn, hiti og rafmagn"
  )
}


# Data Logic --------------------------------------------------------------
#' @export
num_sellers <- function(data, input) {
  data |> 
    filter(
      tegund %in% !!input$tegund
    ) |> 
    distinct(kaupandi) |> 
    collect() |> 
    nrow()
}

#' @export
most_selling <- function(data, input) {
  data |> 
    filter(
      tegund %in% !!input$tegund
    ) |> 
    count(kaupandi, wt = kr) |> 
    arrange(desc(n)) |> 
    collect() |> 
    slice(1) |> 
    pull(kaupandi)
}

#' @export
total_value <- function(data, input) {
  data |> 
    filter(
      tegund %in% !!input$tegund
    ) |> 
    summarise(
      total = sum(kr)
    ) |> 
    collect() |> 
    pull(total) |> 
    ggplot_utils$isk()
}


# Tables ------------------------------------------------------------------
#' @export
table <- function(data, input) {
  req(input$kaupendur)
  tab_dat <- data |> 
    filter(
      tegund == input$tegund
    ) |> 
    count(kaupandi, wt = kr, name = "kr") |> 
    filter(kr != 0) |> 
    collect() |> 
    arrange(desc(kaupandi %in% input$kaupendur), desc(kr))
  
  tab <- tab_dat |> 
    gt() |> 
    tab_header(
      title = "Heildarkaup tímabils eftir stofnun"
    ) |> 
    cols_label(
      kaupandi = "kaupandir",
      kr = "Heildarútgjöld"
    ) |> 
    fmt_currency(
      kr, 
      currency = "ISK",
      placement = "right"
    ) |> 
    opt_interactive(
      use_sorting = F
    )
  
  
  values <- input$kaupendur
  colors <- ggplot_utils$get_colors(n = length(values))
  
  
  for (i in seq_along(input$kaupendur)) {
    tab <- tab |> 
      gt_highlight_rows(
        rows = (kaupandi == values[i]),
        fill = colors[i],
        font_weight = 500, 
        font_color = "#000000",
        alpha = 0.6
      )
  }
  
  tab
}

# Plots -------------------------------------------------------------------


#' @export
line_plot <- function(data, input) {
  req(input$kaupendur)
  plot_dat <- data |> 
    filter(
      tegund %in% !!input$tegund,
      kaupandi %in% !!input$kaupendur
    ) |> 
    collect() |> 
    mutate(
      kaupandi = fct_reorder(kaupandi, kr)
    )
  
  
  colors <- tibble(
    kaupandi = input$kaupendur,
    color = ggplot_utils$get_colors(n = length(input$kaupendur))
  )
  
  
  
  p <- plot_dat |> 
    inner_join(
      colors,
      by = join_by(kaupandi)
    ) |> 
    ggplot(aes(dags, kr, group = kaupandi, col = color)) +
    geom_line(
      linewidth = 1.3
    ) +
    geom_point(
      size = 4
    ) +
    scale_y_continuous(
      labels = ggplot_utils$label_isk(),
      limits = 1.05 * c(0, max(plot_dat$kr)),
      expand = expansion()
    ) +
    scale_color_identity() +
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
select_kaupendur <- function(data, input, ns) {
  
  choices <- data |> 
    filter(tegund == !!input$tegund) |> 
    count(kaupandi, wt = kr) |> 
    arrange(desc(n)) |> 
    collect() |> 
    slice(1:30) |>  
    pull(kaupandi)
  
  selectizeInput(
    inputId = ns,
    label = "Kaupandi",
    choices = choices, 
    selected = choices[1:3],
    multiple = TRUE,
    options = list(maxItems = 9)
  )
}


# UI Elements -------------------------------------------------------------
card_height <- "160px"
#' @export
vbs <- function(text1, text2, text3) {
  list(
    value_box(
      title = "Fjöldi kaupenda",
      value = text1,
      showcase = bs_icon("bank"),
      theme = "primary",
      height = card_height,
      max_height = card_height,
      style = theme$vbox_style(),
      fill = TRUE,
    ),
    value_box(
      title = "Mest notað af",
      value = text2,
      showcase = bs_icon("building"),
      theme = "secondary",
      height = card_height,
      max_height = card_height,
      style = theme$vbox_style(),
      fill = TRUE
    ),
    value_box(
      title = "Samtals keypt fyrir",
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
