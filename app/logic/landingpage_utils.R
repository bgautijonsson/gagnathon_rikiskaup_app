# app/logic/landingpage_utils.R

box::use(
  bslib[value_box],
  plotly[plot_ly, add_lines, layout, config],
  bsicons[bs_icon],
  shinipsum[random_ggplotly, random_text],
  shiny[p]
)

box::use(
  app/logic/plotly_utils
)

card_height <- 100

#' @export
vbs <- list(
  value_box(
    title = "Kaupendur",
    value = random_text(nwords = "10"),
    theme = "purple",
    showcase = random_ggplotly(type = "point") |> plotly_utils$card_theme(),
    full_screen = TRUE,
    p(random_text(nwords = "5")),
    height = card_height
  ),
  value_box(
    title = "Seljendur",
    value = random_text(nwords = "10"),
    theme = "bg-success",
    showcase = random_ggplotly(type = "line") |> plotly_utils$card_theme(),
    full_screen = TRUE,
    p(random_text(nwords = "5")),
    height = card_height
  ),
  value_box(
    title = "Fjármagn",
    value = random_text(nwords = "10"),
    theme = "pink",
    showcase = random_ggplotly(type = "ribbon") |> plotly_utils$card_theme(),
    full_screen = TRUE,
    p(random_text(nwords = "5")),
    height = card_height
  )
)
