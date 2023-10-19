# app/logic/sidebar_utils.R

box::use(
  shiny[br, h5, p, HTML]
)

#' @export
sidebar_info <- paste0(
  br(" "),
  h5("Höfundar:"),
  p("Anna Eva Steindórsdóttir"), 
  p("Brynjólfur G. Guðrúnar Jónsson"), 
  p("Rafael Daníel Vias")
) |> 
  HTML()