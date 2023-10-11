# app/view/opnirreikningar.R

box::use(
  bslib[nav_item],
  shiny[tags, icon]
)

#' @export
link_github <- function() {
  tags$a(
    icon("github"), 
    "Kóði", 
    href = "https://github.com/bgautijonsson/gagnathon_rikiskaup", 
    target = "_blank"
  )
}


#' @export
link_tolfraedi_hi <- function() {
  tags$a(
    icon("school"), 
    "Tölfræði við HÍ", 
    href = "https://www.hi.is/framhaldsnam/tolfraedi", 
    target = "_blank"
  )
}
