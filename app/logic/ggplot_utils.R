# app/logic/ggplot_utils.R

#' @export
cut_isk_scale <- function() {
  c(0, " þús." = 100, "m" = 1e6, "M" = 1e9)
}