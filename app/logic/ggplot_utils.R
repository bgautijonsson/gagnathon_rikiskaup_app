# app/logic/ggplot_utils.R

box::use(
  scales[label_dollar, dollar],
  RColorBrewer[brewer.pal]
)

#' @export
cut_isk_scale <- function() {
  c(0, " þús." = 100, "m" = 1e6, "M" = 1e9)
}


#' @export
label_isk <- function() {
  label_dollar(prefix = "", suffix = " kr", scale_cut = cut_isk_scale())
}


#' @export
isk <- function(x) {
  dollar(x, prefix = "", suffix = " kr", big.mark = ".", decimal.mark = ",")
}

#' @export
get_colors <- function(n) {
  if (n >= 3) {
    return(brewer.pal(n = n, name = "Set1"))
  } else {
    out <- c("#e41a1c", "#377eb8")
    return(out[seq_len(n)])
  }
}