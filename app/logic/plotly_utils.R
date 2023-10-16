# app/logic/plotly_utils.R

box::use(
  plotly[layout, config],
  htmlwidgets[onRender]
)
#' @export
card_theme <- function(plotly_plot) {
  plotly_plot |> 
    layout(
      xaxis = list(visible = F, showgrid = F, title = ""),
      yaxis = list(visible = F, showgrid = F, title = ""),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "white"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) |> 
    config(displayModeBar = F) |> 
    onRender(
      "function(el) {
      var ro = new ResizeObserver(function() {
         var visible = el.offsetHeight > 200;
         Plotly.relayout(el, {'xaxis.visible': visible});
      });
      ro.observe(el);
    }"
    )
}
