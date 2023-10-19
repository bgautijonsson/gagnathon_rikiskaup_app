# app/logic/map_utils.R

box::use(
  shiny[selectizeInput, selectInput, validate, need],
  glue[glue],
  dplyr[mutate, filter, summarise, inner_join, join_by, rename, pull],
  sf[st_as_sf, st_set_crs],
  leaflet[
    leaflet, colorNumeric, addProviderTiles, 
    addCircleMarkers, providers, markerClusterOptions,
    addLayersControl, layersControlOptions, 
    JS, markerOptions, addMeasure, addLegend
  ],
  htmltools[HTML],
  htmlwidgets[onRender]
)

box::use(
  app/logic/data[food_data],
  app/logic/ggplot_utils[isk]
)


#' @export
select_seller <- function(ns) {
  
  food_choices <- unique(food_data$birgi)
  
  selectizeInput(
    inputId = ns,
    label = HTML("Veitingastaður<br><small>Hægt er að velja fleiri en einn stað</small>"),
    choices = food_choices, 
    selected = "Múlakaffi ehf.",
    multiple = TRUE
  )
}

#' @export
select_plotvar <- function(ns) {
  var_choices <- c(
    "Krónur" = "kr", 
    "Krónur á starfsmann" = "kr_per_staff",
    "Fjöldi kaupa" = "fjoldi_kaupa"
  )
  
  selectInput(
    inputId = ns,
    label = "Breyta",
    choices = var_choices, 
    selected = var_choices[1]
  )
}

#' @export
make_labels <- function(data) {
  data |> 
    mutate(
      label = glue(
        paste0(
          "<b>{kaupandi}</b><br>",
          "Fjöldi kaupa: {fjoldi_kaupa}<br>",
          "Heildarkaup (kr): {isk(kr)}<br>",
          "Heildarkaup á mánuði (kr/mán): {isk(kr/manudir)}<br>",
          "Kaup per stöðugildi (kr): {isk(kr_per_staff)}<br>",
          "Kaup per stöðugildi á mánuði (kr/mán): {isk(kr_per_staff/manudir)}"
        )
      )
    )
}

#' @export
make_color_pal <- function(x) {
  colorNumeric(
    "Reds",
    domain = c(min(x), max(x)),
    reverse = FALSE
  )
}


#' @export
make_map <- function(food_data, coord_data, input) {
  
  validate(
    need(input$seller, "Vinsamlegast veldu að minnsta kost einn seljanda.")
  )
  
  plot_dat <- food_data |> 
    filter(
      birgi %in% input$seller
    ) |> 
    summarise(
      kr = sum(kr),
      kr_per_staff = sum(kr_per_staff),
      fjoldi_kaupa = sum(fjoldi_kaupa),
      manudir = as.numeric(max(dags) - min(dags)) / 30,
      .by = kaupandi
    ) |> 
    inner_join(
      coord_data,
      by = join_by(kaupandi)
    ) |> 
    st_as_sf(
      coords = c("e_hnit", "n_hnit")
    ) |> 
    st_set_crs("WGS84") |> 
    make_labels() 
  
  if (isTRUE(input$cluster)) {
    clust_opt <- markerClusterOptions(
      zoomToBoundsOnClick = FALSE
    )
  } else {
    clust_opt <- NA
  }
  p <- plot_dat  |> 
    leaflet() |> 
    addProviderTiles(providers$Esri.WorldStreetMap)
  
  
  pal <- list()
  my_vars <- c( "kr_per_staff", "kr", "fjoldi_kaupa")
  my_var_names <- c("Krónum per starfsmann", "Krónutölu", "Fjölda kaupa")
  for (i in seq_along(my_vars)) {
    var <- my_vars[i]
    values <- plot_dat |> pull(var)
    values <- 1e5 * values / max(values)
    pal[[i]] <- make_color_pal(values)
    p <- p |> 
      addCircleMarkers(
        fillColor = ~ pal[[i]](values),
        color = "black",
        weight = 1,
        fillOpacity = ~ log(values),
        radius = ~ log(values),
        label = ~ lapply(label, HTML),
        group = my_var_names[i],
        clusterOptions = clust_opt
      )
    
  }
  custom_legend_values <- seq(0, 1, length.out = 7)
  legend_pal <- make_color_pal(custom_legend_values)
  p |> 
    addLayersControl(
      baseGroups = my_var_names,
      options = layersControlOptions(collapsed = FALSE)
    ) |> 
    onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<label style=\"text-align:center;font-weight:700;font-size:20px\">Lita eftir</label>');
        }
    ") |> 
    addLegend(
      "topright", 
      colors = rev(legend_pal(custom_legend_values)),
      labels = rev(c("Minni", "", "", "", "", "", "Meiri")),
      opacity = 1
    )
  
  
  
  
  
}

