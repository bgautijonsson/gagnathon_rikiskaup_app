# app/logic/map_utils.R

box::use(
  shiny[selectizeInput]
)


#' @export
select_seller <- function(ns) {
  
  food_choices <- c(
    "Múlakaffi ehf.",
    "Krydd og Kavíar ehf.",
    "Pizza-Pizza Ehf.",
    "KFC ehf"
  )
  
  selectizeInput(
    inputId = ns,
    label = "Veitingastaður",
    choices = food_choices, 
    selected = "Múlakaffi ehf.",
    multiple = TRUE,
    options = list(maxItems = 9)
  )
}


#' @export
make_map <- function(data, input) {
  library(arrow)
  library(tidyverse)
  library(sf)
  library(leaflet)
  library(glue)
  library(htmltools)
  hnit <- read_delim(
    "https://raw.githubusercontent.com/annaeva99/gagnathon_stadfong/main/data/stofnanir_rvk_hnit.csv",
    delim = " "
  ) |> 
    # janitor::clean_names() |> 
    select(
      kaupandi = Stofnun,
      n_hnit = N_HNIT_WGS84,
      e_hnit = E_HNIT_WGS84
    ) |> 
    summarise(
      n_hnit = mean(n_hnit),
      e_hnit = mean(e_hnit),
      .by = kaupandi
    )
  
  make_labels <- function(data) {
    data |> 
      mutate(
        label = glue(
          str_c(
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
  plot_dat <- food_data |> 
    filter(
      birgi == "Múlakaffi ehf."
    ) |> 
    summarise(
      kr = sum(kr),
      kr_per_staff = sum(kr_per_staff),
      fjoldi_kaupa = sum(fjoldi_kaupa),
      manudir = as.numeric(max(dags) - min(dags)) / 30,
      .by = kaupandi
    ) |> 
    inner_join(
      hnit,
      by = join_by(kaupandi)
    ) |> 
    st_as_sf(
      coords = c("e_hnit", "n_hnit")
    ) |> 
    st_set_crs("WGS84") |> 
    make_labels()
  
  pal <- colorNumeric(
    "OrRd",
    domain = c(0, max(plot_dat$kr_per_staff)),
    reverse = FALSE
  )
  
  
  
  plot_dat  |> 
    leaflet() |> 
    addProviderTiles(providers$Stamen) |> 
    addCircleMarkers(
      color = ~ pal(kr_per_staff),
      # stroke = FALSE,
      fillOpacity = ~ log(kr_per_staff),
      radius = ~ log(kr_per_staff),
      label = ~ lapply(label, HTML)
    )
}