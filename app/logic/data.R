# app/logic/data.R

box::use(
  arrow[copy_files, s3_bucket, read_parquet],
  dplyr[distinct, collect, pull, rename, select, mutate, filter, summarise],
  readr[read_delim]
)

#' @export
download_data <- function() {
  copy_files(
    s3_bucket("gagnathon/app_data.parquet"),
    "app/static/data/app_data.parquet/"
  )
  copy_files(
    s3_bucket("gagnathon/food_data.parquet"),
    "app/static/data/food_data.parquet/"
  )
  
  copy_files(
    s3_bucket("gagnathon/tegund_data.parquet"),
    "app/static/data/tegund_data.parquet/"
  )
}

#' @export
or_data <- read_parquet("app/static/data/app_data.parquet/part-0.parquet", as_data_frame = FALSE) |> 
  mutate(
    birgi2 = kaupandi,
    kaupandi2 = birgi
  ) |> 
  select(-birgi, -kaupandi) |> 
  rename(
    birgi = birgi2,
    kaupandi = kaupandi2
  )


#' @export
food_data <- read_parquet("app/static/data/food_data.parquet/part-0.parquet")


#' @export
coord_data <- hnit <- read_delim(
  "https://raw.githubusercontent.com/annaeva99/gagnathon_stadfong/main/data/stofnanir_rvk_hnit.csv",
  delim = " "
) |> 
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


#' @export
tegund_data <- read_parquet("app/static/data/tegund_data.parquet/part-0.parquet")
