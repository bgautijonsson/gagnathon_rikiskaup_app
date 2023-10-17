# app/logic/data.R

box::use(
  arrow[copy_files, s3_bucket, read_parquet],
  dplyr[distinct, collect, pull, rename, select, mutate, filter]
)

#' @export
download_data <- function() {
  copy_files(
    s3_bucket("gagnathon"),
    "app/static/data/"
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
