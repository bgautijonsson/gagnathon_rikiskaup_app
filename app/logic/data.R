# app/logic/data.R

box::use(
  arrow[copy_files, s3_bucket, read_parquet],
  dplyr[distinct, collect, pull, rename, select, mutate, filter]
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
