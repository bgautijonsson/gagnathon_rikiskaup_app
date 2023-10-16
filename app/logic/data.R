# app/logic/data.R

box::use(
  arrow[copy_files, s3_bucket, read_parquet],
  dplyr[collect],
  dplyr[distinct, collect, pull]
)

#' @export
download_data <- function() {
  copy_files(
    s3_bucket("gagnathon"),
    "app/static/data/"
  )
}

#' @export
or_data <- read_parquet("app/static/data/opnirreikningar.parquet/part-0.parquet", as_data_frame = FALSE)
