#' Get the data from all the pools
#'
#' @return a tibble
#' @export
#'
#' @examples
#' x <- get_pool_info()
GetPoolInfo <- function() {
  x <- CallDefillamaApi("pools", type = "yields")
  x <- jsonlite::fromJSON(x) |>
    tibble::as_tibble() |>
    dplyr::mutate(date = base::as.Date(base::as.POSIXct(base::as.numeric(date),
      origin = "1970-01-01 00:00:00",
      tz = "UTC"
    )))
  return(x)
}
