#' Get the data from all the pools
#'
#' @return a tibble
#' @export
#'
#' @examples
#' x <- get_pool_info()
get_pool_info <- function() {
  x <- call_defillama_api("pools", type = "yields")
  x <- jsonlite::fromJSON(x) |>
    tibble::as_tibble() |>
    dplyr::mutate(date = as.Date(as.POSIXct(as.numeric(date),
      origin = "1970-01-01 00:00:00",
      tz = "UTC"
    )))
  return(x)
}
