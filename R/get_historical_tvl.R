# This function grabs the historical TVL on all chains


#' Get the historical TVL for all protocols denominated in USD
#'
#' @return a tibble with the date and value
#' @export
#'
#' @examples
#' x <- get_historical_tvl()
get_historical_tvl <- function(){
  x <- call_defillama_api("charts")
  x <- jsonlite::fromJSON(x) |>
    tibble::as_tibble() |>
    dplyr::mutate(date = as.Date(as.POSIXct(as.numeric(date),
                                     origin="1970-01-01 00:00:00",
                                     tz="UTC")))
  return(x)
}
