# This scrip tcontains all the functions to get coin info
# from DeFeLlama API




#' Title
#'
#' @param coin
#'
#' @return
#' @export
#'
#' @examples
get_currnt_coin_price <- function(coin = "ethereum") {
  coin_id <- glue::glue("coingecko:{coin}")
  end_point <- glue::glue("prices/current/{coin_id}")
  x <- call_defillama_api(end_point,type="coins")
  x <- jsonlite::fromJSON(x)[[1]][[1]] |>
    tibble::as_tibble() |>
    dplyr::mutate(timestamp = lubridate::as_datetime(timestamp))
  return(x)
}
