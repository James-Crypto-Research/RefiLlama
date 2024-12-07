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
get_current_coin_price <- function(coin = "ethereum") {
  coin_id <- glue::glue("coingecko:{coin}")
  end_point <- glue::glue("prices/current/{coin_id}")
  x <- call_defillama_api(end_point,type="coins")
  x <- jsonlite::fromJSON(x)[[1]][[1]] |>
    tibble::as_tibble() |>
    dplyr::mutate(timestamp = lubridate::as_datetime(timestamp))
  return(x)
}

get_earliest_coin_date <- function(coin = "ethereum") {
  coin_id <- glue::glue("coingecko:{coin}")
  end_point <- glue::glue("prices/first/{coin_id}")
  x <- call_defillama_api(end_point, type="coins")
  x <- jsonlite::fromJSON(x)[[1]][[1]] |>
    tibble::as_tibble() |>
    dplyr::mutate(timestamp = lubridate::as_datetime(timestamp))
  return(x)
}

get_coin_percentage <- function(coin = "ethereum", timestamp = Sys.time(), lookForward = FALSE, period = "24h") {
  coin_id <- glue::glue("coingecko:{coin}")
  end_point <- glue::glue("prices/percentage/{coin_id}")
  params <- list(timestamp = as.numeric(timestamp), lookForward = lookForward, period = period)
  x <- call_defillama_api(end_point, type="coins", query = params)
  x <- jsonlite::fromJSON(x)[[1]][[1]] |>
    tibble::as_tibble()
  return(x)
}