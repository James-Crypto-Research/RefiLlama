# This script contains all the functions to get coin info
# from DeFeLlama API

#' Get Current Coin Price
#'
#' This function calls the DefiLlama API and returns the current price for the specified coin.
#'
#' @param coin A character string representing the coin. Default is "ethereum".
#'
#' @return A tibble containing the current price and timestamp of the specified coin.
#' @export
#'
#' @examples
#' get_current_coin_price("bitcoin")
get_current_coin_price <- function(coin = "ethereum") {
  coin_id <- glue::glue("coingecko:{coin}")
  end_point <- glue::glue("prices/current/{coin_id}")
  x <- call_defillama_api(end_point,type="coins")
  x <- jsonlite::fromJSON(x)[[1]][[1]] |>
    tibble::as_tibble() |>
    dplyr::mutate(timestamp = lubridate::as_datetime(timestamp))
  return(x)
}

#' Get Earliest Coin Date
#'
#' This function calls the DefiLlama API and returns the earliest available date for the specified coin.
#'
#' @param coin A character string representing the coin. Default is "ethereum".
#'
#' @return A tibble containing the earliest date and timestamp of the specified coin.
#' @export
#'
#' @examples
#' get_earliest_coin_date("bitcoin")
get_earliest_coin_date <- function(coin = "ethereum") {
  coin_id <- glue::glue("coingecko:{coin}")
  end_point <- glue::glue("prices/first/{coin_id}")
  x <- call_defillama_api(end_point, type="coins")
  x <- jsonlite::fromJSON(x)[[1]][[1]] |>
    tibble::as_tibble() |>
    dplyr::mutate(timestamp = lubridate::as_datetime(timestamp))
  return(x)
}

#' Get Coin Percentage Change
#'
#' This function calls the DefiLlama API and returns the percentage change for the specified coin over a given period.
#'
#' @param coin A character string representing the coin. Default is "ethereum".
#' @param timestamp A POSIXct object representing the timestamp. Default is the current system time.
#' @param lookForward A logical value indicating whether to look forward in time. Default is FALSE.
#' @param period A character string representing the period for percentage change. Default is "24h".
#'
#' @return A tibble containing the percentage change of the specified coin.
#' @export
#'
#' @examples
#' get_coin_percentage("bitcoin", Sys.time(), FALSE, "24h")
get_coin_percentage <- function(coin = "ethereum", timestamp = Sys.time(), lookForward = FALSE, period = "24h") {
  coin_id <- glue::glue("coingecko:{coin}")
  end_point <- glue::glue("prices/percentage/{coin_id}")
  params <- list(timestamp = as.numeric(timestamp), lookForward = lookForward, period = period)
  x <- call_defillama_api(end_point, type="coins", query = params)
  x <- jsonlite::fromJSON(x)[[1]][[1]] |>
    tibble::as_tibble()
  return(x)
}
