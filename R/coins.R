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
GetCurrentCoinPrice <- function(coin = "ethereum") {
  coin_id <- glue::glue("coingecko:{coin}")
  end_point <- glue::glue("coins/prices/current/{coin_id}")
  x <- CallDefillamaApi(end_point, type = "coins")
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
GetEarliestCoinDate <- function(coin = "ethereum") {
  coin_id <- glue::glue("coingecko:{coin}")
  end_point <- glue::glue("coins/prices/first/{coin_id}")
  x <- CallDefillamaApi(end_point, type = "coins")
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
GetCoinPercentage <- function(coin = "ethereum", timestamp = base::Sys.time(), lookForward = FALSE, period = "24h") {
  coin_id <- glue::glue("coingecko:{coin}")
  end_point <- glue::glue("coins/percentage/{coin_id}")
  params <- base::list(timestamp = base::as.numeric(timestamp), lookForward = lookForward, period = period)
  x <- CallDefillamaApi(end_point, type = "coins", query = params)
  x <- jsonlite::fromJSON(x)[[1]][[1]] |>
    tibble::as_tibble()
  return(x)
}

#' Get Historical Coin Prices
#'
#' This function calls the DefiLlama API and returns historical prices for specified coins at a specific timestamp.
#'
#' @param coins A character vector of coin identifiers (e.g., c("ethereum", "bitcoin")).
#' @param timestamp A POSIXct object representing the timestamp for historical prices.
#' @param searchWidth A character string representing the time range to search for price ("4h", "24h").
#'
#' @return A tibble containing historical prices for the specified coins.
#' @export
#'
#' @examples
#' GetHistoricalCoinPrices(c("bitcoin", "ethereum"), as.POSIXct("2024-01-01"))
GetHistoricalCoinPrices <- function(coins, timestamp, searchWidth = NULL) {
  coin_ids <- base::paste0("coingecko:", coins, collapse = ",")
  timestamp_unix <- base::as.numeric(timestamp)
  end_point <- glue::glue("coins/prices/historical/{timestamp_unix}/{coin_ids}")

  params <- base::list()
  if (!base::is.null(searchWidth)) {
    params$searchWidth <- searchWidth
  }

  x <- CallDefillamaApi(end_point, type = "pro-api", query = params)
  x <- jsonlite::fromJSON(x)

  # Convert nested list structure to tibble
  if ("coins" %in% base::names(x)) {
    result <- base::data.frame()
    for (coin_key in base::names(x$coins)) {
      coin_data <- x$coins[[coin_key]]
      coin_data$coin_id <- coin_key
      result <- base::rbind(result, coin_data)
    }
    result <- tibble::as_tibble(result)
    if ("timestamp" %in% base::names(result)) {
      result <- result |> dplyr::mutate(timestamp = lubridate::as_datetime(timestamp))
    }
    return(result)
  }
  return(tibble::tibble())
}

#' Get Coin Price Charts
#'
#' This function calls the DefiLlama API and returns price chart data for specified coins with configurable intervals.
#'
#' @param coins A character vector of coin identifiers (e.g., c("ethereum", "bitcoin")).
#' @param period A character string representing the time period ("1d", "7d", "30d", "90d", "180d", "365d").
#' @param span An integer representing the data point interval in hours.
#' @param searchWidth A character string representing the search width.
#'
#' @return A tibble containing price chart data for the specified coins.
#' @export
#'
#' @examples
#' GetCoinCharts(c("bitcoin", "ethereum"), period = "7d", span = 24)
GetCoinCharts <- function(coins, period = NULL, span = NULL, searchWidth = NULL) {
  coin_ids <- base::paste0("coingecko:", coins, collapse = ",")
  end_point <- glue::glue("coins/chart/{coin_ids}")

  params <- base::list()
  if (!base::is.null(period)) params$period <- period
  if (!base::is.null(span)) params$span <- span
  if (!base::is.null(searchWidth)) params$searchWidth <- searchWidth

  x <- CallDefillamaApi(end_point, type = "pro-api", query = params)
  x <- jsonlite::fromJSON(x)

  # Convert nested list structure to tibble
  if ("coins" %in% base::names(x)) {
    result <- base::data.frame()
    for (coin_key in base::names(x$coins)) {
      coin_data <- x$coins[[coin_key]]
      if ("prices" %in% base::names(coin_data)) {
        prices_df <- base::data.frame(coin_data$prices)
        prices_df$coin_id <- coin_key
        prices_df$symbol <- coin_data$symbol
        prices_df$confidence <- coin_data$confidence
        result <- base::rbind(result, prices_df)
      }
    }
    result <- tibble::as_tibble(result)
    if ("timestamp" %in% base::names(result)) {
      result <- result |> dplyr::mutate(timestamp = lubridate::as_datetime(timestamp))
    }
    return(result)
  }
  return(tibble::tibble())
}

#' Get Block Number at Timestamp
#'
#' This function calls the DefiLlama API and returns the block number for a specific chain at a given timestamp.
#'
#' @param chain A character string representing the blockchain name (e.g., "ethereum", "polygon").
#' @param timestamp A POSIXct object representing the timestamp.
#'
#' @return A tibble containing the block height and timestamp.
#' @export
#'
#' @examples
#' GetBlockAtTimestamp("ethereum", as.POSIXct("2024-01-01"))
GetBlockAtTimestamp <- function(chain, timestamp) {
  timestamp_unix <- base::as.numeric(timestamp)
  end_point <- glue::glue("coins/block/{chain}/{timestamp_unix}")

  x <- CallDefillamaApi(end_point, type = "pro-api")
  x <- jsonlite::fromJSON(x) |>
    tibble::as_tibble()

  if ("timestamp" %in% base::names(x)) {
    x <- x |> dplyr::mutate(timestamp = lubridate::as_datetime(timestamp))
  }

  return(x)
}

#' Get Batch Historical Coin Prices
#'
#' This function calls the DefiLlama API with POST method to get historical prices for multiple coins across multiple timestamps.
#'
#' @param coins_timestamps A named list where names are coin identifiers and values are vectors of timestamps.
#'
#' @return A tibble containing batch historical prices for the specified coins and timestamps.
#' @export
#'
#' @examples
#' \dontrun{
#' coins_data <- list(
#'   "ethereum" = c(as.numeric(as.POSIXct("2024-01-01")), as.numeric(as.POSIXct("2024-01-02"))),
#'   "bitcoin" = c(as.numeric(as.POSIXct("2024-01-01")))
#' )
#' GetBatchHistoricalCoinPrices(coins_data)
#' }
GetBatchHistoricalCoinPrices <- function(coins_timestamps) {
  end_point <- "coins/batchHistorical"

  # Convert coin names to coingecko format
  coins_data <- base::list()
  for (coin_name in base::names(coins_timestamps)) {
    coin_key <- base::paste0("coingecko:", coin_name)
    coins_data[[coin_key]] <- coins_timestamps[[coin_name]]
  }

  request_body <- base::list(coins = coins_data)

  # Use httr2 for POST request since CallDefillamaApi doesn't support POST
  req_obj <- httr2::request("https://pro-api.llama.fi") |>
    httr2::req_url_path(end_point) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(request_body)

  tryCatch({
    resp <- req_obj |> httr2::req_perform()
    x <- resp |> httr2::resp_body_string()
    x <- jsonlite::fromJSON(x)

    # Convert nested list structure to tibble
    if ("coins" %in% base::names(x)) {
      result <- base::data.frame()
      for (coin_key in base::names(x$coins)) {
        coin_data <- x$coins[[coin_key]]
        if ("prices" %in% base::names(coin_data)) {
          prices_df <- base::data.frame(coin_data$prices)
          prices_df$coin_id <- coin_key
          result <- base::rbind(result, prices_df)
        }
      }
      result <- tibble::as_tibble(result)
      if ("timestamp" %in% base::names(result)) {
        result <- result |> dplyr::mutate(timestamp = lubridate::as_datetime(timestamp))
      }
      return(result)
    }
    return(tibble::tibble())
  }, error = function(e) {
    msg <- glue::glue("DefiLlama batch API request failed ({e$message})")
    base::stop(msg, call. = TRUE)
  })
}
