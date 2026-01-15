# This file contains all the API functions for volume data from DeFiLlama
# Creation Date: 2024-10-29

volume24h <- change_1d <- change_7d <- change_30d <- NULL

#' Get DEX Volume Overview
#'
#' This function returns aggregated DEX volume data across all protocols.
#'
#' @param excludeTotalDataChart A logical value indicating whether to exclude chart data. Default is FALSE.
#' @param excludeTotalDataChartBreakdown A logical value indicating whether to exclude breakdown data. Default is FALSE.
#' @param dataType A character string specifying data type ("dailyVolume" or "totalVolume"). Default is "dailyVolume".
#'
#' @return A tibble containing DEX volume overview data.
#' @export
#'
#' @examples
#' GetDexVolumeOverview()
GetDexVolumeOverview <- function(excludeTotalDataChart = FALSE, excludeTotalDataChartBreakdown = FALSE, dataType = "dailyVolume") {
  end_point <- "api/overview/dexs"

  params <- base::list()
  if (excludeTotalDataChart) params$excludeTotalDataChart <- "true"
  if (excludeTotalDataChartBreakdown) params$excludeTotalDataChartBreakdown <- "true"
  if (!base::is.null(dataType)) params$dataType <- dataType

  x <- CallDefillamaApi(end_point, type = "pro-api", query = params)
  x <- jsonlite::fromJSON(x)

  # Convert protocols list to tibble if it exists
  if ("protocols" %in% base::names(x)) {
    protocols_df <- tibble::as_tibble(x$protocols)

    # Handle chart data if it exists
    chart_df <- NULL
    if ("chart" %in% base::names(x) && !excludeTotalDataChart) {
      chart_df <- tibble::as_tibble(x$chart)
      if ("date" %in% base::names(chart_df)) {
        chart_df <- chart_df |>
          dplyr::mutate(date = base::as.Date(base::as.POSIXct(date, origin = "1970-01-01 00:00:00", tz = "UTC")))
      }
    }

    # Return combined data
    return(base::list(
      summary = base::list(
        totalVolume = x$totalVolume,
        change_1d = x$change_1d,
        change_7d = x$change_7d,
        change_30d = x$change_30d
      ),
      protocols = protocols_df,
      chart = chart_df
    ))
  }

  return(tibble::tibble())
}

#' Get DEX Volume for Specific Chain
#'
#' This function returns DEX volume data for a specific blockchain.
#'
#' @param chain A character string representing the blockchain name (e.g., "Ethereum", "Arbitrum").
#' @param excludeTotalDataChart A logical value indicating whether to exclude chart data. Default is FALSE.
#' @param excludeTotalDataChartBreakdown A logical value indicating whether to exclude breakdown data. Default is FALSE.
#'
#' @return A tibble containing DEX volume data for the specified chain.
#' @export
#'
#' @examples
#' GetDexVolumeByChain("Ethereum")
GetDexVolumeByChain <- function(chain, excludeTotalDataChart = FALSE, excludeTotalDataChartBreakdown = FALSE) {
  end_point <- glue::glue("api/overview/dexs/{chain}")

  params <- base::list()
  if (excludeTotalDataChart) params$excludeTotalDataChart <- "true"
  if (excludeTotalDataChartBreakdown) params$excludeTotalDataChartBreakdown <- "true"

  x <- CallDefillamaApi(end_point, type = "pro-api", query = params)
  x <- jsonlite::fromJSON(x)

  # Convert protocols list to tibble if it exists
  if ("protocols" %in% base::names(x)) {
    protocols_df <- tibble::as_tibble(x$protocols)

    # Handle chart data if it exists
    chart_df <- NULL
    if ("chart" %in% base::names(x) && !excludeTotalDataChart) {
      chart_df <- tibble::as_tibble(x$chart)
      if ("date" %in% base::names(chart_df)) {
        chart_df <- chart_df |>
          dplyr::mutate(date = base::as.Date(base::as.POSIXct(date, origin = "1970-01-01 00:00:00", tz = "UTC")))
      }
    }

    # Return combined data
    return(base::list(
      summary = base::list(
        totalVolume = x$totalVolume,
        change_1d = x$change_1d,
        change_7d = x$change_7d,
        change_30d = x$change_30d
      ),
      protocols = protocols_df,
      chart = chart_df
    ))
  }

  return(tibble::tibble())
}

#' Get DEX Protocol Summary
#'
#' This function returns volume data for a specific DEX protocol.
#'
#' @param protocol A character string representing the protocol slug (e.g., "uniswap", "curve").
#' @param dataType A character string specifying data type ("dailyVolume" or "totalVolume"). Default is "dailyVolume".
#'
#' @return A tibble containing volume data for the specified DEX protocol.
#' @export
#'
#' @examples
#' GetDexProtocolSummary("uniswap")
GetDexProtocolSummary <- function(protocol, dataType = "dailyVolume") {
  end_point <- glue::glue("api/summary/dexs/{protocol}")

  params <- base::list()
  if (!base::is.null(dataType)) params$dataType <- dataType

  x <- CallDefillamaApi(end_point, type = "pro-api", query = params)
  x <- jsonlite::fromJSON(x)

  # Convert daily volume data to tibble if it exists
  if ("dailyVolume" %in% base::names(x)) {
    daily_volume_df <- tibble::as_tibble(x$dailyVolume)
    if ("date" %in% base::names(daily_volume_df)) {
      daily_volume_df <- daily_volume_df |>
        dplyr::mutate(date = base::as.Date(base::as.POSIXct(date, origin = "1970-01-01 00:00:00", tz = "UTC")))
    }
    x$dailyVolume <- daily_volume_df
  }

  # Convert to tibble for single row data
  summary_data <- tibble::tibble(
    id = x$id,
    name = x$name,
    displayName = x$displayName,
    volume24h = x$volume24h,
    volume7d = x$volume7d,
    volume30d = x$volume30d,
    totalVolume = x$totalVolume,
    change_1d = x$change_1d,
    change_7d = x$change_7d
  )

  # Return combined data
  return(base::list(
    summary = summary_data,
    chains = x$chains,
    chainBreakdown = x$chainBreakdown,
    dailyVolume = x$dailyVolume
  ))
}

#' Get Options Volume Overview
#'
#' This function returns aggregated options trading volume data.
#'
#' @param excludeTotalDataChart A logical value indicating whether to exclude chart data. Default is FALSE.
#' @param excludeTotalDataChartBreakdown A logical value indicating whether to exclude breakdown data. Default is FALSE.
#'
#' @return A tibble containing options volume overview data.
#' @export
#'
#' @examples
#' GetOptionsVolumeOverview()
GetOptionsVolumeOverview <- function(excludeTotalDataChart = FALSE, excludeTotalDataChartBreakdown = FALSE) {
  end_point <- "api/overview/options"

  params <- base::list()
  if (excludeTotalDataChart) params$excludeTotalDataChart <- "true"
  if (excludeTotalDataChartBreakdown) params$excludeTotalDataChartBreakdown <- "true"

  x <- CallDefillamaApi(end_point, type = "pro-api", query = params)
  x <- jsonlite::fromJSON(x)

  # Convert protocols list to tibble if it exists
  if ("protocols" %in% base::names(x)) {
    protocols_df <- tibble::as_tibble(x$protocols)

    # Return combined data
    return(base::list(
      summary = base::list(
        totalPremiumVolume = x$totalPremiumVolume,
        totalNotionalVolume = x$totalNotionalVolume
      ),
      protocols = protocols_df
    ))
  }

  return(tibble::tibble())
}

#' Get Options Volume by Chain
#'
#' This function returns options trading volume data for a specific blockchain.
#'
#' @param chain A character string representing the blockchain name (e.g., "Ethereum", "Arbitrum").
#'
#' @return A tibble containing options volume data for the specified chain.
#' @export
#'
#' @examples
#' GetOptionsVolumeByChain("Ethereum")
GetOptionsVolumeByChain <- function(chain) {
  end_point <- glue::glue("api/overview/options/{chain}")

  x <- CallDefillamaApi(end_point, type = "pro-api")
  x <- jsonlite::fromJSON(x)

  # Convert protocols list to tibble if it exists
  if ("protocols" %in% base::names(x)) {
    protocols_df <- tibble::as_tibble(x$protocols)

    # Return combined data
    return(base::list(
      summary = base::list(
        totalPremiumVolume = x$totalPremiumVolume,
        totalNotionalVolume = x$totalNotionalVolume
      ),
      protocols = protocols_df
    ))
  }

  return(tibble::tibble())
}

#' Get Options Protocol Summary
#'
#' This function returns options trading data for a specific protocol.
#'
#' @param protocol A character string representing the protocol slug (e.g., "lyra", "opyn").
#'
#' @return A tibble containing options data for the specified protocol.
#' @export
#'
#' @examples
#' GetOptionsProtocolSummary("lyra")
GetOptionsProtocolSummary <- function(protocol) {
  end_point <- glue::glue("api/summary/options/{protocol}")

  x <- CallDefillamaApi(end_point, type = "pro-api")
  x <- jsonlite::fromJSON(x)

  # Convert daily volume data to tibbles if they exist
  if ("dailyPremiumVolume" %in% base::names(x)) {
    daily_premium_df <- tibble::as_tibble(x$dailyPremiumVolume)
    if ("date" %in% base::names(daily_premium_df)) {
      daily_premium_df <- daily_premium_df |>
        dplyr::mutate(date = base::as.Date(base::as.POSIXct(date, origin = "1970-01-01 00:00:00", tz = "UTC")))
    }
    x$dailyPremiumVolume <- daily_premium_df
  }

  if ("dailyNotionalVolume" %in% base::names(x)) {
    daily_notional_df <- tibble::as_tibble(x$dailyNotionalVolume)
    if ("date" %in% base::names(daily_notional_df)) {
      daily_notional_df <- daily_notional_df |>
        dplyr::mutate(date = base::as.Date(base::as.POSIXct(date, origin = "1970-01-01 00:00:00", tz = "UTC")))
    }
    x$dailyNotionalVolume <- daily_notional_df
  }

  # Convert to summary tibble
  summary_data <- tibble::tibble(
    name = x$name,
    premiumVolume24h = x$premiumVolume24h,
    notionalVolume24h = x$notionalVolume24h,
    totalPremiumVolume = x$totalPremiumVolume,
    totalNotionalVolume = x$totalNotionalVolume
  )

  # Return combined data
  return(base::list(
    summary = summary_data,
    chains = x$chains,
    dailyPremiumVolume = x$dailyPremiumVolume,
    dailyNotionalVolume = x$dailyNotionalVolume
  ))
}