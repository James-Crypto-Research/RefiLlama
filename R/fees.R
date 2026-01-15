# This file contains all the API functions for fees and revenue data from DeFiLlama
# Creation Date: 2024-10-29

fees24h <- revenue24h <- change_1d <- change_7d <- change_30d <- NULL

#' Get Fees Overview
#'
#' This function returns aggregated protocol fees and revenue data.
#'
#' @param excludeTotalDataChart A logical value indicating whether to exclude chart data. Default is FALSE.
#' @param excludeTotalDataChartBreakdown A logical value indicating whether to exclude breakdown data. Default is FALSE.
#' @param dataType A character string specifying data type ("dailyFees", "dailyRevenue", or "dailyHoldersRevenue"). Default is "dailyFees".
#'
#' @return A list containing fees overview data.
#' @export
#'
#' @examples
#' GetFeesOverview()
GetFeesOverview <- function(excludeTotalDataChart = FALSE, excludeTotalDataChartBreakdown = FALSE, dataType = "dailyFees") {
  end_point <- "api/overview/fees"

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
        totalFees24h = x$totalFees24h,
        totalRevenue24h = x$totalRevenue24h,
        change_1d = x$change_1d
      ),
      protocols = protocols_df,
      chart = chart_df
    ))
  }

  return(tibble::tibble())
}

#' Get Fees by Chain
#'
#' This function returns fees and revenue data for a specific blockchain.
#'
#' @param chain A character string representing the blockchain name (e.g., "Ethereum", "Arbitrum").
#' @param dataType A character string specifying data type ("dailyFees", "dailyRevenue", or "dailyHoldersRevenue"). Default is "dailyFees".
#'
#' @return A list containing fees data for the specified chain.
#' @export
#'
#' @examples
#' GetFeesByChain("Ethereum")
GetFeesByChain <- function(chain, dataType = "dailyFees") {
  end_point <- glue::glue("api/overview/fees/{chain}")

  params <- base::list()
  if (!base::is.null(dataType)) params$dataType <- dataType

  x <- CallDefillamaApi(end_point, type = "pro-api", query = params)
  x <- jsonlite::fromJSON(x)

  # Convert protocols list to tibble if it exists
  if ("protocols" %in% base::names(x)) {
    protocols_df <- tibble::as_tibble(x$protocols)

    # Return combined data
    return(base::list(
      summary = base::list(
        totalFees24h = x$totalFees24h,
        totalRevenue24h = x$totalRevenue24h,
        change_1d = x$change_1d
      ),
      protocols = protocols_df
    ))
  }

  return(tibble::tibble())
}

#' Get Protocol Fees Summary
#'
#' This function returns fees and revenue data for a specific protocol.
#'
#' @param protocol A character string representing the protocol slug (e.g., "uniswap", "aave").
#' @param dataType A character string specifying data type ("dailyFees", "dailyRevenue", or "dailyHoldersRevenue"). Default is "dailyFees".
#'
#' @return A list containing fees data for the specified protocol.
#' @export
#'
#' @examples
#' GetProtocolFeesSummary("uniswap")
GetProtocolFeesSummary <- function(protocol, dataType = "dailyFees") {
  end_point <- glue::glue("api/summary/fees/{protocol}")

  params <- base::list()
  if (!base::is.null(dataType)) params$dataType <- dataType

  x <- CallDefillamaApi(end_point, type = "pro-api", query = params)
  x <- jsonlite::fromJSON(x)

  # Convert chart data to tibble if it exists
  if ("totalDataChart" %in% base::names(x)) {
    chart_df <- base::data.frame()
    for (i in base::seq_along(x$totalDataChart)) {
      chart_item <- x$totalDataChart[[i]]
      if (base::length(chart_item) >= 2) {
        chart_df <- base::rbind(chart_df, base::data.frame(
          date = chart_item[1],
          value = chart_item[2]
        ))
      }
    }
    if (base::nrow(chart_df) > 0) {
      chart_df <- tibble::as_tibble(chart_df) |>
        dplyr::mutate(date = base::as.Date(base::as.POSIXct(date, origin = "1970-01-01 00:00:00", tz = "UTC")))
      x$totalDataChart <- chart_df
    }
  }

  # Convert breakdown chart data if it exists
  if ("totalDataChartBreakdown" %in% base::names(x)) {
    breakdown_df <- base::data.frame()
    for (i in base::seq_along(x$totalDataChartBreakdown)) {
      breakdown_item <- x$totalDataChartBreakdown[[i]]
      if (base::length(breakdown_item) >= 2) {
        date_val <- breakdown_item[1]
        breakdown_val <- breakdown_item[2]

        # Handle nested breakdown structure
        if (base::is.list(breakdown_val)) {
          for (chain_name in base::names(breakdown_val)) {
            chain_data <- breakdown_val[[chain_name]]
            if (base::is.list(chain_data)) {
              for (protocol_name in base::names(chain_data)) {
                breakdown_df <- base::rbind(breakdown_df, base::data.frame(
                  date = date_val,
                  chain = chain_name,
                  protocol = protocol_name,
                  value = chain_data[[protocol_name]]
                ))
              }
            }
          }
        }
      }
    }
    if (base::nrow(breakdown_df) > 0) {
      breakdown_df <- tibble::as_tibble(breakdown_df) |>
        dplyr::mutate(date = base::as.Date(base::as.POSIXct(date, origin = "1970-01-01 00:00:00", tz = "UTC")))
      x$totalDataChartBreakdown <- breakdown_df
    }
  }

  # Convert to summary tibble
  summary_data <- tibble::tibble(
    id = x$id,
    name = x$name,
    displayName = x$displayName,
    total24h = x$total24h,
    total48hto24h = x$total48hto24h,
    total7d = x$total7d,
    totalAllTime = x$totalAllTime,
    change_1d = x$change_1d
  )

  # Return combined data
  return(base::list(
    summary = summary_data,
    chains = x$chains,
    totalDataChart = x$totalDataChart,
    totalDataChartBreakdown = x$totalDataChartBreakdown,
    metadata = base::list(
      url = x$url,
      description = x$description,
      logo = x$logo,
      gecko_id = x$gecko_id,
      twitter = x$twitter,
      symbol = x$symbol
    )
  ))
}