# This file contains all the API functions around TVL for DefiLllama
# Creation Date: 2023-12-12

totalLiquidityUSD <- type <- NULL
tvl <- name <- chain <- chains <- category <- NULL



#' Grab the list of protocols from DeFiLlama
#'
#' This function gets the current list or protocols from DeFiLlama. It can also
#' returns the number of chains the protocol is on as well as its type. Basic
#' filter by chain and minimum TVL is also supported
#'
#'
#' @param tvl_limit This is the minimum level of Total Value Locked to return
#' @param which_chain Optional parameter for what chain to focus on.
#' @param details Should the type and number and list of chains as well as the category
#'   be returned?
#'
#' @return A tibble with date, tvl, chain, number of chains, list of chains, category
#' @export
#'
#' @examples
#' x <- get_list_protocol(tvl_limit=1E9,which_chain="Polygon",details=TRUE)
get_list_protocol <- function(tvl_limit = 0, which_chain = NULL, details=FALSE){
  resp <- call_defillama_api("protocols") |> jsonlite::fromJSON()
  tmp <- resp |>
    dplyr::filter(tvl >= tvl_limit) |>
    dplyr::select(name, chain, chains, tvl, category) |>
    tibble::as_tibble()
  if (!is.null(which_chain)) {
    output <- tmp |> dplyr::filter(which_chain %in% chain)
  } else {
    output <- tmp
  }
  if (details) {
    output$n_chains <- purrr::map(output$chains, length) |> unlist()
    output$category <- output$category
  } else {
    output <- output |> dplyr::select(name)
  }
  return(output)
}

#' Get a protocol's historic TVL per chain and category
#'
#' This function returns a wide tibble with the historic TVL per
#' chain of a protocol. It can also return the category of the protocol.
#'
#' @param protocol a string for protocol to return
#' @param category a logical whether to return type as a column
#'
#' @return a wide tibble of date, category and one column per chain
#' @export
#' @importFrom rlang :=
#'
#' @examples
#' x <- get_tvl_historical_protocol("uniswap",category=TRUE)
get_tvl_historical_protocol <- function(protocol="MakerDAO",category=FALSE){
  url_protocol <- stringr::str_replace_all(protocol," ","-")
  end_point <- glue::glue("protocol/",url_protocol)
  resp <- call_defillama_api(end_point) |> jsonlite::fromJSON()
  the_chains <- resp$chains

  tmp_function <- \(z) resp[["chainTvls"]][[z]][["tvl"]] |>
    tibble::as_tibble() |>
    dplyr::mutate(
      date = as.Date(as.POSIXct(date,
        origin = "1970-01-01 00:00:00",
        tz = "UTC"
      )),
      {{ z }} := totalLiquidityUSD,
      totalLiquidityUSD = NULL
    )

  tmp_data <- the_chains |>
    purrr::map(tmp_function) |>
    plyr::join_all(by = "date") |>
    tibble::as_tibble()
  if (category) {
    tmp_data$category <- resp[["category"]]
    tmp_data <- tmp_data |> dplyr::relocate(category, .after = date)
  }
  return(tmp_data)
}



#' Get the historical TVL for all protocols denominated in USD
#'
#' This function returns the sum of all TVLs for all dates available
#'
#' @return a tibble with the date and value
#' @export
#'
#' @examples
#' x <- get_tvl_historical_all()
get_tvl_historical_all <- function(){
  x <- call_defillama_api("v2/historicalChainTvl/")
  x <- jsonlite::fromJSON(x) |>
    tibble::as_tibble() |>
    dplyr::mutate(date = as.Date(as.POSIXct(as.numeric(date),
                                            origin="1970-01-01 00:00:00",
                                            tz="UTC")))
  return(x)
}



# This function gets the historical TVL for a given chain

#' Return the historical TVL per chain
#'
#' @param chain a string containing the chain to return
#'
#' @return a tibble with the date and the TVL in USD
#' @export
#' @importFrom rlang :=
#'
#' @examples
#' x <- get_tvl_historical_chain("Polygon)
get_tvl_historical_chain <- function(chain="Ethereum"){
  end_point <- glue::glue("v2/historicalChainTvl/", chain)
  tmp_name <- glue::glue(chain,"_totaltvl")
  x <- call_defillama_api(end_point)
  x <- JSONtoTibble(x)|>
    dplyr::rename({{ tmp_name }} := totalLiquidityUSD)
  return(x)
}



#' Get all current TVL of a chain
#'
#' @return
#' @export
#'
#' @examples
get_current_tvl <- function(chain = "Ethereum") {
  end_point <- glue::glue("tvls/", chain)
  tmp_name <- glue::glue(chain, "_totaltvl")
  x <- call_defillama_api(end_point)
  x <- JSONtoTibble(x)|>
    dplyr::rename({{ tmp_name }} := totalLiquidityUSD)
  return(x)
}



#' Get all current TVLs
#'
#' @return
#' @export
#'
#' @examples
#' x <- get_tvl_current_all()
get_tvl_current_all <- function(){
  x <- call_defillama_api("v2/chains")
  x <- jsonlite::fromJSON(x) |>
    tibble::as_tibble() |>
    dplyr::select(-gecko_id)
  return(x)
}


#' Get a specific TVL for a protocol
#'
#' @return
#' @export
#'
#' @examples
#' x <- get_tvl_current_protocol("Uniswap")
get_tvl_current_protocol <- function(protocol="Uniswap"){
  end_point <- glue::glue("tvl/", protocol)
  x <- call_defillama_api(end_point)
  x <- jsonlite::fromJSON(x) |>
    tibble::as_tibble()
  return(x)
}







