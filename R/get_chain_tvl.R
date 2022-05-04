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
#' x <- get_chain_tvl("Polygon)
get_chain_tvl <- function(chain="Ethereum"){
  end_point <- glue::glue("charts/", chain)
  tmp_name <- glue::glue(chain,"_totaltvl")
  x <- call_defillama_api(end_point)
  x <- jsonlite::fromJSON(x) |>
    tibble::as_tibble() |>
    dplyr::mutate(date = as.Date(as.POSIXct(as.numeric(date),
                                     origin="1970-01-01 00:00:00",
                                     tz="UTC"))) |>
    dplyr::rename( {{tmp_name}} := totalLiquidityUSD)
  return(x)
}

