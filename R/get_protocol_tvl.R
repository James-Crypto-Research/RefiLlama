totalLiquidityUSD <- type <-  NULL


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
#' x <- get_protocol_tvl("WBTC")
get_protocol_tvl <- function(protocol="MakerDAO",category=FALSE){
  end_point <- glue::glue("protocol/",protocol)
  resp <- call_defillama_api(end_point) |> jsonlite::fromJSON()
  the_chains <- resp$chains

  tmp_function <- \(z) resp[["chainTvls"]][[z]][["tvl"]] |>
    tibble::as_tibble() |>
    dplyr::mutate(date=as.Date(as.POSIXct(date,
                                          origin="1970-01-01 00:00:00",
                                          tz="UTC")),
                  {{z}} := totalLiquidityUSD,
                  totalLiquidityUSD = NULL)

  tmp_data <- the_chains |> purrr::map(tmp_function) |>
    plyr::join_all(by="date") |> tibble::as_tibble()
  if (category){
    tmp_data$category <- resp[["category"]]
    tmp_data <- tmp_data |> dplyr::relocate(category,.after=date)
  }
  return(tmp_data)
}
