tvl <- name <- chain <- chains <- category <- NULL

#' Grab the list of protocols from DeFiLlama
#'
#' This function gets the current list or protocols from DeFiLlama. It can also
#' returns the number of chains the protocol is on as well as its type. Basic
#' filter by chain and minimum TVL is also supported
#'
#'
#' @param tvl_limit This is the minimum level of Total Value Locked to return
#' @param which_chain This is what chains to focus on. For ones on multiple chains th
#'     value "Multi-Chain" chould be used
#' @param details Should the type and number and list of chains as well as the category
#'   be returned?
#'
#' @return A tibble with date, tvl, chain, number of chains, list of chains, category
#' @export
#'
#' @examples
#' x <- get_protocol_list(tvl_limit=1E9,which_chain="Polygon")
get_protocol_list <- function(tvl_limit = 0, which_chain = NULL, details=FALSE){
  resp <- call_defillama_api("protocols") |> jsonlite::fromJSON()
  tmp <- resp |> dplyr::filter(tvl >= tvl_limit) |> dplyr::select(name,chain, chains,tvl,category) |>
    tibble::as_tibble()
  if (!is.null(which_chain)) {
    output <- tmp |> dplyr::filter(chain == which_chain )
  } else {
    output <- tmp
  }
  if (details) {
    output$n_chains <- purrr::map(output$chains,length) |> unlist()
    output$category <- output$category
  } else {
    output <- output |> dplyr::select(name)
  }
  return(output)
}
