# These functions grab all the protocol information


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
