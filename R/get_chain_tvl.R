# This function gets the historical TVL for a given chain

get_chain_tvl <- function(chain="Ethereum"){
  end_point <- glue::glue("charts/", chain)
  x <- call_defillama_api(end_point)
  x <- jsonlite::fromJSON(x) |>
    tibble::as_tibble() |>
    mutate(date = as.Date(as.POSIXct(as.numeric(date),
                                     origin="1970-01-01 00:00:00",
                                     tz="UTC")))
  return(x)
}

