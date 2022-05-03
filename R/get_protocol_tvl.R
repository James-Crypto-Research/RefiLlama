# This function grabs the TVL for a given protocol

get_protocol_tvl <- function(protocol="MakerDAO",type=FALSE){
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
  if (type){
    tmp_data$type <- resp[["category"]]
    tmp_data <- tmp_data |> dplyr::relocate(type,.after=date)
  }
  return(tmp_data)
}
