#' Get all current TVLs
#'
#' @return
#' @export
#'
#' @examples
get_current_tvl <- function(){
  x <- call_defillama_api("chains")
  x <- jsonlite::fromJSON(x) |>
    tibble::as_tibble() |>
    dplyr::select(-gecko_id)
  return(x)
}
