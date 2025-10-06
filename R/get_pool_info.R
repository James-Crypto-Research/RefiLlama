#' Get the data from all the pools
#'
#' @return a tibble
#' @export
#'
#' @examples
#' x <- GetPoolInfo()
GetPoolInfo <- function() {
  x <- CallDefillamaApi("pools", type = "yields")
  x <- jsonlite::fromJSON(x)
  # Extract the data array from the response
  if ("data" %in% base::names(x)) {
    x <- x$data
  }
  x <- tibble::as_tibble(x)
  return(x)
}
