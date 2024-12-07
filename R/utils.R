#' Call DefiLlamam API
#'
#'  This is the core function connecting to DefiLlama. Since all (most?) API
#'  calls and authentication this is a very straightforward function.
#'  It does some standardization of the output for individual handler functions
#'  to parse
#'
#' @param path The path to pass to the API URL
#'
#' @return a parsed list from the JSON structure
#'
#' @examples
#' \dontrun{
#' x <- call_defillama_api()
#' }
call_defillama_api <- function(path, type = "api") {
  # This section checks that the type argument is in the following list
  # of valid types "api","yield","stablecoins","coins","abi-decoder","bridges"
  valid_types <- c("api", "yield", "stablecoins", "coins", "abi-decoder", "bridges")
  if (!(type %in% valid_types)) {
    msg <- glue::glue("Invalid type argument ({type})", "\n", "Valid types are:", "\n", paste(valid_types, collapse = ", "))
    stop(msg, call. = TRUE)
  }
  req_obj <- httr2::request(paste0("https://", type, ".llama.fi"))
  tryCatch({
    req_obj <- req_obj |> httr2::req_url_path(path) |> httr2::req_perform()
  }, error = function(e) {
    msg <- glue::glue("DefiLlama API request failed ({e$message})", "\n", req_obj$url)
    stop(msg, call. = TRUE)
  })
  parsed <- req_obj |> httr2::resp_body_string()
  return(parsed)
}

#' Convert JSON to tibble from API call
#'
#' @param x a JSON string
#'
#' @return a tibble
#' @export
#'
#' @examples
#' \dontrun{
#'  x <- JSONtoTibble(call_defillama_api("pools", type = "yields"))
#' }
JSONtoTibble <- function(x) {
  x <- jsonlite::fromJSON(x) |>
    tibble::as_tibble() |>
    dplyr::mutate(date = as.Date(as.POSIXct(as.numeric(date),
                                            origin = "1970-01-01 00:00:00",
                                            tz = "UTC"
    )))
  return(x)
}
