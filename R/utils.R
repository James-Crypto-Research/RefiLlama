#' Call DefiLlamam API
#'
#'  This is the core function connecting to DefiLlama. Since all (most?) API
#'  calls and authentication this is a very straightforward function.
#'  It does some standardization of the output for individual handler functions
#'  to parse
#'
#' @param path The path to pass to the API URL
#' @param type The type of API endpoint (api, yields, stablecoins, coins, abi-decoder, bridges)
#' @param query Optional list of query parameters to append to the URL
#'
#' @return a parsed list from the JSON structure
#'
#' @examples
#' \dontrun{
#' x <- CallDefillamaApi("protocols")
#' }
CallDefillamaApi <- function(path, type = "api", query = NULL) {
  # This section checks that the type argument is in the following list
  # of valid types "api","yields","stablecoins","coins","abi-decoder","bridges"
  valid_types <- c("api", "yields", "stablecoins", "coins", "abi-decoder", "bridges")
  if (!(type %in% valid_types)) {
    msg <- glue::glue("Invalid type argument ({type})", "\n", "Valid types are:", "\n", base::paste(valid_types, collapse = ", "))
    base::stop(msg, call. = TRUE)
  }
  req_obj <- httr2::request(base::paste0("https://", type, ".llama.fi"))
  tryCatch({
    req_obj <- req_obj |> httr2::req_url_path(path)
    if (!base::is.null(query)) {
      req_obj <- req_obj |> httr2::req_url_query(!!!query)
    }
    req_obj <- req_obj |> httr2::req_perform()
  }, error = function(e) {
    msg <- glue::glue("DefiLlama API request failed ({e$message})", "\n", req_obj$url)
    base::stop(msg, call. = TRUE)
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
#'  x <- JsonToTibble(CallDefillamaApi("pools", type = "yields"))
#' }
JsonToTibble <- function(x) {
  x <- jsonlite::fromJSON(x) |>
    tibble::as_tibble() |>
    dplyr::mutate(date = base::as.Date(base::as.POSIXct(base::as.numeric(date),
                                            origin = "1970-01-01 00:00:00",
                                            tz = "UTC"
    )))
  return(x)
}
