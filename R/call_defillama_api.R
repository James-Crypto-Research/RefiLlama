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
call_defillama_api <- function(path) {
  tmp_url <- httr::modify_url("https://api.llama.fi/",path = path)
  resp <- httr::GET(url = tmp_url)
  if (httr::http_error(resp)) {
    msg <- glue::glue("DefiLlama API request failed ({httr::status_code(resp)})","\n", url)
    stop(msg, call. = TRUE)
  }
  parsed <- httr::content(resp, as="text", encoding="UTF-8")
  return(parsed)
}
