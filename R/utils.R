token_env <- rlang::new_environment()
NULL

#' @export
nyt_token <- function() {
  token_env$token <- rstudioapi::askForPassword("Please enter New York Times API token")
}

is_token_set <- function() {

  if (is_empty(token_env$token)) {
    abort("A token must be set using the nyt_token function")
  }
  token_env$token
}

is_whole_number <- function(x) {
  abs(x - round(x)) < .Machine$double.eps
}

sys_time <- function(numeric = FALSE, format = "%Y-%m-%d %H:%M:%S %Z") {
  res <- format(Sys.time(), format)
  if (numeric) as.numeric(res) else res
}

is_json <- function(resp) {
  if (http_type(resp) != "application/json") {
    abort(sprintf("API did not return application/json, but %s",
                  http_type(resp)))
  }
}

request_failed <- function(resp, parsed) {
  if (http_error(resp)) {
    abort(sprintf(
      "New York Times API request failed [%s]\n%s\n<%s>",
      status_code(resp),
      parsed$message,
      parsed$documentation_url
    ))
  }
}
