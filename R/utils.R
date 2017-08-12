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
      "New York Times API request failed [%s]\n%s",
      status_code(resp),
      parsed$message
    ))
  }
}

# This function is copied from the httr package see (https://github.com/r-lib/httr/blob/577e2161a16611c714b5bcee96ca44aa5f787068/R/progress.R)

bytes <- function(x, digits = 3, ...) {
  power <- min(floor(log(abs(x), 1000)), 4)
  if (power < 1) {
    unit <- "B"
  } else {
    unit <- c("kB", "MB", "GB", "TB")[[power]]
    x <- x / (1000 ^ power)
  }

  formatted <- format(signif(x, digits = digits), big.mark = ",",
                      scientific = FALSE)

  paste0(formatted, " ", unit)
}
