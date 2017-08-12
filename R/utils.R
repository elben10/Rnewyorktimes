token_env <- rlang::new_environment()
NULL

#' @export
nyt_token <- function() {
  token_env$token <- rstudioapi::askForPassword("Please enter New York Times API token")
}

is_token_set <- function() {

  if (is_empty(token_env$token)) {
    abort("A token must either be set using the nyt_token function")
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
