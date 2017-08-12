token_env <- rlang::new_environment()
NULL

#' @export
nyt_token <- function() {
  token_env$token <- rstudioapi::askForPassword("Please enter New York Times API token")
}

is_token_set <- function(token) {

  if (!is_missing(token)) {
    if (!is_character(token)) {
      abort("token must be a string")
    }
  }

  if (is_empty(token_env$token) & is_missing(token)) {
    abort("A token must either be set as the token argument or by the nyt_token function")
  }
}

is_whole_number <- function(x) {
  abs(x - round(x)) < .Machine$double.eps
}
