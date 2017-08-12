token <- rlang::new_environment()
NULL

#' @export
nyt_token <- function() {
  token$token <- rstudioapi::askForPassword("Please enter New York Times API token")
}
