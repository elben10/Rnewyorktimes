#' @import rlang
#' @importFrom httr GET http_type http_error status_code content
#' @importFrom purrr modify_if map
NULL

#' Get articles from NYT Archieve
#'
#' The function returns a tibble as default, which contains all New York Times articles of the specified
#' year and month. Articles are available from 1851 and up to the present year and month.
#'
#' @param month the desired month provided as a wholenumber
#' @param year the desired year provided as a wholenumber
#' @param tibble if TRUE the API result is formatted and returned as a tibble, else a complete response
#' is provided
#' @seealso \link[Rnewyorktimes]{nyt_token}
#' @export
#' @examples
#' \dontrun{nyt_archieve(month = 1, year = 1970) #Remember to set token}
nyt_archieve <- function(month = 1, year = 1970, tibble = TRUE ) {
  token <- is_token_set()
  archieve_input_success(month = month, year = year)

  url <- "http://api.nytimes.com/"
  path <- sprintf("svc/archive/v1/%s/%s.json",
                  year,
                  month)

  resp <- GET(url, path = path, query = list(`api-key` = token))

  is_json(resp)

  parsed <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  request_failed(resp, parsed)

  if (tibble) {
    res <- archieve_tibble(parsed$response$docs)
  } else {
    res <- list(content = parsed,
                response = resp)
  }

  attr(res, "tibble") <- tibble
  attr(res, "path") <- path
  attr(res, "date") <- sprintf("%s-%s", year, month)
  attr(res, "size") <- bytes(length(resp$content))
  class(res) <- c("NewYorkTimesAPI", "NewYorkTimesAPI_archieve", class(res))
  res
}

archieve_input_success <- function(month, year) {
  if (!is.numeric(month) | !is.numeric(year)) {
    abort("month and year must be whole numbers provided as integers or double")
  }

  if (!is_whole_number(month) | !is_whole_number(year)) {
    abort("month and year must be whole numbers provided as integers or double")
  }

  if (!(month %in% 1:12)) {
    abort("month can only take values from 1 to 12")
  }

  if (!(year %in% 1851:as.numeric(format(Sys.time(), "%Y")))) {
    abort(
      sprintf("year can only take values from 1851 to %s",
              format(Sys.time(), "%Y"))
    )
  }

  if (year == sys_time(TRUE, "%Y") & month > sys_time(TRUE, "%m")) {
    abort(
      sprintf(
        "month must be equal to or smaller than %s",
        sys_time(TRUE, format = "%m")
      )
    )
  }
}

#' @export
print.NewYorkTimesAPI_archieve <- function(x, ...) {
  cat(sprintf("<New York Times - Archieve>\n  Date: %s\n  Path: %s\n  Size: %s\n",
              attr(x, "date"),
              attr(x, "path"),
              attr(x, "size")))
  if(attr(x, "tibble") == TRUE) {
    res <- x
    class(res) <- c("tbl_df", "tbl", "data.frame")
    print(res)
  } else {
    cat(utils::str(x, max.level = 1, give.attr = FALSE))
  }
}

archieve_tibble <- function(x) {
  col01 = as.character(modify_if(map(x, ~.x[["web_url"]]), is_null, ~NA))
  col02 = as.character(modify_if(map(x, ~.x[["snippet"]]), is_null, ~NA))
  col03 = as.character(modify_if(map(x, ~.x[["lead_paragraph"]]), is_null, ~NA))
  col04 = as.character(modify_if(map(x, ~.x[["print_page"]]), is_null, ~NA))
  col05 = modify_if(map(x, ~.x[["blog"]]), is_null, ~NA)
  col06 = as.character(modify_if(map(x, ~.x[["source"]]), is_null, ~NA))
  col07 = modify_if(map(x, ~.x[["multimedia"]]), is_null, ~NA)
  col08 = modify_if(map(x, ~.x[["headline"]]), is_null, ~NA)
  col09 = modify_if(map(x, ~.x[["keywords"]]), is_null, ~NA)
  col10 = modify_if(map(x, ~.x[["pub_date"]]), is_null, ~NA)
  col11 = modify_if(map(x, ~.x[["document_type"]]), is_null, ~NA)
  col12 = modify_if(map(x, ~.x[["news_desk"]]), is_null, ~NA)
  col13 = modify_if(map(x, ~.x[["section_name"]]), is_null, ~NA)
  col14 = modify_if(map(x, ~.x[["subsection_name"]]), is_null, ~NA)
  col15 = modify_if(map(x, ~.x[["byline"]]), is_null, ~NA)
  col16 = modify_if(map(x, ~.x[["type_of_material"]]), is_null, ~NA)
  col17 = modify_if(map(x, ~.x[["_id"]]), is_null, ~NA)
  col18 = modify_if(map(x, ~.x[["word_count"]]), is_null, ~NA)
  col19 = modify_if(map(x, ~.x[["slideshow_credits"]]), is_null, ~NA)

  tibble::tibble(web_url = col01,
         snippet = col02,
         lead_paragraph = col03,
         print_page = col04,
         blog = col05,
         source = col06,
         multimedia = col07,
         headline = col08,
         keywords = col09,
         pub_data = col10,
         document_type = col11,
         news_desk = col12,
         section_name = col13,
         subsection_name = col14,
         byline = col15,
         type_of_material = col16,
         id = col17,
         word_count = col18,
         slideshow_credits = col19
         )
}

