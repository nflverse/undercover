#' Scrape with ScrapeOps.io
#'
#' Adds ScrapeOps proxy api service to `.request()` call.
#'
#' @param url Full URL to be scraped
#' @param ... arguments to pass to `.request()`
#' @param api_key ScrapeOps API key, defaults to env var SCRAPEOPS_API_KEY
#'
#' @inheritDotParams .request
#'
#' @export
scrapeops_request <- function(url,
                              ...,
                              api_key = Sys.getenv("SCRAPEOPS_API_KEY"),
                              user_agent = NULL){

  stopifnot(
    !is.character(api_key) && nchar(api_key) > 0,
    !is.character(url) && length(url) == 1
  )

  access_url <- httr::modify_url(
    url = "https://proxy.scrapeops.io/v1/",
    query = list(
      api_key = api_key,
      url = url
    )
  )

  redacted_url <- httr::modify_url(
    "https://proxy.scrapeops.io/v1/",
    query = list(
      api_key = "scrapeops_api_key",
      url = url
    )
  )

  resp <- .request(url = access_url, ..., log_url = url, user_agent = user_agent)

  resp <- .scrapeops_redact(resp, redacted_url)

  return(resp)
}

.scrapeops_redact <- function(resp, redacted_url){
  attr(resp, "url") <- redacted_url
  attr(resp, "response")$url <- redacted_url
  attr(resp, "response")$request$url <- redacted_url
  attr(resp, "undercover_provider") <- "scrapeops"
  return(resp)
}


#' Query Fake User Agent from ScrapeOps.io
#'
#' @param api_key ScrapeOps API key, defaults to env var `SCRAPEOPS_API_KEY`
#'
#' @return ScrapeOps API returns 10 fake user agents. The function choses one
#'   of them randomly and returns it.
#' @export
scrapeops_fake_useragent <- function(api_key = Sys.getenv("SCRAPEOPS_API_KEY")){
  resp <- httr::GET(
    url = "http://headers.scrapeops.io/v1/user-agents",
    query = list(api_key = api_key)
  )

  httr::content(resp, as = "parsed") |>
    unlist(use.names = FALSE) |>
    sample(1)
}
