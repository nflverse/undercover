#' base request function
#'
#' Formats http request with RETRY and customizable logging
#'
#' @param url Full URL to be scraped.
#' @param ... arguments to pass to httr request function
#' @param verb character: a curl verb supported by httr, e.g. GET, POST, PUT, DELETE
#' this is passed to `httr::RETRY()`. Defaults to \code{"GET"}
#' @param retry_delay Minimum time to wait before retrying. Defaults to \code{1} second.
#' @param retry_times Maximum number of attempts. Defaults to \code{3}.
#' @param log_url url for print logging, defaults to `url`
#' @param verbose whether to print out URLs, default = FALSE
#' @param user_agent The user agent to pass on to `httr::user_agent()`
#'
#' @export
.request <- function(url,
                    ...,
                    verb = "GET",
                    retry_delay = 1L,
                    retry_times = 3L,
                    log_url = url,
                    verbose = getOption("undercover.verbose", FALSE),
                    user_agent = NULL){

  stopifnot(
    is.character(url) && length(url) == 1,
    is.numeric(retry_times),
    is.numeric(retry_delay),
    verbose %in% c(TRUE, FALSE)
  )

  if(verbose) cli::cli_alert_info("Retrieving {log_url}")

  resp <- httr::RETRY(
    verb = verb,
    url = url,
    pause_base = retry_delay,
    pause_min = retry_delay,
    times = retry_times,
    httr::user_agent(user_agent),
    ...
  )

  out <- httr::content(resp, as = "text")

  if(httr::http_error(resp)){
    cli::cli_warn("ERROR: {httr::http_status(resp)$message} \n URL:{log_url}")
    out <- character()
  }

  out <- structure(
    out,
    url = url,
    status_code = httr::status_code(resp),
    response = resp,
    class = c("undercover_request", class(out))
  )

  return(out)
}
