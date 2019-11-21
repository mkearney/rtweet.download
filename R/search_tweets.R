#' Search tweets download
#'
#' Automate the data collection for large Twitter searches (via
#' \code{\link[rtweet]{search_tweets}})
#'
#' @param q Search query to be used to find matching tweets from the past 6-9
#'   days. See \code{\link[rtweet]{search_tweets}}) for more information on
#'   Twitter search query syntax.
#' @param n Number of desired tweets to return. See details for more information
#'   about relevant rate limits.
#' @param ... Other parameters are passed to
#'   \code{\link[rtweet]{search_tweets}}).
#' @param .stw Optionally supply a preexisting output vector (like that returned
#'   by this function)–if NULL, the default, this function will start fresh.
#' @return Returns a list data frames of search data
#' @family .stw
#' @details This function attempts to search and collect data for up to 18,000
#'   (when using the default rtweet authorization token) or 63,000 (when using
#'   token generated from your own Twitter app with sufficient bearer token-
#'   level permissions) statuses every 15 minutes, sleeping between calls unti
#'   Twitter's API rate limit resets.
#' @export
search_tweets_download <- function(q, n, ..., .stw = NULL) {
  total <- n
  token <- prep_tokens(rtweet::get_token())
  .stw  <- prep_stwout(.stw, n, token)
  first <- TRUE
  while (total > 0) {
    token <- determine_token(token, "search_tweets")
    count <- determine_count(token, "search_tweets", first = first)
    .stw[[length(.stw) + 1L]] <- search_tweets_warning_nap(q,
      n = count, ..., token = token$token)
    total <- total - count
    nrows_collected <- sum(dapr::vap_int(.stw, NROW))
    spf <- repc("1", nchar(use_commas_int(n)))
    complete("Collected data for ", cint(nrows_collected, spf),
      " tweets (", cdbl((n - total) / n * 100, "11.1"), "%)")
    first <- FALSE
  }
  .stw
}

prep_stwout <- function(.stw = NULL, n, token) {
  num <- 18000
  if ("bearer" %in% names(token)) {
    num <- c(num, 45000)
  }
  len <- ceiling(n / sum(num)) + 2
  if (!is_null(.stw)) {
    if (length(.stw) < len) {
      .stw <- c(.stw, vector("list", len - length(.stw)))
    }
    return(.stw)
  }
  vector("list", len)
}
