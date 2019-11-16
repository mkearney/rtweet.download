lookup_tweets_download <- function(x, ..., tokens = NULL) {
  token <- prep_tokens(token)
  outpt <- vector("list")
  first <- TRUE
  while (length(x) > 0) {
    token <- determine_token(token, "lookup_users")
    count <- determine_count(token, "lookup_users", first = first)
    outpt[[length(outpt) + 1L]] <- lookup_users_warning_nap(
      x[seq_len(count)], ..., token = token$token)
    first <- FALSE
  }
  outpt
}


prep_twtout <- function(.twt = NULL, n, token) {
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
