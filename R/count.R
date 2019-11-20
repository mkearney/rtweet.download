determine_count <- function(token, query, first = FALSE) {
  if (is_bearer(token[["token"]])) {
    n <- switch(query,
      'search_tweets' = 45000,
      'lookup_users' = 30000,
      'get_friends' = 15
    )
  } else {
    n <- switch(query,
      'search_tweets' = 18000,
      'lookup_users' = 90000,
      'get_friends' = 15
    )
  }
  if (!first) {
    return(n)
  }
  rl <- rate_limit2(token = token[["token"]], query = query)
  if (rl[["remaining"]] == 0) {
    return(n)
  }
  rl[["remaining"]] * (n / rl[["limit"]])
}
