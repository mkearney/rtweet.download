run_it_back <- function(.f, .times = 3L) {
  eval(parse(text = paste0('function(...) {
    .i <- 0L
    while (
      is.null(x <- tryCatch(', .f, '(...), error = function(e) NULL)) &&
        .i <= .times
    ) {
      .i <- .i + 1L
      Sys.sleep(0.5)
    }
    x
  }')))
}

get_timeline2  <- run_it_back("rtweet::get_timeline")

rate_limit2    <- run_it_back("rtweet::rate_limit")

lookup_users2  <- run_it_back("rtweet::lookup_users")

search_tweets2 <- run_it_back("rtweet::search_tweets")

search_users2  <- run_it_back("rtweet::search_users")

get_friends2   <- run_it_back("rtweet::get_friends")

get_followers2 <- run_it_back("rtweet::get_followers")

get_favorites2 <- run_it_back("rtweet::get_favorites")


fun_warning_nap <- function(.fn, .f, iter = 5, nap = 30) {
  .f <- c(.f, paste0(.fn, "_w", seq_len(iter - 1)))
  body <- paste0('    ', .fn, '_w', seq_len(iter), ' <- function(...) {
      tryCatch(
        ', .f, '(...),
        warning = function(w) {
          nap_retry(', nap, ')
          ', .f, '(...)
        }
      )
    }', collapse = "\n")

  eval(parse(text = paste0(
    'function(...) {\n',
    body, '\n',

    '    ', .fn, '_w', iter, '(...)\n}'
  )))
}

get_friends_warning_nap   <- fun_warning_nap("get_friends", "get_friends2", 5)

get_followers_warning_nap <- fun_warning_nap("get_followers", "get_followers2", 5)

search_tweets_warning_nap <- fun_warning_nap("search_tweets", "search_tweets2", 5)

get_timeline_warning_nap  <- fun_warning_nap("get_timeline", "get_timeline2", 5)

get_favorites_warning_nap <- fun_warning_nap("get_favorites", "get_favorites2", 5)
