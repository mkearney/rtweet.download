run_it_back <- function(.f, .times = 3L) {
  eval(parse(text = paste0('function(...) {
    .i <- 0L
    while (
      is.null(x <- tryCatch(', .f, '(...), error = function(e) NULL)) &&
        .i <= .times
    ) {
      .i <- .i + 1L
      Sys.sleep(1.5)
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

warning_fun <- function(w, nap = 30) {
  if (any(grepl("Rate limit|\\b88\\b", w))) {
    nap_retry(nap)
  }
}

exhaust_it <- function(.fun, .x, ..., .token) {
  args <- list("placeholder", ..., token = .token)
  o <- vector("list", length(.x))
  for (i in seq_along(.x)) {
    args[[1]] <- .x[[i]]
    o[[i]] <- do.call(.fun, args)
  }
  o
}
# o <- exhaust_it(rtweet::search_tweets, c("#rstats", "#rtweet"), n = 200,
#   .token = rtweet::get_token())

fun_warning_nap <- function(.fn, .f, iter = 5, nap = 30) {
  .f <- c(.f, paste0(.fn, "_w", seq_len(iter - 1)))
  body <- paste0('    ', .fn, '_w', seq_len(iter), ' <- function(...) {
      tryCatch(
        ', .f, '(...),
        warning = function(w) {
          if (any(grepl("Rate limit|\\b88\\b|too many", w, ignore.case = TRUE))) {
            nap_retry(', nap, ')
          }
          ', .f, '(...)
        },
        error = function(e) {
          Sys.sleep(1.0)
          tryCatch(', .f, '(...), error = function(e) tibble::tibble())
        }
      )
    }', collapse = "\n")

  eval(parse(text = paste0(
    'function(...) {\n',
    body, '\n',

    '    ', .fn, '_w', iter, '(...)\n}'
  )))
}

get_friends_warning_nap   <- fun_warning_nap("get_friends",   "get_friends",   5)

get_followers_warning_nap <- fun_warning_nap("get_followers", "get_followers2", 5)

search_tweets_warning_nap <- fun_warning_nap("search_tweets", "search_tweets", 5)

get_timeline_warning_nap  <- fun_warning_nap("get_timeline",  "get_timeline2",  5)

get_favorites_warning_nap <- fun_warning_nap("get_favorites", "get_favorites2", 5)
