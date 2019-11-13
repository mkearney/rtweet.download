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
search_users2 <- run_it_back("rtweet::search_users")
get_friends2 <- run_it_back("rtweet::get_friends")
get_followers2 <- run_it_back("rtweet::get_followers")
get_favorites2 <- run_it_back("rtweet::get_favorites")
