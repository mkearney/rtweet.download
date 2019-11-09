


## function for: sleep until rate limit reset
rate_limit_sleep <- function() {
  if (!exists(".tkn")) {
    .tkn      <<- get_token()
  }
  if (!exists(".rl_count")) {
    .rl_count <<- 15 - rate_limit2("get_friends", token = .tkn)$remaining
    .regtoken <<- TRUE
  }
  if (.rl_count < 14L) {
    .rl_count <<- .rl_count + 1L
    return(invisible())
  }
  if (.regtoken && is_bearable()) {
    .tkn      <<- bearer_token()
    .regtoken <<- FALSE
  } else {
    .tkn <<- get_token()
    .regtoken <<- TRUE
  }
  rl          <- rate_limit2("get_friends", token = .tkn)
  .rl_count   <<- 15 - rl$remaining
  if (.rl_count < 15) {
    return(invisible())
  }
  s <- as.numeric(difftime(rl$reset_at, Sys.time(), units = "secs"))
  if (s < 0) {
    return(invisible())
  }
  cat("Sleeping for about", round(s / 60, 2), "minutes...\n")
  Sys.sleep(s + 1)
}


nap <- function(s) {
  pb <- progress::progress_bar$new(
    format = crayon::blue("Sleeping    [:bar] :percent"),
    total = 60, clear = FALSE, width = 60)
  pb$tick(0)
  for (i in seq_len(60)) {
    Sys.sleep(s / 60)
    pb$tick()
  }
  invisible(TRUE)
}
