nap_wait <- function(s) {
  pb <- progress::progress_bar$new(
    format = crayon::blue("Sleep until rate limit reset [:bar] :eta"),
    total = 500, clear = TRUE, width = 60)
  pb$tick(0)
  for (i in seq_len(500)) {
    Sys.sleep(s / 500)
    pb$tick()
  }
  invisible(TRUE)
}

nap_retry <- function(s) {
  pb <- progress::progress_bar$new(
    format = crayon::blue("Waiting to retry [:bar] :eta"),
    total = 500, clear = TRUE, width = 60)
  pb$tick(0)
  for (i in seq_len(500)) {
    Sys.sleep(s / 500)
    pb$tick()
  }
  invisible(TRUE)
}
