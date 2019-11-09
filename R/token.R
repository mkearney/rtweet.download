is_bearable <- function(token = NULL) {
  if (exists.rr("bearable")) {
    return(get.rr("bearable"))
  }
  token <- token %||% rtweet::get_token()
  assign.rr(bearable = isTRUE(grepl("read-write", rtweet:::api_access_level(token))))
  invisible()
}

current_token <- function() {
  if (exists.rr("current_token")) {
    return(get.rr("current_token"))
  }
  assign.rr(current_token = rtweet::get_token())
  invisible()
}
