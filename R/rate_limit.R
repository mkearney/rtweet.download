usr_rate_limit_sleep <- function() {
  ## .tkn = current token
  if (!exists.rr(".tkn")) {
    .tkn <- rtweet::get_token()
    assign.rr(.tkn = .tkn)
  } else {
    .tkn <- get.rr(".tkn")
  }
  rl <- rate_limit2("lookup_users", token = .tkn)
  rlm <- (rl[["remaining"]] %||% 0L)
  if (rlm > 0) {
    return(rlm)
  }
  if (rlm == 0 && !inherits(.tkn, "bearer") && is_bearable(.tkn)) {
    .btkn <- rtweet::bearer_token(.tkn)
    rlb <- rate_limit2("lookup_users", token = .btkn)
    rlbm <- (rlb[["remaining"]] %||% 0L)
    if (rlbm > 0) {
      assign.rr(.tkn = .btkn)
      return(rlbm)
    }
    rlra <- as.numeric(rl[["reset"]] %||% 900, "secs")
    rlbra <- as.numeric(rlb[["reset"]] %||% 900, "secs")
    if (rlra <= rlbra) {
      s <- rlra
      assign.rr(.tkn = .tkn)
    } else {
      s <- rlbra
      assign.rr(.tkn = .btkn)
    }
    if (s < 0) {
      s <- 900
    }
    nap_wait(s + 1)
    return(15L)
  }

  if (rlm == 0 && inherits(.tkn, "bearer")) {
    .btkn <- rtweet::get_token()
    rlb <- rate_limit2("lookup_users", token = .btkn)
    rlbm <- (rlb[["remaining"]] %||% 0L)
    if (rlbm > 0) {
      assign.rr(.tkn = .btkn)
      return(rlbm)
    }
    rlra <- as.numeric(rl[["reset"]] %||% 900, "secs")
    rlbra <- as.numeric(rlb[["reset"]] %||% 900, "secs")
    if (rlra <= rlbra) {
      s <- rlra
      assign.rr(.tkn = .tkn)
    } else {
      s <- rlbra
      assign.rr(.tkn = .btkn)
    }
    if (s < 0) {
      s <- 900
    }
    nap_wait(s + 1)
    return(15L)
  }
  s <- as.numeric(rl[["reset"]] %||% 900, "secs")
  nap_wait(s + 1L)
  15L
}


fds_rate_limit_sleep <- function() {
  ## .tkn = current token
  if (!exists.rr(".tkn")) {
    .tkn <- rtweet::get_token()
    assign.rr(.tkn = .tkn)
  } else {
    .tkn <- get.rr(".tkn")
  }
  rl <- rate_limit2("get_friends", token = .tkn)
  rlm <- (rl[["remaining"]] %||% 0L)
  if (rlm > 0) {
    return(rlm)
  }
  if (rlm == 0 && !inherits(.tkn, "bearer") && is_bearable(.tkn)) {
    .btkn <- rtweet::bearer_token(.tkn)
    rlb <- rate_limit2("get_friends", token = .btkn)
    rlbm <- (rlb[["remaining"]] %||% 0L)
    if (rlbm > 0) {
      assign.rr(.tkn = .btkn)
      return(rlbm)
    }
    rlra <- as.numeric(rl[["reset"]] %||% 900, "secs")
    rlbra <- as.numeric(rlb[["reset"]] %||% 900, "secs")
    if (rlra <= rlbra) {
      s <- rlra
      assign.rr(.tkn = .tkn)
    } else {
      s <- rlbra
      assign.rr(.tkn = .btkn)
    }
    if (s < 0) {
      s <- 900
    }
    nap_wait(s + 1)
    return(15L)
  }

  if (rlm == 0 && inherits(.tkn, "bearer")) {
    .btkn <- rtweet::get_token()
    rlb <- rate_limit2("get_friends", token = .btkn)
    rlbm <- (rlb[["remaining"]] %||% 0L)
    if (rlbm > 0) {
      assign.rr(.tkn = .btkn)
      return(rlbm)
    }
    rlra <- as.numeric(rl[["reset"]] %||% 900, "secs")
    rlbra <- as.numeric(rlb[["reset"]] %||% 900, "secs")
    if (rlra <= rlbra) {
      s <- rlra
      assign.rr(.tkn = .tkn)
    } else {
      s <- rlbra
      assign.rr(.tkn = .btkn)
    }
    if (s < 0) {
      s <- 900
    }
    nap_wait(s + 1)
    return(15L)
  }
  s <- as.numeric(rl[["reset"]] %||% 900, "secs")
  nap_wait(s + 1L)
  15L
}

## function for: sleep until rate limit reset
fds_rate_limit_sleep_ <- function() {

  ## .tkn = current token
  if (!exists.rr(".tkn")) {
    .tkn <- rtweet::get_token()
    assign.rr(.tkn = .tkn)
  } else {
    .tkn <- get.rr(".tkn")
  }

  ## .regtoken = whether user token or bearer token
  .regtoken <- !inherits(.tkn, "bearer")
  assign.rr(.regtoken = .regtoken)

  ## .rl_fds_count = running count of requests
  if (!exists.rr(".rl_fds_count")) {
    rl <- rate_limit2("get_friends", token = .tkn)
    .rl_fds_count <- 15L - (rl[["remaining"]] %||% 0L)
    .rl_reset_at <- rl[["reset_at"]] %||% (Sys.time() + 60 * 15)
  } else {
    .rl_fds_count <- get.rr(".rl_fds_count")
    .rl_reset_at <- get.rr(".rl_reset_at")
  }
  if (Sys.time() > .rl_reset_at) {
    .rl_fds_count <- 0L
  }

  ## if .rl_fds_count is less than 15 continue
  if (.rl_fds_count < 15L) {
    .rl_fds_count <- .rl_fds_count + 1L
    assign.rr(.rl_fds_count = .rl_fds_count)
    assign.rr(.rl_reset_at = .rl_reset_at)
    return(invisible())
  }

  ## switch to bearer or user token (if possible)
  if (.regtoken && is_bearable(.tkn)) {
    .tkn <- rtweet::bearer_token(.tkn)
    .regtoken <- FALSE
  } else if (!.regtoken) {
    .tkn <- rtweet::get_token()
    .regtoken <- TRUE
  }

  ## get rate limit information
  rl <- rate_limit2("get_friends", token = .tkn)
  .rl_fds_count <- 15L - (rl[["remaining"]] %||% 0L)
  .rl_reset_at <- rl[["reset_at"]] %||% (Sys.time() + 60 * 15)
  assign.rr(.tkn  = .tkn)
  assign.rr(.regtoken = .regtoken)
  assign.rr(.rl_reset_at = .rl_reset_at)

  ## if remaining calls then continue
  if (.rl_fds_count < 15) {
    .rl_fds_count <- .rl_fds_count + 1L
    assign.rr(.rl_fds_count = .rl_fds_count)
    return(invisible())
  }

  ## otherwise sleep
  s <- as.numeric(difftime(rl[["reset_at"]] %||% (Sys.time() + 60 * 15),
    Sys.time(), units = "secs"))
  assign.rr(.rl_fds_count = 1L)
  assign.rr(.rl_reset_at = Sys.time() + 60 * 15)
  if (s < 0) {
    return(invisible())
  }
  nap(s)
}


nap_wait <- function(s) {
  pb <- progress::progress_bar$new(
    format = crayon::blue("Sleep until rate limit reset [:bar] :eta"),
    total = 1000, clear = TRUE, width = 60)
  pb$tick(0)
  for (i in seq_len(1000)) {
    Sys.sleep(s / 1000)
    pb$tick()
  }
  invisible(TRUE)
}

nap_retry <- function(s) {
  pb <- progress::progress_bar$new(
    format = crayon::blue("Waiting to retry [:bar] :eta"),
    total = 1000, clear = TRUE, width = 60)
  pb$tick(0)
  for (i in seq_len(1000)) {
    Sys.sleep(s / 1000)
    pb$tick()
  }
  invisible(TRUE)
}

determine_token <- function(token, query) {
  ## if next_token already exists
  if ("next_token" %in% names(token)) {
    currentoken      <- token$token
    token$token      <- token$next_token
    token$next_token <- currentoken
    return(token)
  }
  ## if not bearable, then just use the one
  if (!"bearer_token" %in% names(token)) {
    token$token      <- token$user
    token$next_token <- token$user
    return(token)
  }
  ## determine next token
  rlu <- rate_limit2(token = token$user, query = query)
  rlb <- rate_limit2(token = token$bearer, query = query)
  if (isTRUE(rlb$remaning > rlu$remaining)) {
    token$token      <- token$bearer
    token$next_token <- token$user
  } else if (isTRUE(rlu$remaning > rlb$remaining)) {
    token$token      <- token$user
    token$next_token <- token$bearer
  } else if (isTRUE(rlu$reset_at <= rlb$reset_at)) {
    token$token      <- token$user
    token$next_token <- token$bearer
  } else {
    token$token      <- token$bearer
    token$next_token <- token$user
  }
  token
}

prep_tokens <- function(token = NULL) {
  token <- token %||% rtweet::get_token()
  if (!is_bearable(token)) {
    return(list(user = token))
  }
  list(user = token, bearer = rtweet::bearer_token(token))
}

determine_count <- function(token, query, first = FALSE) {
  if (is_bearer(token$token)) {
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
  rl <- rate_limit2(token = token$token, query = query)
  if (rl$remaining == 0) {
    return(n)
  }
  rl$remaining * (n / rl$limit)
}
