

#' Get friends download
#'
#' Automate the collection of friend IDs for a large number of users (via
#' \code{\link[rtweet]{get_friends}})
#'
#' @param x Either a data frame or character vector containing user identifiers
#'   for which friends IDs will be retrieved. See details for more information
#'   about how this works.
#' @param ... If \code{x} is a data frame this can be used to select columns
#'   containing the appropriate user identifying information (user_id and/or
#'   screen_name). This uses the tidyselect specification. If \code{x} is a
#'   character vector, then the first unnamed or non-argument named value is
#'   assumed to be labels (screen names) corresponding with \code{x}.
#' @param output Optionally supply a preexisting output vector (like that returned
#'   by this function)–if NULL, the default, this function will start fresh.
#' @param verbose Whether the function should print information/status updates,
#'   defaults to TRUE. Setting this to FALSE will silent most printing.
#' @return Returns a list data frames with user and friend ID information. See
#'   \code{\link[rtweet]{get_friends}} for more information.
#' @family friends
#' @details This function attempts to retrieve friends IDs for as many as 15 to
#'   30 users every 15 minutes, sleeping between calls until Twitter's API rate
#'   limit resets. If your API token is linked to your own Twitter APP and has
#'   appropriate permissions to create a 'bearer token', then this function will
#'   make 30 calls every 15 minutes. If the token cannot be used via bearer
#'   authorization, then 15 calls are made every 15 minutes.
#'
#'   It's worth noting that 15 calls does not mean 15 complete friend IDs.
#'   Twitter limits the number of returned friends returned by a single call to
#'   5,000, which is more friends than 95% of Twitter users, but at times is
#'   frustrating because some users like Senator Amy Klobuchar follow over
#'   170,000 accounts–that many accounts can take 45 mintues to collect! It's
#'   interesting, though, that Amy Klobuchar is also unusually prolific in both
#'   following Twitter users and writing/sponsoring legislation.
#' @export
get_friends_download <- function(x, ..., output = NULL, verbose = TRUE) {
  UseMethod("get_friends_download")
}

#' @export
get_friends_download.data.frame <- function(x, ..., output = NULL, verbose = TRUE) {
  x <- x[, tidyselect::vars_select(names(x), ...), drop = FALSE]
  stopifnot(
    nrow(x) > 0L,
    ncol(x) < 3L
  )
  if (ncol(x) == 1L) {
    sns <- x[[1]]
    x <- x[[1]]
  } else if (all(grepl("^\\d+$", x[[1]]))) {
    sns <- x[[2]]
    x <- x[[1]]
  } else {
    sns <- x[[1]]
    x <- x[[2]]
  }
  get_friends_download(x, sns, output = output, verbose = verbose)
}

#' @export
get_friends_download.character <- function(x, ..., output = NULL, verbose = TRUE) {
  ## prepare screen names and user IDs
  sns <- ..1 %||% x
  sns <- sns[!is.na(x) & !duplicated(x)]
  x <- x[!is.na(x) & !duplicated(x)]
  if (all(!grepl("^\\d+$", sns))) {
    sns <- paste0("@", sns)
  }
  mchars <- max(nchar(sns))
  sns <- paste0(dapr::vap_chr(mchars - nchar(sns), ~
      paste0(rep(" ", .x), collapse = "")), sns)

  ## prepare and create token(s) object
  token <- prep_tokens(rtweet::get_token())

  ## if output vector is not supplied
  if (is_null(output)) {
    output <- vector("list", length(x))
  } else {
    stopifnot(
      is.list(output),
      length(output) == length(x)
    )
  }
  tusrs <- length(x)
  if (has_bearer(token)) {
    rlc <- 30
  } else {
    rlc <- 15
  }
  if (verbose) {
    dotdotdot("This should take around ", cdbl(tusrs / rlc * 15), " mins")
  }
  token <- determine_token(token, "get_friends")
  n <- fds_rate_limit_sleep(token)

  tryCatch({
    ## for loop
    for (i in seq_along(output)) {
      ## skip if data already exists
      if (n_row(output[[i]]) > 0) {
        next
      }
      ## check rate limit remaining / change out token if possible
      while (n == 0) {
        token <- determine_token(token, "get_friends")
        n <- fds_rate_limit_sleep(token)
      }

      ## get friends list – and extract next cursor (page) value
      output[[i]] <- get_friends_warning_nap(x[i], token = this_token(token))
      n <- n - 1L
      np <- next_cursor_download(output[[i]])

      ## if user follows more than 5,000 accounts, make additional calls using np
      while (length(np) > 0 && !np %in% c(0, -1)) {
        while (n == 0) {
          token <- determine_token(token, "get_friends")
          n <- fds_rate_limit_sleep(token)
        }
        fdsi <- get_friends_warning_nap(x[i], page = np, token = this_token(token))
        n <- n - 1L
        np <- next_cursor_download(fdsi)
        if (n_row(fdsi) > 0) {
          output[[i]] <- rbind(output[[i]], fdsi)
        }
      }
      if (verbose) {
        complete(rd_timestamp(),
          " ", cint(n_row(output[[i]]), "100,000"),
          " friend IDs for ", sns[i],
          " (", cdbl(i / tusrs * 100, "11.1"), "%)")
      }

    }
    output
  },
    interrupt = function(i) return(output),
    error = function(e) return(output)
  )
}


next_cursor_download <- function(x) {
  tryCatch(
    rtweet::next_cursor(x),
    error = function(e) NULL
  )
}

fds_rate_limit_sleep <- function(token) {
  rl <- rate_limit2(query = "get_friends", token = this_token(token))
  rlm <- rl[["remaining"]] %||% 0L
  if (rlm > 0) {
    return(rlm)
  }
  s <- as.numeric(rl[["reset"]] %||% 900, "secs")
  nap_wait(s + 10)
  15L
}
