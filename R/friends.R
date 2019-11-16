

#' Get friends download
#'
#' Automate friends-list collection for a large number of users (via
#' \code{\link[rtweet]{get_friends()}})
#'
#' @param x A vector of user IDs or screen names for which friends lists will be
#'   retrieved. See details for more information about how this works.
#' @param .fds Optionally supply a preexisting output vector (like that returned
#'   by this function)–if NULL, the default, this function will start fresh.
#' @return Returns a list data frames with user and friend ID information
#' @family .fds
#' @details This function attempts to retrieve friends lists for 15-30 users
#'   every 15 minutes, sleeping between calls until Twitter's API rate limit
#'   resets. If your API token is linked to your own Twitter APP and has
#'   appropriate permissions to create a 'bearer token', then this function will
#'   make 30 calls every 15 minutes. If the token cannot be used via bearer
#'   authorization, then 15 calls are made every 15 minutes.
#' @export
get_friends_download <- function(x, .fds = NULL) {
  x <- unique(x[!is.na(x)])
  if (all(grepl("^\\d+$", x))) {
    ul <- lookup_users2(x)
    sns <- ul$screen_name[match(x, ul$user_id)]
    sns <- ifelse(is.na(sns), ul, paste0("@", sns))
  } else {
    sns <- paste0("@", x)
  }
  mchars <- max(nchar(sns))
  sns <- paste0(dapr::vap_chr(mchars - nchar(sns), ~
      paste0(rep(" ", .x), collapse = "")), sns)

  ## if .fds is not supplied
  if (is_null(.fds)) {
    .fds <- vector("list", length(x))
  } else {
    ## if .fds is supplied, ignore any users w/ data already collected
    dr <- x %in% unlist(lapply(.fds, "[[", "user"))
    if (any(dr)) {
      x <- x[!dr]
      sns <- sns[!dr]
      complete("Omit ", cint(sum(dr)), " users with friends already collected")
    }
  }
  tusrs <- length(x)
  if (is_bearable()) {
    rlc <- 30
  } else {
    rlc <- 15
  }
  info("This should take around ", cdbl(tusrs / rlc * 15), " mins")
  n <- 0L

  ## for loop
  for (i in seq_along(.fds)) {
    ## skip if data already exists
    if (NROW(.fds[[i]]) > 0) {
      next
    }
    ## check rate limit remaining / change out token if possible
    if (n == 0) {
      n <- fds_rate_limit_sleep()
    }

    ## get friends list – and extract next cursor (page) value
    .fds[[i]] <- get_friends_warning_nap(x[i], token = get.rr(".tkn"))
    n <- n - 1L
    np <- next_cursor(.fds[[i]])

    ## if user follows more than 5,000 accounts, make additional calls using np
    while (length(np) > 0 && np != 0) {
      if (n == 0) {
        n <- fds_rate_limit_sleep()
      }

      fdsi <- get_friends_warning_nap(x[i], page = np, token = get.rr(".tkn"))
      n <- n - 1L
      np <- next_cursor(fdsi)
      if (NROW(fdsi) > 0) {
        .fds[[i]] <- rbind(.fds[[i]], fdsi)
      }
    }
    complete("Collected ", cint(NROW(.fds[[i]]), "100,000"),
      " friend IDs for ", sns[i],
      " (", cdbl(i / tusrs * 100, "11.1"), "%)")
  }
  .fds
}

