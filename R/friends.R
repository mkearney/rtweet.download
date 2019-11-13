

#' Get friends download
#'
#' Automate friends-list collection for a large number of users (via \code{\link[rtweet]{get_friends()}})
#'
#' @param x A vector of user IDs or screen names for which friends lists will be
#'   retrieved. See details for more information about how this works.
#' @param new Logical indicating whether to start a new data collection–if FALSE,
#'   the default, then this function will pick up where any previous in-session
#'   calls left off.
#' @return Returns a list of friends lists; this list can be retrieved via
#'   \code{\link{get_.fds()}} even if an error occurs (and the R session wasn't
#'   restarted).
#' @family .fds
#' @details This function attempts to retrieve friends lists for 15-30 users
#'   every 15 minutes, sleeping between calls until Twitter's API rate limit
#'   resets. If your API token is linked to your own Twitter APP and has
#'   appropriate permissions to create a 'bearer token', then this function will
#'   make 30 calls every 15 minutes. If the token cannot be used via bearer
#'   authorization, then 15 calls are made every 15 minutes.
#' @export
get_friends_download <- function(x, new = FALSE) {
  x <- unique(x[!is.na(x)])

  ## if new=TRUE ~~~ if .fds doesn't exist
  if (new || !exists.rr(".fds")) {
    .fds <- vector("list", length(x))
    assign.rr(.fds = .fds)
    complete("Create `.fds` in `.rr` environment")
  } else if (exists.rr(".fds")) {
    ## if .fds does exists, ignore any users w/ data already collected
    .fds <- get.rr(".fds")
    dr <- x %in% unlist(lapply(.fds, "[[", "user"))
    if (any(dr)) {
      x <- x[!dr]
      complete("Omit ", cint(sum(dr)), " users with friends already collected")
    }
  }
  tusrs <- length(x)
  if (is_bearable()) {
    rlc <- 30
  } else {
    rlc <- 15
  }
  waiting("This should take around ", cint(tusrs / rlc * 15), " mins")
  n <- 0L

  ## for loop
  for (i in seq_along(.fds)) {
    if (n == 0) {
      n <- fds_rate_limit_sleep()
    }

    if (inherits(get.rr(".tkn"), "bearer")) {
      rl <- rate_limit2("get_friends", token = get.rr(".tkn"))
      cat("using bearer token with", rl$remaining, "remaining\n")
    } else {
      rl <- rate_limit2("get_friends", token = get.rr(".tkn"))
      cat("using user token with", rl$remaining, "remaining\n")
    }

    ## get friends list – and extract next cursor (page) value
    .fds[[i]] <- get_friends2(x[i], token = get.rr(".tkn"))
    n <- n - 1L
    assign.rr(.fds = .fds)
    np <- next_cursor(.fds[[i]])

    ## if user follows more than 5,000 accounts, make additional calls using np
    while (length(np) > 0 && np != 0) {
      if (n == 0) {
        n <- fds_rate_limit_sleep()
      }

      if (inherits(get.rr(".tkn"), "bearer")) {
        rl <- rate_limit2("get_friends", token = get.rr(".tkn"))
        cat("using bearer token with", rl$remaining, "remaining\n")
      } else {
        rl <- rate_limit2("get_friends", token = get.rr(".tkn"))
        cat("using user token with", rl$remaining, "remaining\n")
      }

      fdsi <- get_friends2(x[i], page = np, token = get.rr(".tkn"))
      n <- n - 1L
      np <- next_cursor(fdsi)
      .fds[[i]] <- rbind(.fds[[i]], fdsi)
      assign.rr(.fds = .fds)
    }
    complete("Collected friend IDs for ",
      cint(i, sp = nchar(tusrs)), " users (", cint(i / tusrs * 100), "%)")
  }
  .fds
}


#' Get .fds
#'
#' Retrieves '.fds' object created by \code{\link{get_friends_recipe}()} and stored in
#' rtweet.recipes's special built-in environment.
#'
#' @return If .fds can be found, it (a list of friends lists) will be returned
#'   otherwise this produces an error.
#' @family .fds
#' @export
get_.fds <- function() {
  if (!exists.rr(".fds")) {
    stop("Cannot find a `.fds` object", call. = FALSE)
  }
  get.rr(".fds")
}
