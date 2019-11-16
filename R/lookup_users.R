#' Lookup users download
#'
#' Automate users data collection for a large number of users (via
#' \code{\link[rtweet]{lookup_users()}})
#'
#' @param x A vector of user IDs or screen names for which data will be looked up.
#'   See details for more information about how this works.
#' @param .usr Optionally supply a preexisting output vector (like that returned
#'   by this function)–if NULL, the default, this function will start fresh.
#' @return Returns a list data frames of user data
#' @family .usr
#' @details This function attempts to lookup data for 90,000 users every 15
#'   minutes, sleeping between calls until Twitter's API rate limit resets.
#' @export
lookup_users_download <- function(x, .usr = NULL) {
  x <- unique(x[!is.na(x)])

  ## if .usr is not supplied
  if (is_null(.usr)) {
    .usr <- vector("list", ceiling(length(x) / 90000) + 1L)
  } else {
    ## if .usr is supplied, ignore any users w/ data already collected
    if (all(grepl("^\\d+$", x))) {
      dr <- x %in% unlist(lapply(.usr, "[[", "user_id"))
    } else {
      dr <- x %in% unlist(lapply(.usr, "[[", "screen_name"))
    }
    if (any(dr)) {
      x <- x[!dr]
      check("Omit ", cint(sum(dr)), " users already collected")
    }
  }
  tusrs <- length(x)
  info("This should take around ", cdbl(tusrs / 90000 * 15), " mins")

  ## for loop
  for (i in seq_along(.usr)) {
    ## skip if data already exists
    if (NROW(.usr[[i]]) > 0) {
      next
    }
    ## determine number of users to lookup
    if (90000 > length(x)) {
      n <- length(x)
    } else {
      n <- 90000
    }
    rl <- rate_limit2("lookup_users")
    if (is_null(rl)) {
      s <- 60 * 15
      r <- 900 * 100
    } else if (rl$remaining == 0) {
      s <- as.numeric(rl$reset, "secs")
      r <- rl$limit * 100
    } else {
      s <- 0
      r <- rl$remaining * 100
    }
    if (s > 0) {
      nap_wait(s)
    }
    if (n > r) {
      n <- r
    }
    ## lookup users
    .usr[[i]] <- lookup_users2(x[seq_len(n)])
    ## drop the ones already looked up
    x <- x[-seq_len(n)]

    nrows_collected <- sum(dapr::vap_int(.usr, NROW))
    spf <- repc("1", nchar(use_commas_int(tusrs)))
    complete("Collected data for ", cint(nrows_collected, spf),
      " users (", cdbl((tusrs - length(x)) / tusrs * 100, "11.1"), "%)")
    if (length(x) == 0) {
      break
    }
  }
  if (length(.usr) > 1L && is_null(.usr[[length(.usr)]])) {
    .usr <- .usr[-length(.usr)]
  }
  .usr
}
