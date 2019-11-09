

#' Lookup users recipe
#'
#' Automate users data collection for a large number of users (via \code{\link[rtweet]{lookup_Users()}})
#'
#' @param x A vector of user IDs or screen names for which data will be looked up.
#'   See details for more information about how this works.
#' @param new Logical indicating whether to start a new data collection–if FALSE,
#'   the default, then this function will pick up where any previous in-session
#'   calls left off.
#' @return Returns a list of users data; this list can be retrieved via
#'   \code{\link{get_.usr()}} even if an error occurs (and the R session wasn't
#'   restarted).
#' @family .usr
#' @details This function attempts to lookup data for 90,000 users every 15
#'   minutes, sleeping between calls until Twitter's API rate limit resets.
#' @export
lookup_users_recipe <- function(x, new = FALSE) {
  x <- unique(x[!is.na(x)])
  ## if new=TRUE ~~~ if .usr doesn't exist
  if (new || !exists.rr(".usr")) {
    .usr <- list()
    assign.rr(.usr = .usr)
    check("Create `.usr` in `.rr` environment")
  } else if (exists.rr(".usr")) {
    ## if .usr does exists, ignore any users w/ data already collected
    .usr <- get.rr(".usr")
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
  waiting("This should take around ", cint(tusrs / 90000 * 15), " mins")

  while (length(x) > 0) {
    n <- 90000
    if (n > length(x)) {
      n <- length(x)
    }
    rl <- rate_limit2("lookup_users")
    if (rl$remaining < 20) {
      s <- as.numeric(rl$reset, "secs")
      if (s > 0) {
        nap(s)
      }
      rl$remaining <- rl$limit
    }
    if (n > (rl$remaining * 100)) {
      n <- rl$remaining * 100
    }
    .usr[[length(.usr) + 1L]] <<- lookup_users2(x[seq_len(n)])
    assign.rr(.usr = ".usr")
    x <- x[-seq_len(n)]
    nrows_collected <- sum(dapr::vap_int(.usr, NROW))
    complete("Data collection total: ", cint(nrows_collected),
      " users (", cint(nrows_collected / tusrs * 100), "%)")
  }
  .usr
}


#' Get .usr
#'
#' Retrieves '.usr' object created by \code{\link{lookup_users_recipe}()} and stored in
#' rtweet.recipes's special built-in environment.
#'
#' @return If .usr can be found, it (a list of users data) will be returned
#'   otherwise this produces an error.
#' @family .usr
#' @export
get_.usr <- function() {
  if (!exists.rr(".usr")) {
    stop("Cannot find a `.usr` object", call. = FALSE)
  }
  get.rr(".usr")
}
