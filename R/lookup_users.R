#' Lookup users download
#'
#' Automate users data collection for a large number of users (via
#' \code{\link[rtweet]{lookup_users}})
#'
#' @param x Either a data frame or character vector containing user identifiers
#'   for which users will be retrieved. See details for more information
#'   about how this works.
#' @param output Optionally supply a preexisting output vector (like that returned
#'   by this function)–if NULL, the default, this function will start fresh.
#' @param verbose Whether the function should print information/status updates,
#'   defaults to TRUE. Setting this to FALSE will silent most printing.
#' @return Returns a list data frames with looked up user information. See
#'   \code{\link[rtweet]{lookup_users}} for more information.
#' @family users
#' @details This function attempts to lookup users information for as many as
#'   90,000 users every 15 minutes, sleeping between calls until Twitter's API
#'   rate limit resets.
#'
#'   It's worth noting that information on many users will not be returned due
#'   to changed screen names, account suspensions, deactivations, etc.
#' @export
lookup_users_download <- function(x, output = NULL, verbose = TRUE) {
  UseMethod("lookup_users_download")
}

#' @export
lookup_users_download.data.frame <- function(x, output = NULL, verbose = TRUE) {
  if (n_row(x) == 0) {
    stop("No users found in this data frame")
  }
  x <- x[, dapr::vap_lgl(x, is.atomic), drop = FALSE]
  if (NCOL(x) == 0) {
    stop("No atomic columns found in this data frame")
  }
  if (ncol(x) == 1L && inherits(x[[1]], c("character", "factor"))) {
    x <- as.character(x[[1]])
    return(lookup_users_download(x, output = output, verbose = verbose))
  }
  if (grepl("user_id$|screen_name$", names(x)[1])) {
    x <- as.character(x[[1]])
    return(lookup_users_download(x, output = output, verbose = verbose))
  }
  if ("user_id" %in% names(x)) {
    x <- as.character(x[["user_id"]])
    return(lookup_users_download(x, output = output, verbose = verbose))
  }
  if ("screen_name" %in% names(x)) {
    x <- as.character(x[["screen_name"]])
    return(lookup_users_download(x, output = output, verbose = verbose))
  }
  if (any(grepl("user_id$|screen_name$", names(x)))) {
    x <- as.character(x[[grep("user_id$|screen_name$", names(x))[1]]])
    return(lookup_users_download(x, output = output, verbose = verbose))
  }
  stop("data frame must contain atomic 'user_id' or 'screen_name' column")
}

#' @export
lookup_users_download.character <- function(x, output = NULL, verbose = TRUE) {
  x <- x[!is.na(x) & !duplicated(x)]

  ## prepare and create token(s) object
  token <- rtweet::get_token()

  ## if output vector is not supplied
  if (is_null(output)) {
    output <- vector("list", ceiling(length(x) / 90000) + 1L)
  } else {
    stopifnot(
      is.list(output),
      length(output) >= ceiling(length(x) / 90000)
    )
  }
  tusrs <- length(x)
  if (verbose) {
    dotdotdot("This should take around ", cdbl(tusrs / 90000 * 15), " mins")
  }
  n <- usrs_rate_limit_sleep(token)
  ctr <- 0L

  tryCatch({
    ## for loop
    for (i in seq_along(output)) {
      ## skip if data already exists
      if (n_row(output[[i]]) > 0) {
        ctr <- ctr + n_row(output[[i]])
        x <- x[-seq_len(n_row(output[[i]]))]
        next
      }
      ## check rate limit remaining / change out token if possible
      while (n == 0) {
        n <- usrs_rate_limit_sleep(token)
      }

      if (n > length(x)) {
        n <- length(x)
      }

      ## lookup users data
      output[[i]] <- lookup_users_warning_nap(x[seq_len(n)], token = token)
      x <- x[-seq_len(n)]
      ctr <- ctr + n
      n <- 0L

      if (verbose) {
        complete(pgray(rd_timestamp()),
          "", pgold(cint(n_row(output[[i]]), "00,000")),
          pgray(" users looked up "),
          pgray(cli::symbol$ellipsis), pgray(" ("),
          pgray(cdbl(i / tusrs * 100, "1.1")), pgray("%)"))
      }
      if (length(x) == 0) {
        break
      }
    }
    output
  },
    interrupt = function(i) return(output),
    error = function(e) return(output)
  )
}


usrs_rate_limit_sleep <- function(token) {
  rl <- rate_limit2(query = "lookup_users", token = token)
  rlm <- rl[["remaining"]] %||% 0L
  if (rlm > 0) {
    return(rlm * 100L)
  }
  s <- as.numeric(rl[["reset"]] %||% 900, "secs")
  nap_wait(s + 60)
  90000L
}
