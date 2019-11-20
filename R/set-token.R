
#' Set token
#'
#' Stores Twitter API token information for the duration of the session
#'
#' @param x Either a token or path to a token. If path, the token is read using
#'   \code{readRDS} (this is default {rtweet} behavior). If token, it is saved
#'   in the current working directory as ".rtweet_token"
#' @return The token is invisibly returned but more importantly the environment
#'   variable "TWITTER_PAT" is set to point toward the saved token file. This
#'   will be reset at the end of the session.
#' @examples
#'
#' ## if your system already has an environment variable for an rtweet token,
#' ## this will return the path
#' (pat <- Sys.getenv("TWITTER_PAT"))
#'
#' ## if your system doesn't have this environment variable OR if you wish to
#' ## override this value, then enter the desired path or token object
#' #pat <- "/path/to/rtweet-token.rds"
#'
#' ## and then set the token for use for the remainder of the session
#' set_token(pat)
#'
#' @export
set_token <- function(x) {
  UseMethod("set_token")
}

#' @export
set_token.character <- function(x) {
  if (!file.exists(x)) {
    stop("Couldn't find token file", call. = FALSE)
  }
  stopifnot(
    is_usertoken(readRDS(x)) || is_bearer(readRDS(x))
  )
  Sys.setenv(TWITTER_PAT = x)
  complete("Environment variable set: 'TWITTER_PAT=" %P% x, "'")
  invisible(token)
}

#' @export
set_token.Token <- function(x) {
  saveRDS(x, ".rtweet_token")
  Sys.setenv(".rtweet_token")
  complete("Token saved and environment variable set: 'TWITTER_PAT=.rtweet_token'")
  invisible(x)
}

#' @export
set_token.bearer <- function(x) {
  saveRDS(x, ".rtweet_token")
  Sys.setenv(".rtweet_token")
  complete("Token saved and environment variable set: 'TWITTER_PAT=.rtweet_token'")
  invisible(x)
}
