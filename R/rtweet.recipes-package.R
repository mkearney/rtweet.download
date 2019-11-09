#' @keywords internal
#' @import rtweet
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

.rr <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # op <- options()
  # op.rtweet.recipes <- list(
  #   rtweet.recipes.token_path = "~/R-dev"
  # )
  # toset <- !(names(op.rtweet.recipes) %in% names(op))
  # if (any(toset)) {
  #   options(op.rtweet.recipes[toset])
  # }
  invisible()
}
