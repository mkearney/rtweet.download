exists.rr <- function(x) exists(x, envir = .rr, inherits = FALSE)

get.rr <- function(x) get(x, envir = .rr, inherits = FALSE)

assign.rr <- function(...) {
  mmap(assign, names(pretty_dots(...)), list(...), MoreArgs = list(envir = .rr, inherits = FALSE))
  invisible()
}
