exists.rr <- function(x) exists(x, envir = .rr, inherits = FALSE)

get.rr <- function(x) get(x, envir = .rr, inherits = FALSE)

capture_dots <- function(...) {
  eval(substitute(alist(...)), envir = parent.frame())
}

pretty_dots <- function(...) {
  dots <- capture_dots(...)
  if (length(dots) == 0) {
    return(NULL)
  }
  if (is.null(names(dots))) {
    names(dots) <- expr_names(dots)
  }
  nms <- names(dots)
  if ("" %in% nms) {
    names(dots)[nms == ""] <- expr_names(dots[nms == ""])
  }
  dots
}

expr_names <- function(args) {
  vapply(args, deparse, USE.NAMES = FALSE, FUN.VALUE = character(1))
}

mmap <- function(f, ...) {
  f <- match.fun(f)
  mapply(FUN = f, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

assign.rr <- function(...) {
  mmap(assign, names(pretty_dots(...)), list(...), MoreArgs = list(envir = .rr, inherits = FALSE))
  invisible()
}

`%||%` <- function(x, y) {
  if (is_null(x))
    y
  else x
}

is_null <- function(x) length(x) == 0L
