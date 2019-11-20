
complete <- function(..., fill = TRUE) {
  cat(paste0(crayon::green(clisymbols::symbol$tick), " ", ...), fill = fill)
}

info <- function(..., fill = TRUE) {
  cat(paste0(crayon::magenta(clisymbols::symbol$info), " ", ...), fill = fill)
}

dotdotdot <- function(..., fill = TRUE) {
  cat(paste0(crayon::magenta(clisymbols::symbol$ellipsis), " ", ...), fill = fill)
}

this <- function(..., fill = TRUE) {
  cat(paste0(crayon::blue(clisymbols::symbol$arrow_right), " ", ...), fill = fill)
}


cat_line <- function(...) {
  cat(paste0(..., "\n"), sep = "")
}

cint2 <- function(x, sp = NULL) {
  if (!is.integer(x)) {
    x <- round(x, 0)
  }
  if (is.null(sp) || sp < max(nchar(x))) {
    sp <- max(nchar(x))
  }
  x <- sub("\\.\\d+", "", sprintf(paste0("%", sp, "f"), x))
  while (grepl("\\d{4}", x)) {
    x <- sub("(?<=\\d)((?=\\d{3}$)|(?=\\d{3},))", ",", x, perl = TRUE)
  }
  x
}

use_commas_dbl <- function(x) {
  x <- as.character(x)
  dec <- sub("^[^\\.]+(?=\\.)", "", x, perl = TRUE)
  x <- tfse::regmatches_first(x, "^[^\\.]+")
  while (grepl("\\d{4}", x)) {
    x <- sub("(?<=\\d)((?=\\d{3}$)|(?=\\d{3},))", ",", x, perl = TRUE)
    x <- sub("^[ ]{1}", "", x)
  }
  paste0(x, dec)
}
use_commas_int <- function(x) {
  x <- as.character(x)
  while (grepl("\\d{4}", x)) {
    x <- sub("(?<=\\d)((?=\\d{3}$)|(?=\\d{3},))", ",", x, perl = TRUE)
    x <- sub("^[ ]{1}", "", x)
  }
  x
}

format_num <- function(x, f = "1.1") {
  f2 <- nchar(sub("^[^\\.]{0,}\\.", "", f))
  f1 <- nchar(sub("(?<=\\.).*", "", f, perl = TRUE)) + f2
  f <- paste0("%", f1, ".", f2, "f")
  sprintf(f, x)
}
format_int <- function(x, f = "1") {
  f <- paste0("%", nchar(f), ".", 0, "f")
  sprintf(f, x)
}
cdbl <- function(x, f = "1.1") {
  x <- format_num(x, f)
  use_commas_dbl(x)
}
cint <- function(x, f = "1") {
  x <- format_int(x, f)
  use_commas_int(x)
}
repc <- function(x, n, collapse = "") paste(rep(x, n), collapse = collapse)


assert_that <- function(..., env = parent.frame(), msg = NULL) {
  res <- see_if(..., env = env, msg = msg)
  if (res) return(TRUE)

  stop(assert_error(attr(res, "msg")))
}

assert_error <- function (message, call = NULL) {
  class <- c("assert_error", "simpleError", "error", "condition")
  structure(list(message = message, call = call), class = class)
}

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


`%||%` <- function(x, y) {
  if (is_null(x))
    y
  else x
}

is_null <- function(x) length(x) == 0L

rd_timestamp <- function() format(Sys.time(), "%b %d %H:%M:%S")

is_usertoken <- function(x) inherits(x, "Token")

not_token <- function(x) is.list(x) && !is_bearable(x) && !is_usertoken(x)

n_row <- function(...) {
  NROW(tryCatch(..., error = function(e) NULL))
}
