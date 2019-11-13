
cat_call <- function(usrs, tml, i) {
  if (nchar(usrs[i]) < max(nchar(usrs))) {
    sp <- paste(rep(" ", max(nchar(usrs)) - nchar(usrs[i])), collapse = "")
  } else {
    sp <- ""
  }
  cat(paste0(usrs[i], sp, ":"), sub("(?<=\\d)(?=\\d{3}$)", ",",
    sprintf("%4d", NROW(tml[[i]])), perl = TRUE), "tweets\n")
}

complete <- function(..., fill = TRUE) {
  cat(paste0(crayon::green(clisymbols::symbol$tick), " ", ...), fill = fill)
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
