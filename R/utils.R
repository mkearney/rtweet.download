
cat_call <- function(usrs, tml, i) {
  if (nchar(usrs[i]) < max(nchar(usrs))) {
    sp <- paste(rep(" ", max(nchar(usrs)) - nchar(usrs[i])), collapse = "")
  } else {
    sp <- ""
  }
  cat(paste0(usrs[i], sp, ":"), sub("(?<=\\d)(?=\\d{3}$)", ",",
    sprintf("%4d", NROW(tml[[i]])), perl = TRUE), "tweets\n")
}



check <- function(..., fill = TRUE) {
  cat(paste0(crayon::green("✔ "), ...), fill = fill)
}

complete <- function(..., fill = TRUE) {
  cat(paste0(crayon::green(clisymbols::symbol$tick), " ", ...), fill = fill)
}

waiting <- function(..., fill = TRUE) {
  cat(paste0("⏱ ", ...), fill = fill)
}
nextone <- function(..., fill = TRUE) {
  cat(paste0("↩︎ ", ...), fill = fill)
}

cint <- function(x, sp = NULL) {
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
