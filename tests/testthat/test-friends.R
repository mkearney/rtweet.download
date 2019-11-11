test_that("get_friends works", {
  expect_equal(2 * 2, 4)
})

kmw <- rtweet::search_tweets("url:1193338325257474048 OR to:kearneymw since_id:1193338325257474048", n = 200, include_rts = FALSE)
x <- dplyr::filter(kmw, reply_to_status_id == "1193338325257474048")$text
x <- gsub("(@|#|https?://)\\S+| or|\n|:|,|\\?", " ", x)
x <- tfse::trim_ws(x)
x <- tolower(ifelse(grepl("\\.[[:alpha:]]+", x), x, paste0(".", sub("(?<=[[:alpha:]])[^[:alpha:]].*", "", x, perl = TRUE))))
nms <- tfse::regmatches_(x, "\\.[[:alpha:]]+", drop = TRUE)
nms <- unique(sort(nms))
nms <- c(nms, ".xyz")

nms <- paste0(nms, dapr::vap_chr(Map(rep, " ", max(nchar(nms)) + 4 - nchar(nms)), paste, collapse = ""))

cat(paste0("## Ideas for {rtweet}" paste0("  ⁃ rtweet", nms[1:13], "⁃ rtweet", nms[14:26], "⁃ rtweet", nms[27:39], "⁃ rtweet", nms[40:52], collapse = "\n"))

cat()
"rtweet.page",
"rwteet.storage",
"rtweet.express",
"rtweet.guide",
"rtweet.works",
"rtweet.work",
"rtweet.download",
"rtweet.run",
"rtweet.tips",
"rtweet.work"
