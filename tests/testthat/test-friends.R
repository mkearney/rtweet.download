test_that("get_friends_download works", {
  expect_error(get_friends_download())
  expect_error(get_friends_download(data.frame(x = rnorm(5), y = rnorm(5), z = rnorm(5))))
})
