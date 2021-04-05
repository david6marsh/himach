
test_that("onLoad works", {
  # must ensure using s2 afterwards
  requireNamespace("s2", quietly = TRUE)
  old_s2 <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)
  expect_silent(himach:::.onLoad())
  expect_true(sf::sf_use_s2())
  sf::sf_use_s2(old_s2)
})
