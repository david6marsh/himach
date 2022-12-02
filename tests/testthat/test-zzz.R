
test_that("onLoad works", {
  # must ensure using s2 afterwards
  requireNamespace("s2", quietly = TRUE)
  old_s2 <- sf::sf_use_s2()
  suppressMessages(
    capture.output(sf::sf_use_s2(FALSE))
    )
  # not interested in whether sf provides a message
  suppressMessages(
    capture.output(himach:::.onLoad())
    )
  # we're only need to know that now using s2
  expect_true(sf::sf_use_s2())
  suppressMessages(
    capture.output(sf::sf_use_s2(old_s2))
    )
})
