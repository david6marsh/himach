library(testthat)
library(Mach2)

test_that("Default aircraft data loads", {
  expect_warning(make_aircraft())
  expect_known_value(make_aircraft(warn = FALSE), "known/ac_default_load")
})

test_that("Aircraft data loads", {
  ac <- data.frame(id = "test", type = "test aircraft",
                   over_sea_M = 2.0, over_land_M = 0.9, accel_Mpm = 0.2,
                   arrdep_kph = 300, range_km = 6000, stringsAsFactors=FALSE)
  expect_known_value(make_aircraft(ac), "known/ac_load")
  #missing vbl
  ac <- data.frame(id = "test", type = "test aircraft",
                   over_sea_M = 2.0, over_land_M = 0.9,
                   arrdep_kph = 300, range_km = 6000, stringsAsFactors=FALSE)
  expect_error(make_aircraft(ac))
  # vbl not numeric
  ac <- data.frame(id = "test", type = "test aircraft",
                   over_sea_M = 2.0, over_land_M = 0.9, accel_Mpm = 0.2,
                   arrdep_kph = 300, range_km = "6,000", stringsAsFactors=FALSE)
  expect_error(make_aircraft(ac))

})

test_that("Default airport data loads", {
  expect_message(z <- make_airports())
  # expect_known_hash(make_airports(),
  #                    hash = "00682f18dc")
  expect_known_value(z %>% filter(APICAO == "EGLL"), "known/default_airport_EGLL")
})

test_that("Airport data loads", {
  # normal functioning
  airports <- data.frame(APICAO = "TEST", lat = 10, long = 10, stringsAsFactors = FALSE)
  expect_known_value(make_airports(airports), "known/TEST_airport")

  # with missing variable
  airports_miss <- data.frame(APICAO = "TEST", lat = 10, stringsAsFactors = FALSE)
  expect_error(make_airports(airports_miss), "is missing:")


})

test_that("NZ maps available", {
  expect_true(all(st_is(NZ_coast, c("POLYGON", "MULTIPOLYGON"))))
  expect_true(all(st_is(NZ_buffer30, c("POLYGON", "MULTIPOLYGON"))))
})

test_that("can make AP2",{
  z <- make_AP2("EGLL","NZCH")
  expect_equal(z$AP2, "EGLL<>NZCH")
  expect_equal(signif(z$gcdist_km,3), 19000)
  #don't mind which order they're in
  expect_setequal(round(z[1,c("from_long", "to_long")],2),
                  c(-0.46, 172.53))
  aps <- make_airports()
  z <- make_AP2("BIKF", "EDDF")
  #check sort order
  expect_equal(z$AP2, "EDDF<>BIKF")
  expect_error(make_AP2("EGLL","ZZZZ", aps), "unknown")
})

test_that("can copy attributes", {
  x <- 1
  attr(x, "test") <- "here"
  y <- 1
  y <- Mach2:::copy_attr(x, y, c("test"))
  expect_equal(attributes(x), attributes(y))

  expect_warning(Mach2:::copy_attr(x, y, c("not here")))
})

test_that("can rename in an environment", {
  test_env <- new.env()
  assign("rubbly", 5, envir = test_env)
  woof <- Mach2:::ren_subst("rubbly", "ubbl", "obber",
                            in_env = test_env)
  expect_equal(get("robbery", envir = test_env), 5)
})
