context("resource")

test_that("select resource exoplanet", {
  cnx <- connect("https://jacaton-r.herokuapp.com", "demo", "1234")
  out <- resource(cnx, "exoplanets")
  expect_equal(TRUE, is.list(out))
  expect_equal("exoplanets", out[[5]])
})

test_that("select resource oficinas", {
  cnx <- connect("https://jacaton-r.herokuapp.com", "demo", "1234")
  out <- resource(cnx, "oficinas")
  expect_equal(TRUE, is.list(out))
  expect_equal("oficinas", out[[5]])
})

