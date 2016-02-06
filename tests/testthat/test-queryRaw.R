context("queryRaw")

test_that("raw query", {
  cnx <- connect("https://jacaton-r.herokuapp.com", "demo", "1234")
  resource <- resource(cnx, "oficinas")
  out <- queryRaw(resource, "?limit=2&skip=1")
  expect_equal(2, nrow(out))
  expect_equal(7, ncol(out))
})