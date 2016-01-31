context("count")

test_that("count offices > 2", {
  cnx <- connect("https://jacaton-r.herokuapp.com", "demo", "1234")
  resource <- resource(cnx, "oficinas")
  out <- count(resource)
  expect_equal(TRUE, out > 2)
})

test_that("count sevilla office == 1", {
  cnx <- connect("https://jacaton-r.herokuapp.com", "demo", "1234")
  resource <- resource(cnx, "oficinas")
  out <- count(resource, conditions=buildCondition("nombre", "==", "Seville"))
  expect_equal(out, 1)
})