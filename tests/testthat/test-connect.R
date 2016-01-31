context("connect")

test_that("connect should work", {
  out <- connect("https://jacaton-r.herokuapp.com", "demo", "1234")
  expect_equal(TRUE, is.list(out))
  expect_equal("https://jacaton-r.herokuapp.com", out[[1]])
  expect_equal("demo", out[[2]])
  expect_equal("1234", out[[3]])
  expect_equal(200, out[[4]]$status)
})

test_that("connect should fail: due to unexisting URL", {
  expect_error(connect("https://api.acme.com", "demo", "1234"))
})

test_that("connect should fail due to invalid credentials", {
  expect_error(connect("https://jacaton-r.herokuapp.com", "noUser", "noPassword"))
})
