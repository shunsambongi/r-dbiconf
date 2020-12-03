library(mockery)
library(mockr)

test_that("interactive loader works when value is TRUE", {
  loader <- list(interactive = TRUE)
  loader <- new_loader(loader, "TEST")

  m <- mock("secret")
  mockr::with_mock(
    loader_resolve_interactive_secret = m,
    loader_resolve_interactive_rstudio = function(...) "bad rstudio",
    loader_resolve_interactive_readline = function(...) "bad readline",
    arg <- loader_resolve(loader)
  )

  expect_called(m, 1L)
  expect_equal(arg, "secret")
})

test_that("interactive loader works with rstudioapi", {
  loader <- list(interactive = list(prompt = "test prompt", secret = FALSE))
  loader <- new_loader(loader, "TEST")

  m <- mock("rstudio")
  mockr::with_mock(
    loader_resolve_interactive_secret = function(...) "bad secret",
    loader_resolve_interactive_rstudio = m,
    loader_resolve_interactive_readline = function(...) "bad readline",
    rstudioapi_is_available = function() TRUE,
    arg <- loader_resolve(loader)
  )

  expect_called(m, 1L)
  expect_equal(arg, "rstudio")
  expect_args(m, 1, title = "TEST", prompt = "test prompt")
})

test_that("interactive loader works without rstudioapi", {
  loader <- list(interactive = list(prompt = "test prompt", secret = FALSE))
  loader <- new_loader(loader, "TEST")

  m <- mock("readline")
  mockr::with_mock(
    loader_resolve_interactive_secret = function(...) "bad secret",
    loader_resolve_interactive_rstudio = function(...) "bad rstudio",
    loader_resolve_interactive_readline = m,
    rstudioapi_is_available = function() FALSE,
    arg <- loader_resolve(loader)
  )

  expect_called(m, 1L)
  expect_equal(arg, "readline")
  expect_args(m, 1, prompt = "test prompt")
})

