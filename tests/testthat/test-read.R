
test_that("valid.yaml passes validation", {
  path <- test_config_path("valid.toml")
  expect_error(validate_config(read_config(path)), NA)
})

test_that("interactive.yaml passes validation", {
  path <- test_config_path("interactive.toml")
  expect_error(validate_config(read_config(path)), NA)
})

test_that("file.yaml passes validation", {
  path <- test_config_path("file.toml")
  expect_error(validate_config(read_config(path)), NA)
})

test_that("keyring.yaml passes validation", {
  path <- test_config_path("keyring.toml")
  expect_error(validate_config(read_config(path)), NA)
})
