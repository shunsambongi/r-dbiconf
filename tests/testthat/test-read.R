
test_that("valid.yaml passes validation", {
  path <- test_config_path("valid.toml")
  expect_error(validate_config(read_config(path)), NA)
})
