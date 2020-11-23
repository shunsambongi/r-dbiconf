has_package <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

assert_package <- function(package) {
  if (!has_package(package)) {
    rlang::abort(
      paste0("Missing required package: ", package),
      "dbiconf_missing_package"
    )
  }
}
