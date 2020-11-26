has_package <- function(package) {
  out <- requireNamespace(package, quietly = TRUE)
  out
}

assert_package <- function(package) {
  if (!has_package(package)) {
    rlang::abort(
      paste0("Missing required package: ", package),
      "dbiconf_missing_package"
    )
  }
}

rstudioapi_is_available <- function() {
  has_package("rstudioapi") && rstudioapi::isAvailable()
}

named_list <- function() {
  rlang::set_names(list(), character())
}
