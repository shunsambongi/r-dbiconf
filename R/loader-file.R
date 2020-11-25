load_arg.dbiconf_loader_file <- function(loader) {
  assert_package("brio")
  if (is.list(loader)) {
    trim <- purrr::pluck(loader, "trim", .default = TRUE)
    loader <- list(path = purrr::chuck(loader, "path"))
  }

  out <- load_arg_default(loader, brio::read_file)

  if (isTRUE(trim)) {
    out <- trimws(out)
  }

  out
}
