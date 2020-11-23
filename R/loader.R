loader_types <- c("envvar", "file", "keyring", "interactive")

new_loader <- function(loader, name) {
  type <- rlang::arg_match0(
    rlang::names2(loader), values = loader_types, arg_nm = "loader_type"
  )
  value <- loader[[type]]
  class = c(paste0("dbiconf_loader_", type), "dbiconf_loader")
  structure(value, name = name, class = class)
}

load_arg <- function(loader) {
  UseMethod("load_arg")
}

load_arg_default <- function(loader, fn) {
  if (rlang::is_string(loader)) {
    fn(loader)
  } else {
    rlang::exec(fn, !!!loader)
  }
}

load_arg.dbiconf_loader_envvar <- function(loader) {
  load_arg_default(loader, Sys.getenv)
}

load_arg.dbiconf_loader_file <- function(loader) {
  assert_package("readr")
  trimws(load_arg_default(loader, readr::read_file))
}

load_arg.dbiconf_loader_keyring <- function(loader) {
  assert_package("keyring")
  load_arg_default(loader, keyring::key_get)
}

load_arg.dbiconf_loader_interactive <- function(loader) {
  assert_package("askpass")
  if (isTRUE(loader)) {
    loader <- attr(loader, "name")
  }
  load_arg_default(loader, askpass::askpass)
}
