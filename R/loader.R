loader_types <- c("envvar", "file", "keyring", "interactive")

new_loader <- function(loader, name) {
  type <- rlang::arg_match0(
    rlang::names2(loader), values = loader_types, arg_nm = "loader_type"
  )
  value <- loader[[type]]
  class = c(paste0("dbiconf_loader_", type), "dbiconf_loader")
  structure(value, type = type, name = name, class = class)
}

new_loader_wrapper <- function(loader, name) {
  loader <- new_loader(loader, name)
  out <- function() {
    load_arg(loader)
  }
  structure(out, loader = loader, class = c("dbiconf_loader_wrapper", "dbiconf_loader"))
}

is_loader <- function(x) {
  inherits(x, "dbiconf_loader")
}

#' @export
format.dbiconf_loader <- function(x, ...) {
  sprintf("<loader: %s>", attr(x, "type"))
}

#' @export
format.dbiconf_loader_wrapper <- function(x, ...) {
  format(attr(x, "loader"), ...)
}

#' @export
print.dbiconf_loader <- function(x, ...) {
  cat(sprintf("<dbiconf_loader>\n%s", format(x)), "\n", sep = "")
  invisible(x)
}

load_arg <- function(loader) {
  UseMethod("load_arg")
}

load_arg_ <- function(loader) {
  load_arg(loader)
}

load_arg_default <- function(loader, fn) {
  if (rlang::is_string(loader)) {
    fn(loader)
  } else {
    rlang::exec(fn, !!!loader)
  }
}
