loader_types <- c("envvar", "file", "keyring", "interactive")

new_loader <- function(loader, name) {
  stopifnot(is_named_list(loader) && length(loader) == 1L)
  type <- rlang::names2(loader)
  value <- loader[[type]]
  class = c(paste0("dbiconf_loader_", type), "dbiconf_loader")
  structure(
    value,
    type = type,
    name = name,
    template = attr(loader, "template", exact = TRUE),
    class = class
  )
}

new_loader_wrapper <- function(loader, name) {
  loader <- new_loader(loader, name)
  wrapper <- function() {
    loader_resolve(loader)
  }
  structure(
    wrapper,
    loader = loader,
    template = attr(loader, "template", exact = TRUE),
    class = c("dbiconf_loader_wrapper", "dbiconf_loader")
  )
}

is_loader <- function(x) {
  inherits(x, "dbiconf_loader")
}

is_zap_loader <- function(x) {
  if (inherits(x, "dbiconf_loader_wrapper")) {
    x <- attr(x, "loader")
  }
  inherits(x, "dbiconf_loader_zap")
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

#' @export
print.dbiconf_loader_wrapper <- function(x, ...) {
  cat(sprintf("<dbiconf_loader_wrapper>\n%s", format(x)), "\n", sep = "")
  invisible(x)
}

#' Resolve loader value
#'
#' @param loader Loader object (dbiconf_loader) which will be resolved into an
#'   object.
#'
#' @export
loader_resolve <- function(loader) {
  UseMethod("loader_resolve")
}

loader_resolve_default <- function(loader, fn) {
  if (rlang::is_string(loader)) {
    fn(loader)
  } else {
    rlang::exec(fn, !!!loader)
  }
}
