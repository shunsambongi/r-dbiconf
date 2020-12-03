#' @export
loader_resolve.dbiconf_loader_envvar <- function(loader) {
  out <- loader_resolve_default(loader, Sys.getenv)
  if (identical(out, "")) {
    envvar <- if (rlang::is_string(loader)) {
      loader
    } else {
      loader[["x"]]
    }
    rlang::abort(sprintf("Environment variable %s is unset", envvar))
  }
  out
}
