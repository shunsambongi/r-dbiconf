#' @export
loader_resolve.dbiconf_loader_interactive <- function(loader) {
  name <- attr(loader, "name")
  if (isTRUE(loader)) {
    loader <- list(prompt = paste0(name, ": "))
  }
  prompt <- purrr::chuck(loader, "prompt")
  secret <- purrr::pluck(loader, "secret", .default = TRUE)
  loader_resolve_interactive(name, prompt, secret)
}


loader_resolve_interactive <- function(name, prompt, secret) {
  if (isTRUE(secret)) {
    loader_resolve_interactive_secret(prompt)
  } else if (rstudioapi_is_available()) {
    loader_resolve_interactive_rstudio(title = name, prompt = prompt)
  } else {
    loader_resolve_interactive_readline(prompt)
  }
}

loader_resolve_interactive_secret <- function(prompt) {
  assert_package("askpass")
  loader_resolve_default(prompt, askpass::askpass)
}

loader_resolve_interactive_rstudio <- function(title, prompt) {
  rstudioapi::showPrompt(title, prompt)
}

loader_resolve_interactive_readline <- function(prompt) {
  readline(prompt)
}
