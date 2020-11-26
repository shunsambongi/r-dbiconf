load_arg.dbiconf_loader_interactive <- function(loader) {
  name <- attr(loader, "name")
  if (isTRUE(loader)) {
    loader <- list(prompt = paste0(name, ": "))
  }
  prompt <- purrr::chuck(loader, "prompt")
  secret <- purrr::pluck(loader, "secret", .default = TRUE)
  load_arg_interactive(name, prompt, secret)
}


load_arg_interactive <- function(name, prompt, secret) {
  if (isTRUE(secret)) {
    load_arg_interactive_secret(prompt)
  } else if (rstudioapi_is_available()) {
    load_arg_interactive_rstudio(title = name, prompt = prompt)
  } else {
    load_arg_interactive_readline(prompt)
  }
}

load_arg_interactive_secret <- function(prompt) {
  assert_package("askpass")
  load_arg_default(prompt, askpass::askpass)
}

load_arg_interactive_rstudio <- function(title, prompt) {
  rstudioapi::showPrompt(title, prompt)
}

load_arg_interactive_readline <- function(prompt) {
  readline(prompt)
}
