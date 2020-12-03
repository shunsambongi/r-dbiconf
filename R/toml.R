format_toml_ <- function(x) {
  format_toml(x)
}

format_toml <- function(x) {
  UseMethod("format_toml")
}

format_toml.character <- function(x) {
  purrr::map_chr(x, rlang::as_label)
}

format_toml.numeric <- function(x) {
  format_toml_vec(as.character(x))
}

format_toml.logical <- function(x) {
  format_toml_vec(tolower(as.character(x)))
}

format_toml.Date <- function(x) {
  format_toml_vec(as.character(x))
}

format_toml.POSIXct <- function(x) {
  format_toml_vec(as.character(x))
}

format_toml.list <- function(x) {
  x <- purrr::map(x, ~format_toml(.x))
  if (rlang::is_named(x)) {
    x <- format_toml_inline_table(x)
  } else {
    x <- format_toml_array(x)
  }
  x
}

format_toml_vec <- function(x) {
  if (rlang::is_scalar_vector(x)) x else format_toml_array(x)
}

format_toml_array <- function(x) {
  paste("[", paste(x, collapse = ", "), "]")
}

format_toml_inline_table <- function(x) {
  keys <- format_toml_key(rlang::names2(x))
  paste0("{ ", paste0(keys, " = ", x, collapse = ", "), " }")
}

format_toml_key <- function(x) {
  stopifnot(is.character(x))
  matched <- grepl("^[A-Za-z0-9_-]+$", x)
  x[!matched] <- format_toml(x[!matched])
  x
}

format_toml.dbiconf_loader <- function(x) {
  format_toml(rlang::set_names(list(unclass(x)), attr(x, "type")))
}

format_toml.dbiconf_loader_wrapper <- function(x) {
  format_toml(attr(x, "loader"))
}
