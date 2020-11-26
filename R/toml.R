format_toml_ <- function(x) {
  format_toml(x)
}

format_toml <- function(x) {
  UseMethod("format_toml")
}

format_toml.character <- function(x) {
  purrr::map_chr(x, rlang::as_label)
}

format_toml.integer <- function(x) {
  as.character(x)
}

format_toml.logical <- function(x) {
  tolower(as.character(x))
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
