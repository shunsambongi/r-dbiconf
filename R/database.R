
methods::setClass("dbiconf_database",
  contains = "DBIConnector",
  slots = c(".name" = "character"),
  prototype = list(".name" = NA_character_)
)

methods::setValidity("dbiconf_database", function(object) {
  if (!rlang::is_string(object@.name)) {
    "@.name must be a string"
  } else {
    TRUE
  }
})

methods::setMethod("show", "dbiconf_database", function(object) {
  cat(sprintf("[database.%s]\n", format_toml_key(object@.name)))

  drv <- object@.drv
  if (methods::is(drv, "dbiconf_driver")) {
    template <- attr(drv@driver, "template", exact = TRUE)
    drv <- format_toml(drv@driver)
    if (!is.null(template)) {
      template <- format_toml_key(template)
      drv <- paste(drv, crayon::silver(sprintf("[template.%s]", template)))
    }
  } else {
    drv <- format(drv)
  }
  cat("_driver = ", drv, "\n", sep = "")

  args <- object@.conn_args
  if (rlang::is_empty(args)) {
    return()
  }
  nms <- rlang::names2(args)
  vals <- purrr::map(args, function(arg) {
    out <- format_toml(arg)
    template <- attr(arg, "template", exact = TRUE)
    if (!is.null(template)) {
      template <- format_toml_key(template)
      out <- paste(out, crayon::silver(sprintf("[template.%s]", template)))
    }
    out
  })
  cat(paste0(nms, " = ", vals, collapse = "\n"), "\n", sep = "")
})

database_args <- function(database) {
  purrr::map_if(database@.conn_args, rlang::is_function, rlang::exec)
}
