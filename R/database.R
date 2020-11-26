
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
    drv <- format_toml(drv@driver)
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
    if (is.logical(arg)) {
      tolower(as.character(arg))
    } else if (is_loader(arg)) {
      format(arg)
    } else {
      rlang::as_label(arg)
    }
  })
  cat(paste0(nms, " = ", vals, collapse = "\n"))
})

database_args <- function(database) {
  purrr::map_if(database@.conn_args, rlang::is_function, rlang::exec)
}
