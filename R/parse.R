parse_database <- function(database, name, templates) {
  tryCatch({
    template <- get_template(database, templates)
    database <- merge_params(database, template)
    driver <- get_driver(database)
    conn_args <- get_conn_args(database)
    methods::new(
      "dbiconf_database", .name = name, .drv = driver, .conn_args = conn_args
    )
  }, error = function(e) {
    message <- paste0(
      "Error while parsing database: `", name, "`\n", conditionMessage(e)
    )
    rlang::abort(
      message, "dbiconf_parse_error",
      parent = NA, call = conditionCall(e), trace = e$trace
    )
  })
}

get_driver <- function(database) {
  driver <- purrr::pluck(database, "_driver")
  if (is.null(driver)) {
    rlang::abort("Missing required parameter `_driver`.")
  }
  new_driver(driver)
}

get_template <- function(database, templates) {
  name <- purrr::pluck(database, "_template")
  if (is.null(name)) {
    return(list())
  }
  if (!rlang::has_name(templates, name)) {
    rlang::abort(
      paste0("Invalid template specified: ", name),
      "dbiconf_unknown_template"
    )
  }
  purrr::chuck(templates, name)
}

merge_params <- function(database, template) {
    rlang::dots_list(!!!template, !!!database, .homonyms = "last")
}

get_conn_args <- function(database) {
  database[["_template"]] <- rlang::zap()
  database[["_driver"]] <- rlang::zap()
  args <- purrr::imap(database, function(arg, name) {
    if (!rlang::is_bare_list(arg)) {
      return(arg)
    }
    new_loader_wrapper(arg, name)
  })
  purrr::discard(args, ~rlang::is_zap(.x) || is_zap_loader(.x))
}
