#' Read connectors
#'
#' @param file Path to configuration file.
#' @return A named list of DBIConnectors specified in the configuration file.
#' @export
read_connectors <- function(file) {
  config <- read_config(file)
  validate_config(config)
  databases <- purrr::pluck(config, "database", .default = list())
  templates <- purrr::pluck(config, "template", .default = list())
  purrr::imap(databases, parse_database, templates)
}

read_config <- function(file) {
  RcppTOML::parseTOML(input = file, fromFile = TRUE, escape = FALSE)
}

validate_config <- function(config) {
  json <- jsonlite::toJSON(config, auto_unbox = TRUE)
  jsonvalidate::json_validate(
    json = json,
    schema = fs::path_package("dbiconf", "schema/schema.json"),
    error = TRUE,
    greedy = TRUE,
    engine = "ajv"
  )
  invisible(config)
}
