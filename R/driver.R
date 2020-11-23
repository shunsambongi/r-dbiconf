methods::setClass("dbiconf_driver",
  contains = "DBIDriver",
  slots = c("driver" = "character"),
  prototype = list("driver" = NULL)
)

methods::setMethod("dbUnloadDriver", "dbiconf_driver", function(drv, ...) {
  TRUE
})

new_driver <- function(driver) {
  methods::new("dbiconf_driver", driver = driver)
}

methods::setMethod("dbConnect", "dbiconf_driver", function(drv, ...) {
  parts <- strsplit(drv@driver, "::", fixed = TRUE)[[1L]]
  drv <- getExportedValue(parts[1L], parts[2L])
  DBI::dbConnect(drv(), ...)
})
