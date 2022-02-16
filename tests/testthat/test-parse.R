test_that("parse errors become custom errors", {
    testthat::expect_error(
        parse_database("bad", "bad", "bad"),
        class = "dbiconf_parse_error"
    )
})
