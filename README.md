
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dbiconf

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

This package allows you to save DBI database parameters in a
configuration file.

## Example configuration file

Configuration is written in TOML:

``` toml
# example.toml

[database.my_database]
_driver = "odbc::odbc"
server = "my.database.com"
database = "MyDatabase"
uid = "bob_loblaw"
pwd = "password123"
```

How to use it:

``` r
db <- read_databases("example.toml")
conn <- dbConnect(db$my_database)
```
