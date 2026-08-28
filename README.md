# sqlr.postgres

<!-- badges: start -->
<!-- badges: end -->

The PostgreSQL dialect for [sqlr](https://github.com/nbenn/sqlr). Renders a
sqlr schema as PostgreSQL data definition language, and reflects a live
database back into that representation.

``` r
library(sqlr)
library(sqlr.postgres)

schema <- sqlr_schema(
  "app",
  sqlr_table(
    "users",
    sqlr_column("id", sqlr_bigint(), identity = sqlr_identity()),
    sqlr_column("email", sqlr_varchar(255), null = FALSE),
    sqlr_primary_key("id"),
    sqlr_unique("email")
  )
)

sqlr_render(schema, postgres())
```

Given a connection, `sqlr_reflect()` reads the schema back out, and
`sqlr_equal()` compares the two.

## Installation

``` r
pak::pkg_install("nbenn/sqlr.postgres")
```
