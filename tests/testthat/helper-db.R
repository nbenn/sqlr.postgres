pg_con <- function() {
  skip_if_not_installed("RPostgres")

  con <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(),
      host = "localhost", port = 5432,
      user = "dbi", password = "dbi", dbname = "dbi"
    ),
    error = function(e) NULL
  )

  skip_if(is.null(con), "no postgres server on localhost:5432")
  con
}
