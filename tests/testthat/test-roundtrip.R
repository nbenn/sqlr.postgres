test_that("a rendered schema executes and reflects back identically", {
  con <- pg_con()
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "DROP SCHEMA IF EXISTS sqlr_test CASCADE")
  DBI::dbExecute(con, "CREATE SCHEMA sqlr_test")
  on.exit(
    DBI::dbExecute(con, "DROP SCHEMA IF EXISTS sqlr_test CASCADE"),
    add = TRUE, after = FALSE
  )

  desired <- sqlr_schema(
    "sqlr_test",
    sqlr_table(
      "users",
      sqlr_column("id", sqlr_bigint(), identity = sqlr_identity()),
      sqlr_column("email", sqlr_varchar(255), null = FALSE),
      sqlr_column("nickname", sqlr_text(), default = "anon"),
      sqlr_column("active", sqlr_boolean(), default = TRUE),
      sqlr_column("score", sqlr_numeric(10, 2), default = 0L),
      sqlr_column("created", sqlr_timestamp(with_timezone = TRUE),
                  default = sqlr_sql("now()")),
      sqlr_column("meta", sqlr_json(binary = TRUE)),
      sqlr_column("manager_id", sqlr_bigint()),
      sqlr_primary_key("id", name = "users_pkey"),
      sqlr_unique("email", name = "users_email_key"),
      sqlr_foreign_key("manager_id", "users", "id", name = "users_manager_fkey")
    ),
    sqlr_table(
      "orders",
      sqlr_column("id", sqlr_bigint(), identity = sqlr_identity()),
      sqlr_column("user_id", sqlr_bigint(), null = FALSE),
      sqlr_column("total", sqlr_numeric(10, 2)),
      sqlr_column("placed", sqlr_date()),
      sqlr_primary_key("id", name = "orders_pkey"),
      sqlr_foreign_key("user_id", "users", "id", on_delete = "cascade",
                       name = "orders_user_fkey"),
      sqlr_check("total > 0", name = "orders_total_check"),
      sqlr_index("user_id", name = "orders_user_idx")
    )
  )

  for (stmt in sqlr_render(desired, postgres())) {
    expect_error(DBI::dbExecute(con, stmt), NA)
  }

  expect_equal(sqlr_diff(desired, sqlr_reflect(con, "sqlr_test")), character())
})

test_that("serial columns reflect as identity", {
  con <- pg_con()
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "DROP SCHEMA IF EXISTS sqlr_serial CASCADE")
  DBI::dbExecute(con, "CREATE SCHEMA sqlr_serial")
  on.exit(
    DBI::dbExecute(con, "DROP SCHEMA IF EXISTS sqlr_serial CASCADE"),
    add = TRUE, after = FALSE
  )
  DBI::dbExecute(con, "CREATE TABLE sqlr_serial.t (id serial PRIMARY KEY)")

  column <- sqlr_reflect(con, "sqlr_serial")@tables[[1L]]@columns[[1L]]

  expect_false(is.null(column@identity))
  expect_null(column@default)
  expect_equal(column@type@bytes, 4L)
})

test_that("constraint-backed indexes are not reported as indexes", {
  con <- pg_con()
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "DROP SCHEMA IF EXISTS sqlr_idx CASCADE")
  DBI::dbExecute(con, "CREATE SCHEMA sqlr_idx")
  on.exit(
    DBI::dbExecute(con, "DROP SCHEMA IF EXISTS sqlr_idx CASCADE"),
    add = TRUE, after = FALSE
  )
  DBI::dbExecute(
    con,
    "CREATE TABLE sqlr_idx.t (id bigint PRIMARY KEY, email text UNIQUE)"
  )

  tbl <- sqlr_reflect(con, "sqlr_idx")@tables[[1L]]

  expect_length(tbl@indexes, 0L)
  expect_length(tbl@constraints, 2L)
})

test_that("an engine-named schema still compares equal to an authored one", {
  con <- pg_con()
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "DROP SCHEMA IF EXISTS sqlr_names CASCADE")
  DBI::dbExecute(con, "CREATE SCHEMA sqlr_names")
  on.exit(
    DBI::dbExecute(con, "DROP SCHEMA IF EXISTS sqlr_names CASCADE"),
    add = TRUE, after = FALSE
  )
  DBI::dbExecute(
    con,
    "CREATE TABLE sqlr_names.t (id bigint PRIMARY KEY, email text UNIQUE)"
  )

  authored <- sqlr_schema(
    "sqlr_names",
    sqlr_table(
      "t",
      sqlr_column("id", sqlr_bigint()),
      sqlr_column("email", sqlr_text()),
      sqlr_primary_key("id", name = "my_own_pk_name"),
      sqlr_unique("email", name = "my_own_unique_name")
    )
  )

  expect_true(sqlr_equal(authored, sqlr_reflect(con, "sqlr_names")))
})
