test_that("types render under postgres spellings", {
  pg <- postgres()

  expect_equal(sqlr_render_type(sqlr_smallint(), pg), "smallint")
  expect_equal(sqlr_render_type(sqlr_bigint(), pg), "bigint")
  expect_equal(sqlr_render_type(sqlr_double(), pg), "double precision")
  expect_equal(sqlr_render_type(sqlr_numeric(10, 2), pg), "numeric(10, 2)")
  expect_equal(sqlr_render_type(sqlr_numeric(), pg), "numeric")
  expect_equal(sqlr_render_type(sqlr_varchar(255), pg), "character varying(255)")
  expect_equal(sqlr_render_type(sqlr_text(), pg), "text")
  expect_equal(sqlr_render_type(sqlr_json(binary = TRUE), pg), "jsonb")
  expect_equal(
    sqlr_render_type(sqlr_timestamp(with_timezone = TRUE), pg),
    "timestamp with time zone"
  )
})

test_that("an unmodelled type renders verbatim", {
  expect_equal(sqlr_render_type(sqlr_other("geometry"), postgres()), "geometry")
})

test_that("rendering needs no connection", {
  out <- sqlr_render(
    sqlr_table("t", sqlr_column("id", sqlr_bigint()), sqlr_primary_key("id")),
    postgres()
  )

  expect_match(out, "\"id\" bigint NOT NULL")
})
