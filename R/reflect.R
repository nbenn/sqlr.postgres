method(sqlr_reflect_schema, postgres) <-
  function(dialect, con, schema = NULL, ...) {
    if (is.null(schema)) {
      schema <- DBI::dbGetQuery(con, "SELECT current_schema() AS s")$s
    }

    columns <- query_columns(con, schema)
    constraints <- query_constraints(con, schema)
    indexes <- query_indexes(con, schema)

    names <- sort(unique(columns$table_name))
    tables <- lapply(names, function(nm) {
      build_table(
        nm,
        columns[columns$table_name == nm, , drop = FALSE],
        constraints[constraints$table_name == nm, , drop = FALSE],
        indexes[indexes$table_name == nm, , drop = FALSE]
      )
    })

    do.call(sqlr_schema, c(list(name = schema), tables))
  }

build_table <- function(name, columns, constraints, indexes) {
  parts <- c(
    lapply(seq_len(nrow(columns)), function(i) build_column(columns[i, ])),
    build_constraints(constraints),
    build_indexes(indexes)
  )

  do.call(sqlr_table, c(list(name = name), parts))
}

build_column <- function(row) {
  identity <- NULL
  default <- parse_default(row$column_default)

  if (identical(row$is_identity, "YES")) {
    identity <- sqlr_identity(
      generated = if (identical(row$identity_generation, "ALWAYS")) {
        "always"
      } else {
        "by default"
      },
      start = as_int(row$identity_start),
      increment = as_int(row$identity_increment)
    )
    default <- NULL
  } else if (is_sequence_default(row$column_default)) {
    identity <- sqlr_identity()
    default <- NULL
  }

  sqlr_column(
    name = row$column_name,
    type = type_from_catalogue(row),
    null = identical(row$is_nullable, "YES"),
    default = default,
    identity = identity
  )
}

build_constraints <- function(rows) {
  if (!nrow(rows)) {
    return(list())
  }

  keys <- paste(rows$conname, rows$contype)
  lapply(unique(keys), function(key) {
    build_constraint(rows[keys == key, , drop = FALSE])
  })
}

build_constraint <- function(rows) {
  first <- rows[1L, ]
  columns <- rows$column_name[!is.na(rows$column_name)]

  switch(
    first$contype,
    "p" = sqlr_primary_key(columns, name = first$conname),
    "u" = sqlr_unique(columns, name = first$conname),
    "f" = sqlr_foreign_key(
      columns = columns,
      ref_table = first$ref_table,
      ref_columns = rows$ref_column[!is.na(rows$ref_column)],
      on_delete = referential_action(first$confdeltype),
      on_update = referential_action(first$confupdtype),
      name = first$conname
    ),
    sqlr_check(first$check_expr, name = first$conname)
  )
}

referential_action <- function(code) {
  c(
    a = "no action", r = "restrict", c = "cascade",
    n = "set null", d = "set default"
  )[[code]]
}

build_indexes <- function(rows) {
  if (!nrow(rows)) {
    return(list())
  }

  lapply(unique(rows$index_name), function(nm) {
    idx <- rows[rows$index_name == nm, , drop = FALSE]
    sqlr_index(
      columns = idx$column_name,
      name = nm,
      desc = idx$is_desc,
      unique = idx$indisunique[[1L]]
    )
  })
}

query_columns <- function(con, schema) {
  DBI::dbGetQuery(
    con,
    "SELECT c.table_name, c.column_name, c.ordinal_position, c.is_nullable,
            c.column_default, c.data_type, c.character_maximum_length,
            c.numeric_precision, c.numeric_scale, c.datetime_precision,
            c.is_identity, c.identity_generation, c.identity_start,
            c.identity_increment, c.udt_name
       FROM information_schema.columns c
       JOIN information_schema.tables t
         ON t.table_schema = c.table_schema AND t.table_name = c.table_name
      WHERE c.table_schema = $1 AND t.table_type = 'BASE TABLE'
      ORDER BY c.table_name, c.ordinal_position",
    params = list(schema)
  )
}

query_constraints <- function(con, schema) {
  DBI::dbGetQuery(
    con,
    "SELECT rel.relname AS table_name, con.conname, con.contype,
            con.confdeltype, con.confupdtype, fref.relname AS ref_table,
            k.ord, a.attname AS column_name, fa.attname AS ref_column,
            pg_get_expr(con.conbin, con.conrelid) AS check_expr
       FROM pg_constraint con
       JOIN pg_class rel ON rel.oid = con.conrelid
       JOIN pg_namespace ns ON ns.oid = rel.relnamespace
       LEFT JOIN pg_class fref ON fref.oid = con.confrelid
       LEFT JOIN LATERAL unnest(con.conkey) WITH ORDINALITY AS k(attnum, ord)
              ON TRUE
       LEFT JOIN pg_attribute a
              ON a.attrelid = con.conrelid AND a.attnum = k.attnum
       LEFT JOIN LATERAL unnest(con.confkey) WITH ORDINALITY AS fk(attnum, ord)
              ON fk.ord = k.ord
       LEFT JOIN pg_attribute fa
              ON fa.attrelid = con.confrelid AND fa.attnum = fk.attnum
      WHERE ns.nspname = $1
        AND con.contype IN ('p', 'u', 'f', 'c')
        AND rel.relkind = 'r'
      ORDER BY rel.relname, con.conname, k.ord",
    params = list(schema)
  )
}

query_indexes <- function(con, schema) {
  DBI::dbGetQuery(
    con,
    "SELECT rel.relname AS table_name, cls.relname AS index_name,
            idx.indisunique, k.ord, a.attname AS column_name,
            pg_index_column_has_property(idx.indexrelid, k.ord::int, 'desc')
              AS is_desc
       FROM pg_index idx
       JOIN pg_class cls ON cls.oid = idx.indexrelid
       JOIN pg_class rel ON rel.oid = idx.indrelid
       JOIN pg_namespace ns ON ns.oid = rel.relnamespace
       LEFT JOIN LATERAL unnest(idx.indkey) WITH ORDINALITY AS k(attnum, ord)
              ON TRUE
       LEFT JOIN pg_attribute a
              ON a.attrelid = idx.indrelid AND a.attnum = k.attnum
      WHERE ns.nspname = $1
        AND NOT EXISTS (
              SELECT 1 FROM pg_constraint c WHERE c.conindid = idx.indexrelid
            )
      ORDER BY rel.relname, cls.relname, k.ord",
    params = list(schema)
  )
}
