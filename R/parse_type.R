default_datetime_precision <- 6L

type_from_catalogue <- function(row) {
  size <- as_int(row$character_maximum_length)
  precision <- as_int(row$numeric_precision)
  scale <- as_int(row$numeric_scale)
  clock <- datetime_precision(row$datetime_precision)
  raw <- row$data_type

  switch(
    row$data_type,
    "smallint" = sqlr_smallint(raw = raw),
    "integer" = sqlr_int(raw = raw),
    "bigint" = sqlr_bigint(raw = raw),
    "real" = sqlr_real(raw = raw),
    "double precision" = sqlr_double(raw = raw),
    "numeric" = sqlr_numeric(precision, scale, raw = raw),
    "character varying" = sqlr_varchar(size, raw = raw),
    "character" = sqlr_char(size, raw = raw),
    "text" = sqlr_text(raw = raw),
    "bytea" = sqlr_blob(raw = raw),
    "boolean" = sqlr_boolean(raw = raw),
    "date" = sqlr_date(raw = raw),
    "time without time zone" = sqlr_time(FALSE, clock, raw = raw),
    "time with time zone" = sqlr_time(TRUE, clock, raw = raw),
    "timestamp without time zone" = sqlr_timestamp(FALSE, clock, raw = raw),
    "timestamp with time zone" = sqlr_timestamp(TRUE, clock, raw = raw),
    "json" = sqlr_json(FALSE, raw = raw),
    "jsonb" = sqlr_json(TRUE, raw = raw),
    "uuid" = sqlr_uuid(raw = raw),
    sqlr_other(if (is.na(row$udt_name)) raw else row$udt_name, raw = raw)
  )
}

datetime_precision <- function(x) {
  out <- as_int(x)
  if (!is.na(out) && out == default_datetime_precision) NA_integer_ else out
}

as_int <- function(x) {
  if (is.null(x) || length(x) != 1L) NA_integer_ else as.integer(x)
}

parse_default <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) {
    return(NULL)
  }

  stripped <- strip_casts(x)

  if (grepl("^'.*'$", stripped)) {
    return(gsub("''", "'", substr(stripped, 2L, nchar(stripped) - 1L), fixed = TRUE))
  }

  if (stripped %in% c("true", "false")) {
    return(stripped == "true")
  }

  if (grepl("^-?[0-9]+$", stripped)) {
    return(as.integer(stripped))
  }

  if (grepl("^-?[0-9]*\\.[0-9]+$", stripped)) {
    return(as.numeric(stripped))
  }

  sqlr_sql(text = x)
}

strip_casts <- function(x) {
  repeat {
    out <- sub("::[a-zA-Z_][a-zA-Z0-9_ ]*(\\[\\])?$", "", x)
    if (identical(out, x)) {
      return(trimws(out))
    }
    x <- out
  }
}

is_sequence_default <- function(x) {
  !is.null(x) && !is.na(x) && grepl("^nextval\\(", x)
}
