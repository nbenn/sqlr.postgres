method(sqlr_render_type, list(sqlr_integer_type, postgres)) <-
  function(type, dialect, ...) {
    switch(
      as.character(type@bytes),
      "2" = "smallint",
      "4" = "integer",
      "8" = "bigint",
      stop("postgres has no ", type@bytes, "-byte integer", call. = FALSE)
    )
  }

method(sqlr_render_type, list(sqlr_float_type, postgres)) <-
  function(type, dialect, ...) {
    if (type@bytes == 4L) "real" else "double precision"
  }

method(sqlr_render_type, list(sqlr_decimal_type, postgres)) <-
  function(type, dialect, ...) {
    if (is.na(type@precision)) {
      "numeric"
    } else if (is.na(type@scale)) {
      paste0("numeric(", type@precision, ")")
    } else {
      paste0("numeric(", type@precision, ", ", type@scale, ")")
    }
  }

method(sqlr_render_type, list(sqlr_string_type, postgres)) <-
  function(type, dialect, ...) {
    if (is.na(type@size)) {
      if (type@fixed) "character" else "text"
    } else if (type@fixed) {
      paste0("character(", type@size, ")")
    } else {
      paste0("character varying(", type@size, ")")
    }
  }

method(sqlr_render_type, list(sqlr_binary_type, postgres)) <-
  function(type, dialect, ...) "bytea"

method(sqlr_render_type, list(sqlr_boolean_type, postgres)) <-
  function(type, dialect, ...) "boolean"

method(sqlr_render_type, list(sqlr_time_type, postgres)) <-
  function(type, dialect, ...) {
    if (type@kind == "date") {
      return("date")
    }

    precision <- if (is.na(type@precision)) "" else paste0("(", type@precision, ")")
    zone <- if (type@with_timezone) " with time zone" else " without time zone"

    paste0(type@kind, precision, zone)
  }

method(sqlr_render_type, list(sqlr_json_type, postgres)) <-
  function(type, dialect, ...) if (type@binary) "jsonb" else "json"

method(sqlr_render_type, list(sqlr_uuid_type, postgres)) <-
  function(type, dialect, ...) "uuid"

method(sqlr_render_type, list(sqlr_other_type, postgres)) <-
  function(type, dialect, ...) type@name
