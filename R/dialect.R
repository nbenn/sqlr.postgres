#' The PostgreSQL dialect
#'
#' Passed to [sqlr::sqlr_render()] to emit PostgreSQL, and resolved
#' automatically from a `PqConnection` by [sqlr::sqlr_for()]. Building one by
#' hand needs no connection and no driver package, so rendering can be tested
#' without a server.
#'
#' @param version Server version string, as reported by `SHOW server_version`.
#'   Governs version-dependent rendering; unset means assume current.
#' @param attrs Named list of dialect-specific attributes.
#'
#' @return A `postgres` dialect object.
#'
#' @examples
#' sqlr::sqlr_render(
#'   sqlr::sqlr_table("t", sqlr::sqlr_column("id", sqlr::sqlr_bigint())),
#'   postgres()
#' )
#'
#' @export
postgres <- new_class("postgres", parent = sqlr_dialect)

supports_identity <- function(dialect) {
  is.na(dialect@version) ||
    as.numeric(sub("^([0-9]+).*$", "\\1", dialect@version)) >= 10
}
