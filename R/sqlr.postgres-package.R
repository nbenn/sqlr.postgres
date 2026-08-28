#' @import S7
#' @import sqlr
#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
  sqlr::sqlr_register_dialect("PqConnection", function(con) {
    postgres(version = server_version(con))
  })
}

server_version <- function(con) {
  version <- tryCatch(
    DBI::dbGetQuery(con, "SHOW server_version")[[1L]],
    error = function(e) NA_character_
  )
  as.character(version)
}
