#' @import DBI
#' @import methods
#' @import dplyr
#' @include RSASEGDriver.R
NULL

src_saseg <- function(DLLFilePath = NULL, profile = NULL, server = NULL, dbms = NULL, ...) {

  con <- dbConnect(SASEG(), DLLFilePath, profile, server, dbms, ...)

  src_sql("saseg", con)
}


#' @export
src_desc.src_saseg <- function(con) {
  show(con)
}

tbl.src_saseg <- function(src, from, ...) {
  tbl_sql("saseg", src = src, from = from, ...)
}

