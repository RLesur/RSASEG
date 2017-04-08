#' @import DBI
#' @import methods
#' @include SASEGS4.R
#' @include SASEGDataType.R
#' @include utils.R
NULL


#' Driver class for SAS Enterprise Guide.
#'
#' @keywords internal
#' @exportClass SASEGDriver
setClass("SASEGDriver", contains = "DBIDriver")

#' @rdname SASEGDriver-class
setMethod("dbUnloadDriver", "SASEGDriver", function(drv, ...) {
  TRUE
})

setMethod("show", "SASEGDriver", function(object) {
  cat("<SASEGDriver>\n")
})

# SAS EG Class
#' @export
SASEG <- function() {
  new("SASEGDriver")
}


#' SAS class.
#'
#' @exportClass SAS
setClass("SAS", contains = "character")

#' @exportMethod SAS
setGeneric("SAS", function(x, ...) standardGeneric("SAS"))
#' @export
setMethod("SAS", "character", function(x, ...) {
  new("SAS", x)
})
#' @export
setMethod("SAS", "SAS", function(x, ...) {
  return(x)
})
#' @export
setMethod("SAS", "SQL", function(x, ...) {
  new("SAS",
      paste0("PROC SQL DQUOTE=ANSI;\n",
             "ods output SQL_Results=SQLOut;\n",
             x, ";\n",
             "QUIT;\n"))
})

setMethod("show", "SAS", function(object) {
  cat(paste0("<SAS> ", object@.Data, collapse = "\n"))
})


#' SAS EG connection class.
#'
#' @exportClass SASEGConnection
#' @keywords internal
setClass("SASEGConnection",
         contains = "DBIConnection",
         slots = list(
           profile = "character",
           server = "character",
           # Slot application is a SAS EG Application object
           application = "SASEGApplication",
           # Slot SASProject is a SAS EG Project object
           SASProject = "SASEGProject",
           SASUtil = "SASEGCode",
           dbms = "character"
         )
)

#' @param drv An object created by \code{SASEG()}
#' @rdname SASEGDriver-class
#' @export
#' @examples
#' \dontrun{
#' db <- dbConnect(RSASEG::SASEG(), DLLFilePath, profile, server, dbms)
#' dbWriteTable(db, "mtcars", mtcars)
#' dbGetQuery(db, "SELECT * FROM mtcars WHERE cyl == 4")
#' }
setMethod("dbConnect", "SASEGDriver", function(drv, DLLFilePath, profile, server, dbms, ...) {
  # Load SAS.EG.Scripting namespace:
  loadSASEGScripting(DLLFilePath)
  # Create a new SAS EG Application object:
  application <- SASEGApplication()
  # Set profile:
  setProfile(application, profile)
  # Create a new SAS EG Project object:
  SASProject <- newProject(application)
  # Create a new SAS EG Code project to run utils tasks (e.g. fetch programs):
  SASUtil <- newCode(project = SASProject, server = server, program = noteUtil, name = "garbage")
  dbms <- NULL
  class(dbms) <- "character"
  new("SASEGConnection", profile = profile, server = server, application = application, SASProject = SASProject, SASUtil = SASUtil, dbms = dbms)
})

setMethod("show", "SASEGConnection", function(object) {
  show(object@application)
  cat(
    "Used Profile in Active Connection: ", object@profile, "\n",
    "SAS Server to Run Programs in Active Connection: ", object@server, "\n",
    "DBMS SQL Pass-Through: ", if(length(object@dbms) == 0) "NONE" else object@dbms,
    sep = ""
    )
})

setMethod("dbDisconnect", "SASEGConnection", function(conn, projectPath = NULL, ...) {
  if(!is.null(projectPath))  saveAs(conn@SASProject, projectPath)
  terminate(conn@application)
  return(TRUE)
  })


#' SASEG results class.
#'
#' @keywords internal
#' @exportClass SASEGResult
setClass("SASEGResult",
         contains = "DBIResult",
         slots = list(SASResult = "SASEGCode", SASUtil = "SASEGCode", fetched = "function", rowsFetched = "function")
         )

#' @exportMethod setFetched
setGeneric("setFetched", function(res, value) standardGeneric("setFetched"))
setMethod("setFetched", "SASEGResult", function(res, value) {
  res@fetched(set = value)
})

setMethod("dbHasCompleted", "SASEGResult", function(res, ...) {
  res@fetched()
})


#' Send a query to SAS EG.
#'
#' @export
#' @examples
#' # This is another good place to put examples
setMethod("dbSendQuery", "SASEGConnection", function(conn, statement, codeName = NULL, ...) {
  if(class(statement) %in% c("SAS", "SQL")) {
    program <- SAS(statement)
    # Create a new SAS EG Code object with server and SAS program:
    SASCode <- newCode(conn@SASProject, server = conn@server, program = program, name = codeName)
    # Execute SAS program:
    run(SASCode)
    # Only for debugging:
    #cat(getSourceCode(SASCode))
    res <- new("SASEGResult",
               SASResult = SASCode,
               SASUtil = conn@SASUtil,
               fetched = state_generator(),
               rowsFetched = count_generator())
    if(countOutputDatasets(SASCode) == 0) setFetched(res, value = TRUE)
    return(res)
  } else {
    message("Statement:\n", statement,
            "\nClass: ", class(statement),
            "\nNo query sent to SAS: the query must be an SQL or a SAS class object.",
            "\nTry SAS(", statement, ") or\n",
            "SQL(", statement, ")")
    return(NULL)
    }
})


#' @export
setMethod("dbClearResult", "SASEGResult", function(res, ...) {
  l <- getListDatasets(res@SASResult)
  drop_char <- paste("DROP TABLE", lapply(l, getFileName))
  drop_sql <- lapply(drop_char, SQL)
  drop_sas <- paste(noteUtil, lapply(drop_sql, SAS))
  SASUtil <- res@SASUtil
  i <- 1
  while(i <= length(drop_sas)) {
    setText(SASUtil, drop_sas[[i]])
    run(SASUtil)
    i <- i+1
  }
  return(TRUE)
})


#' Retrieve records from SAS EG query
#' @export
setMethod("dbFetch", "SASEGResult", function(res, n = -1, ...) {
  l <- getListDatasets(res@SASResult)
  if(length(l)>1) stop("Query results contain multiple datasets; cannot execute dbFetch() method.\n  Please run:\n  res <- dbSendQuery(statement)\n  datalist <- dbFetchAll(res)")
  l <- l[[1]]
  path <- saveAs(object = l)
  d <- data.table::fread(path)
  setFetched(res = res, value = TRUE)
  return(d)
})

#' @exportMethod dbFetchAll
setGeneric("dbFetchAll", function(res, ...) standardGeneric("dbFetchAll"))
#' @export
setMethod("dbFetchAll", "SASEGResult", function(res, ...) {
  l <- getListDatasets(res@SASResult)
  n <- length(l)
  d <- vector("list", n)
  i <- 1
  while(i <= length(l)) {
    path <- saveAs(object = l[[i]])
    d[[i]] <- data.table::fread(path)
    i <- i+1
  }
  setFetched(res = res, value = TRUE)
  if(n == 1) {d <- d[[1]]}
  return(d)
})

#' @exportMethod dbGetLog
setGeneric("dbGetLog", function(res, ...) standardGeneric("dbGetLog"))
#' @export
setMethod("dbGetLog", "SASEGResult", function(res, ...) {
  getLog(res@SASResult)
})

setMethod("dbDataType", "SASEGDriver", function(dbObj, obj, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  if(class(obj) %in% names(SASEGDataType)) {
    return(SASEGDataType[[class(obj)]])
  } else {
    return(dbDataType(DBI::ANSI(), obj, ...))
  }
})

setMethod("dbDataType", "SASEGConnection", function(dbObj, obj, ...) {
  dbDataType(SASEG(), obj)
})

setMethod("dbQuoteString", c("SASEGConnection", "character"), function(conn, x, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  dbQuoteString(DBI::ANSI(), x, ...)
})

setMethod("dbQuoteString", c("SASEGConnection", "SQL"), function(conn, x, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  dbQuoteString(DBI::ANSI(), x, ...)
})

setMethod("dbQuoteIdentifier", c("SASEGConnection", "character"), function(conn, x, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  dbQuoteIdentifier(DBI::ANSI(), x, ...)
})

setMethod("dbQuoteIdentifier", c("SASEGConnection", "SQL"), function(conn, x, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  dbQuoteIdentifier(DBI::ANSI(), x, ...)
})

setMethod("dbQuoteIdentifier", c("SASEGConnection", "Table"), function(conn, x, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  dbQuoteIdentifier(DBI::ANSI(), x, ...)
})

setMethod("sqlAppendTable", "SASEGConnection", function(con, table, values, row.names = NA, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  code <- sqlAppendTable(DBI::ANSI(), table, values, row.names, ...)
  # SAS PROC SQL INSERT INTO statement has its own syntax. 
  # One row is inserted for each VALUES clause.
  # Multiple VALUES clauses are not separated by commas.
  code <- gsub("),\n  (", ")\nVALUES\n  (", code, fixed = TRUE)
  code <- gsub("VALUES\n  ", "  VALUES", code, fixed = TRUE)
  return(code)
})

setMethod("dbWriteTable", "SASEGConnection", function(conn, name, value, row.names = NA, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  program <- SAS(SQL(paste0(
    sqlCreateTable(con = conn, table = name, fields = value, row.names = row.names, temporary = FALSE),
    ";\n",
    sqlAppendTable(con = conn, table = name, values = value, row.names = row.names, ...)
    )))
  # Create a new SAS EG Code object with server and SAS program:
  SASCode <- newCode(conn@SASProject, 
                     server = conn@server, 
                     program = program, 
                     name = paste("Create dataset", name, "from", deparse(substitute(value)))
                     )
  run(SASCode)
})


