#' @import DBI
#' @import methods
#' @include SASEGS4.R
#' @include SASEGDataType.R
#' @include utils.R
NULL


# Driver Class ------------------------------------------------------------


#' Driver class for SAS Enterprise Guide
#'
#' This a driver class for \code{SAS Enterprise Guide}.
#' 
#' The \code{SASEGDriver} class inherits from the \code{\link[DBI]{DBIDriver-class}}. 
#' @rdname SASEG
#' @exportClass SASEGDriver
setClass("SASEGDriver", contains = "DBIDriver", slots = list(isValid = "function"))

setMethod("show", "SASEGDriver", function(object) {
  cat("<SASEGDriver>\n")
})

#' @description \code{SASEG()} generates a new \code{SASEG} driver.
#' @return \code{SASEG()} returns a \code{SASEGDriver} object.
#' @export
SASEG <- function() {
  new("SASEGDriver", isValid = state_generator(init = TRUE))
}

#' @export
setMethod("dbIsValid", "SASEGDriver", function(dbObj, ...) {
  dbObj@isValid()
})

#' Unload SASEGDriver
#' 
#' This method was developed for \code{DBI} compliance.
#' @return \code{dbUnloadDriver} method returns \code{TRUE}.
#' @keywords internal
#' @export
setMethod("dbUnloadDriver", "SASEGDriver", function(drv, ...) {
  drv@isValid(set = FALSE)
  TRUE
})

# SAS Class and Methods ---------------------------------------------------


#' SAS program class
#' 
#' An S4 class for \code{SAS} programs.
#' @keywords internal
setClass("SAS", contains = "character")

#' Create a SAS program
#' 
#' \code{SAS} methods create a new \code{SAS} class object.
#' @param x An object. May be a \code{character} string or an object of a class 
#'     inheriting from \code{character}. 
#' @rdname SAS-class
#' @family SAS-methods
setGeneric("SAS", function(x, ...) standardGeneric("SAS"))

#' Quote a character string as a SAS program
#' 
#' This method quotes a character string as a \code{SAS} program.
#' This method is very similar to \code{\link[DBI]{SQL}} method for class \code{character}.
#' @param x A character string.
#' @return An object of class \code{SAS}.
#' @keywords internal
#' @family SAS-methods
#' @export
setMethod("SAS", "character", function(x, ...) {
  new("SAS", x)
})

# SAS class objects do not need to be quoted:
#' @export
setMethod("SAS", "SAS", function(x, ...) {
  return(x)
})

#' Transform an SQL statement into a SAS statement
#' 
#' This method wraps an \code{SQL} statement in a \code{PROC SQL}. For instance,
#' \code{SAS(SQL("SELECT * FROM SASHELP.CLASS"))} returns:
#' \tabular{l}{
#'    \code{PROC SQL DQUOTE=ANSI;} \cr
#'    \code{ods output SQL_Results=WORK.SQLOUT;} \cr
#'    \code{SELECT * FROM SASHELP.CLASS;} \cr
#'    \code{\%put &sqlobs;} \cr
#'    \code{QUIT;} \cr
#' }
#' 
#' The \code{ODS} option permits to create a \code{SAS} dataset when a 
#'     \code{SELECT} query is submitted (thx @@ François Malet).
#' @param x An object of class \code{\link[DBI]{SQL}}.
#' @param SQLResult A character string with the name of a table to store 
#'     the \code{SAS/ODS} output. If \code{SQLResult} is \code{NA_character_} 
#'     or \code{""}, the \code{ODS} option is not used.
#' @return An object of class \code{SAS}.
#' @examples 
#' sql_statement <- DBI::SQL("SELECT * \n FROM SASHELP.CLASS")
#' sas_pgm <- RSASEG::SAS(sql_statement)
#' show(sas_pgm)
#' @keywords internal
#' @family SAS-methods
#' @export
setMethod("SAS", "SQL", function(x, SQLResult = "WORK.SQLOUT", ...) {
  if(is.na(SQLResult)|(SQLResult == "")) {
    ods_string <- ""
  } else {
    ods_string <- paste0("ods output SQL_Results=", SQLResult, ";\n")
  }
  new("SAS",
      paste0("PROC SQL DQUOTE=ANSI;\n",
             ods_string,
             x, ";\n",
             "%put &sqlobs;\n", 
             "QUIT;\n"))
})

setMethod("show", "SAS", function(object) {
  cat(paste0("<SAS> ", object@.Data, collapse = "\n"))
})


# Connection Class and Methods --------------------------------------------


#' SAS EG connection class
#'
#' This class inherits from \code{\link[DBI]{DBIConnection-class}}.
#' An object of class \code{SASEGConnection} can be understood a \code{SAS EG} project.
#' @exportClass SASEGConnection
#' @slot profile A character string with the profile name.
#' @slot server A character string with the server name used to run \code{SAS} 
#'     codes.
#' @slot application A \code{\linkS4class{SASEGApplication}} object.
#' @slot SASProject A \code{\linkS4class{SASEGProject}} object.
#' @slot SASUtil A \code{\linkS4class{SASEGCode}} object. This slot is used to 
#'     run codes (eg. fetch programs) in a non persistent way.
#' @slot dbms A character string to store informations for \code{SAS/ACCESS} 
#'     connector. Not used yet.
#' @slot isValid A closure.
#' @keywords internal
setClass("SASEGConnection",
         contains = "DBIConnection",
         slots = list(
           profile = "character",
           server = "character",
           application = "SASEGApplication",
           SASProject = "SASEGProject",
           SASUtil = "SASEGCode",
           dbms = "character",
           isValid = "function"
         )
)

#' @description Use \code{dbConnect} to create a new connection to a \code{SAS} 
#'     server through \code{SAS Enterprise Guide}.
#' @param drv An object created by \code{SASEG()}.
#' @param DLLFilePath A character string with the filepath to \code{SASEGScripting.dll}.
#' @param profile A character string with the \code{SAS EG} profile name.
#' @param server A character string with the server name to run \code{SAS} programs.
#' @return \code{dbConnect} returns an object of class \code{\linkS4class{SASEGConnection}}.
#' @rdname SASEG
#' @export
#' @examples
#' \dontrun{
#' # Modify the path below following your install:
#' path <- "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"
#' my_profile <- "PROFILE"
#' my_server <- "SASPROD"
#' conn <- dbConnect(RSASEG::SASEG(), 
#'                   DLLFilePath = path, 
#'                   profile = my_profile, 
#'                   server = my_server)
#' show(conn)
#' dbWriteTable(conn, "mtcars", mtcars)
#' dbGetQuery(conn, "SELECT * FROM mtcars WHERE cyl = 4")
#' 
#' # Important: you have to disconnect from SAS EG
#' # When disconnecting, you also can save your work
#' RSASEG_project <- paste(normalizePath("~"), "RSASEG.egp", sep = "\\")
#' dbDisconnect(conn, projectPath = RSASEG_project)
#' }
#' @seealso \code{\link[=dbDisconnect,SASEGConnection-method]{dbDisconnect}}
setMethod("dbConnect", "SASEGDriver", function(drv, DLLFilePath, profile, server, ...) {
  # Load SAS.EG.Scripting namespace:
  loadSASEGScripting(DLLFilePath)
  # Create a new SASEGApplication object:
  application <- SASEGApplication()
  # Set profile:
  setProfile(application, profile)
  # Create a new SASEGProject object:
  SASProject <- newProject(application)
  # Create a new SASEGCode object to run utils tasks (e.g. fetch programs) in
  # a non persistent way:
  SASUtil <- newCode(project = SASProject, 
                     server = server, 
                     program = noteUtil, 
                     name = "garbage")
  dbms <- character(0)
  new("SASEGConnection", 
      profile = profile, 
      server = server, 
      application = application, 
      SASProject = SASProject, 
      SASUtil = SASUtil, 
      dbms = dbms,
      isValid = state_generator(init = TRUE)
      )
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

#' @export
setMethod("dbIsValid", "SASEGConnection", function(dbObj, ...) {
  dbObj@isValid()
})

#' Disconnect (or close) a SAS EG connection
#' 
#' \code{dbDisconnect} closes a \code{SAS EG} connection. It is important to 
#'    close connection because creation of a new \code{SASEGConnection} launchs 
#'    \code{SAS EG} in memory. You also can save your work in a \code{SAS EG} project.
#' @param conn An object created by \code{\link[=dbConnect,SASEGDriver-method]{dbConnect}}.
#' @param projectPath A character string with the path to save the project 
#'     created by \code{RSASEG}. \strong{Be careful: \code{dbDisconnect} method 
#'     overwrites existing files without confirmation}. If \code{NULL}, no project is saved.
#' @return \code{dbDisconnect} returns \code{TRUE}.
#' @examples
#' \dontrun{
#' # Modify the path below following your install:
#' path <- "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"
#' my_profile <- "PROFILE"
#' my_server <- "SASPROD"
#' conn <- dbConnect(RSASEG::SASEG(), 
#'                   DLLFilePath = path, 
#'                   profile = my_profile, 
#'                   server = my_server)
#' show(conn)
#' dbWriteTable(conn, "mtcars", mtcars)
#' dbGetQuery(conn, "SELECT * FROM mtcars WHERE cyl = 4")
#' 
#' # Important: you have to disconnect from SAS EG
#' # When disconnecting, you also can save your work
#' RSASEG_project <- paste(normalizePath("~"), "RSASEG.egp", sep = "\\")
#' dbDisconnect(conn, projectPath = RSASEG_project)
#' }
#' @seealso \code{\link[=dbConnect,SASEGDriver-method]{dbConnect}}
#' @export
setMethod("dbDisconnect", "SASEGConnection", function(conn, projectPath = NULL, ...) {
  if(dbIsValid(conn)) {
    if(!is.null(projectPath))  saveAs(conn@SASProject, projectPath)
    terminate(conn@application)
    conn@isValid(set = FALSE)
  } else {
    warning("Connection already disconnect.")
  }
  invisible(TRUE)
})


# Results class and Methods -----------------------------------------------


#' SASEG results class
#' 
#' \code{SASEGResult} class inherits from \code{\link[DBI]{DBIResult-class}}.
#' 
#' @slot SASResult An object of class \code{\linkS4class{SASEGCode}}.
#' @slot SASUtil An object of class \code{\linkS4class{SASEGCode}}.
#' @slot SQLResult A character string. This slot contains the filename of the 
#'     dataset created by the \code{ODS} during a \code{PROC SQL}.
#' @slot fetch A closure.
#' @slot rowsFetched A closure.
#' @slot isValid A closure.
#' @keywords internal
#' @exportClass SASEGResult
setClass("SASEGResult",
         contains = "DBIResult",
         slots = list(SASResult = "SASEGCode", 
                      SASUtil = "SASEGCode",
                      SQLResult = "character",
                      fetched = "function", 
                      rowsFetched = "function",
                      isValid = "function"
                      )
         )

#' Set the slot Fetched as...
#' 
#' This method sets the slot Fetched as a value.
#' @param res A result object.
#' @param value A value.
#' @keywords internal
setGeneric("setFetched", function(res, value) standardGeneric("setFetched"))

#' Set the slot fetched as...
#' 
#' This method sets the slot \code{fetched} as \code{TRUE} or \code{FALSE}.
#' @param res A \code{SASEGResult} object.
#' @param value A logical.
#' @keywords internal
setMethod("setFetched", "SASEGResult", function(res, value) {
  res@fetched(set = value)
})

#' @export
setMethod("dbHasCompleted", "SASEGResult", function(res, ...) {
  res@fetched()
})

#' @export
setMethod("dbIsValid", "SASEGResult", function(dbObj, ...) {
  dbObj@isValid()
})

#' Send a SAS query to SAS EG
#' 
#' This method sends a \code{SAS} query to \code{SAS EG}.
#' 
#' Statements with class \code{\linkS4class{SAS}} are directly sent to the 
#'     \code{SAS} server through \code{SAS EG} escaping any code transformations. 
#' @param conn A \code{SASEGConnection} object.
#' @param statement A \code{\linkS4class{SAS}} object.
#' @param persistent A logical. If \code{TRUE}, a new \code{SASEGCode} object 
#'      is created. If \code{FALSE}, \code{garbage} code is used.
#' @param codeName A character string to name the new \code{\linkS4class{SASEGCode}} object.
#' @return A \code{SASEGResult} object.
#' @keywords internal
#' @export
setMethod(
  "dbSendQuery", 
  c("SASEGConnection", "SAS"), 
  function(conn, statement, persistent = TRUE, codeName = NULL, ...) {
    if(persistent) {
      # Create a new SAS EG Code object with server and SAS program: 
      SASCode <- newCode(conn@SASProject, 
                         server = conn@server, 
                         program = statement, 
                         name = codeName
                         )
    } else {
        SASCode <- conn@SASUtil
        setText(SASCode, paste0(noteUtil, statement))
      }
    # Execute SAS program:
    run(SASCode)
    # Only for debugging:
    # cat(getSourceCode(SASCode))
    res <- new("SASEGResult",
               SASResult = SASCode,
               SASUtil = conn@SASUtil,
               SQLResult = NULL,
               fetched = state_generator(init = FALSE),
               rowsFetched = count_generator(),
               isValid = state_generator(init = TRUE)
               )
    if(countOutputDatasets(SASCode) == 0) setFetched(res, value = TRUE)
    return(res)
  }
)


#' Send an SQL query to SAS EG
#'
#' \code{dbSendQuery} sends an \code{SQL} query to \code{SAS}. The query is 
#'     first embedded in a \code{PROC SQL} and sent to \code{SAS}.
#'     
#' \code{dbSendQuery} can also send a \code{SAS} program: you have to escape it 
#'     first using \code{SAS()}.
#' @param statement A character string containing a \code{SQL} code or an 
#'     \code{\link[DBI]{SQL}} class object.
#' @inheritParams dbSendQuery,SASEGConnection,SAS-method 
#' @return A \code{SASEGResult} object.
#' @seealso Package \code{DBI}: \code{\link[DBI]{dbSendQuery}}
#' @export
#' @examples
#' # This is another good place to put examples
setMethod(
  "dbSendQuery", 
  c("SASEGConnection", "character"), 
  function(conn, statement, persistent = TRUE, codeName = NULL, ...) {
    # Choose a new dataset name to store the result of SQL statement:
    SQLResult <- paste0("WORK.", random_table_name())
    # Transform SQL statement into a SAS statement:
    statement <- SAS(SQL(statement), SQLResult = SQLResult)
    # Retrieve SAS execution results:
    res <- dbSendQuery(conn, statement, persistent, codeName)
    # Set the name of the dataset where SQL results are stored:
    res@SQLResult <- SQLResult
    #SQLResult %in% names(getListDatasets(res@SASResult))
    return(res)
  }
)


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
  res@isValid(set = FALSE)
  return(TRUE)
})


#' Retrieve records from SAS EG query
#' @export
setMethod("dbFetch", "SASEGResult", function(res, n = -1, ...) {
  l <- getListDatasets(res@SASResult)
  if(length(l)>1) stop("Query results contain multiple datasets; cannot execute dbFetch() method.\n  Please run:\n  datalist <- dbFetchAll(res)")
  d <- read(l[[1]])
  setFetched(res = res, value = TRUE)
  return(d)
})

setGeneric("dbFetchAll", function(res, ...) standardGeneric("dbFetchAll"))

#' @export
setMethod("dbFetchAll", "SASEGResult", function(res, ...) {
  l <- getListDatasets(res@SASResult)
  n <- length(l)
  d <- vector("list", n)
  i <- 1
  while(i <= length(l)) {
    d[[i]] <- read(l[[i]])
    i <- i+1
  }
  names(d) <- names(l)
  setFetched(res = res, value = TRUE)
  if(n == 1) {d <- d[[1]]}
  return(d)
})


setGeneric("dbGetLog", function(res, ...) standardGeneric("dbGetLog"))

#' @export
setMethod("dbGetLog", "SASEGResult", function(res, ...) {
  getLog(res@SASResult)
})

#' Find the database data type associated with an R object
#' @export
setMethod("dbDataType", "SASEGDriver", function(dbObj, obj, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  getSASType(obj)
})

#' @export
setMethod("dbDataType", "SASEGConnection", function(dbObj, obj, ...) {
  dbDataType(SASEG(), obj)
})


# SQL Methods -------------------------------------------------------------


#' @export
setMethod("dbQuoteString", c("SASEGConnection", "character"), function(conn, x, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  dbQuoteString(DBI::ANSI(), x, ...)
})

#' @export
setMethod("dbQuoteString", c("SASEGConnection", "SQL"), function(conn, x, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  dbQuoteString(DBI::ANSI(), x, ...)
})

#' @export
setMethod("dbQuoteIdentifier", c("SASEGConnection", "character"), function(conn, x, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  dbQuoteIdentifier(DBI::ANSI(), x, ...)
})

#' @export
setMethod("dbQuoteIdentifier", c("SASEGConnection", "SQL"), function(conn, x, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  dbQuoteIdentifier(DBI::ANSI(), x, ...)
})

#' @export
setMethod("dbQuoteIdentifier", c("SASEGConnection", "Table"), function(conn, x, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  dbQuoteIdentifier(DBI::ANSI(), x, ...)
})

#' @export
setMethod("sqlData", "SASEGConnection", function(con, value, row.names = NA, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  value <- sqlRownamesToColumn(value, row.names)

  # Convert factors to strings
  is_factor <- vapply(value, is.factor, logical(1))
  value[is_factor] <- lapply(value[is_factor], as.character)
  
  # Quote all strings
  is_char <- vapply(value, is.character, logical(1))
  value[is_char] <- lapply(value[is_char], function(x) {
    enc2utf8(dbQuoteString(con, x))
  })  
  
  # Convert logical 
  is_logical <- vapply(value, is.logical, logical(1))
  value[is_logical] <- lapply(value[is_logical], SASFormat)
  
  # Convert dates as SAS DATE9. format
  is_Date <- vapply(value, is.Date, logical(1))
  value[is_Date] <- lapply(value[is_Date], SASFormat)
  
  # Convert datetimes as SAS DATETIME18. format
  is_DateTime <- vapply(value, is.DateTime, logical(1))
  value[is_DateTime] <- lapply(value[is_DateTime], SASFormat)
  
  # Convert everything to character and turn NAs into NULL
  value[] <- lapply(value, as.character)
  value[is.na(value)] <- "NULL"
  
  return(value)
})

#' @export
setMethod("sqlAppendTable", "SASEGConnection", function(con, table, values, row.names = NA, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  stopifnot(is.data.frame(values))
  
  sql_values <- sqlData(con, values, row.names)
  table <- dbQuoteIdentifier(con, table)
  fields <- dbQuoteIdentifier(con, names(sql_values))
            
  # Convert fields into a character matrix
  rows <- do.call(paste, c(sql_values, sep = ", "))
  
  # SAS PROC SQL INSERT INTO statement has its own syntax. 
  # One row is inserted for each VALUES clause.
  # Multiple VALUES clauses are not separated by commas.
  SQL(paste0(
    "INSERT INTO ", table, "\n",
    "  (", paste(fields, collapse = ", "), ")\n",
    paste0("  VALUES(", rows, ")", collapse = "\n")
    ))
  }
)

#' @export
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
                     name = paste("Create dataset", name)
                     )
  run(SASCode)
})
