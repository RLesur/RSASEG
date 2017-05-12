#' @import DBI
#' @import methods
#' @importFrom stringr str_to_upper
#' @importFrom utils packageDescription
#' @include SASEGS4.R
#' @include SASEGDataType.R
#' @include utils.R
NULL

# Driver Class -----------------------------------------------------------------
#             /definition, getters and setters ---------------------------------

#' Driver class for SAS Enterprise Guide
#'
#' This a driver class for \code{SAS Enterprise Guide}. The \code{SASEGDriver} 
#'     class inherits from the \code{\link[DBI]{DBIDriver-class}}. A 
#'     \code{SASEGDriver} object can be understood as a \code{SAS EG} application.
#'     \code{SASEGDriver} objects are constructed by calling 
#'     \code{\link[=SASEG]{SASEG()}}.
#' @slot app A \code{\linkS4class{SASEGApplication}} object.
#' @slot cnx A closure in which active connections are stored. Values can be 
#'     accessed with 
#'     \code{\link[=dbListConnections,SASEGDriver-method]{dbListConnections}}.
#' @slot isValid A closure in which the status of the object is stored. Value 
#'     can be accessed with 
#'     \code{\link[=dbIsValid,SASEGDriver-method]{dbIsValid}}.
#' @seealso \code{SASEGDriver} methods: 
#'     \code{\link[=dbGetInfo,SASEGDriver-method]{dbGetInfo}},
#'     \code{\link[=dbIsValid,SASEGDriver-method]{dbIsValid}},
#'     \code{\link[=dbListConnections,SASEGDriver-method]{dbListConnections}},
#'     \code{\link[=dbUnloadDriver,SASEGDriver-method]{dbUnloadDriver}},
#'     \code{\link[=dbDataType,SASEGDriver-method]{dbDataType}}
#' @keywords internal
setClass(
  "SASEGDriver", 
  contains = "DBIDriver", 
  slots = list(
    app = "SASEGApplication",
    cnx = "list",
    isValid = "function"
    )
  )

setMethod("show", "SASEGDriver", function(object) {
  if(dbIsValid(object)) {
    cat("<SASEGDriver>\n")
    show(app(object))
  } else {
    warning("Invalid driver")
  }
})

#' Access to slot app of an object
#' 
#' Method to access a slot named \code{app}.
#' @param obj An object with a slot named \code{app}.
#' @param ... Other parameters passed on to method.
#' @keywords internal
#' @seealso \code{\link{app,SASEGDriver-method}}, 
#'     \code{\link{app,SASEGConnection-method}}
setGeneric("app", function(obj, ...) standardGeneric("app"))

#' Access to slot app of a SASEGDriver object
#' 
#' \code{app()} access to the slot \code{app} of a \code{SASEGDriver} object. 
#'     This method is not exported. Only developpers may need to use it.
#' @param obj A \code{SASEGDriver} object.
#' @param ... Other parameters passed on to method. Not used.
#' @rdname SASEGDriver-class
#' @keywords internal
setMethod("app", "SASEGDriver", function(obj, ...) {
  obj@app
})

#' Replace the value of slot cnx
#' 
#' A replacement method for a slot named \code{cnx}.
#' @param obj An object with a slot named \code{cnx}.
#' @param value A value to replace for.
#' @keywords internal
setGeneric("cnx<-", function(obj, value) standardGeneric("cnx<-")) 

#' Set the value of slot cnx of a SASEGDriver object
#' 
#' \code{`cnx<-`} replaces value stored in slot \code{cnx}. If there is already an 
#'     active connection, the only value accepted is \code{NULL}, an error is 
#'     raised otherwise. If there is no active connection, an object of class 
#'     \code{\linkS4class{SASEGConnection}} is accepted as a value. This method 
#'     is not exported. Only developpers may need to use it. 
#' @inheritParams app,SASEGDriver-method
#' @param value A value to replace for.
#' @rdname SASEGDriver-class
#' @keywords internal
setMethod("cnx<-", "SASEGDriver", function(obj, value) {
  obj@cnx$set(value)
  return(obj)
}) 

#' Set the value isValid of an object
#' 
#' A replacement method for a slot named \code{isValid}.
#' @param obj An object with a slot named \code{isValid}.
#' @param value A value to replace for.
#' @keywords internal
setGeneric("isValid<-", function(obj, value) standardGeneric("isValid<-")) 

#' Set the value isValid of a SASEGDriver object
#' 
#' Value stored in slot \code{isValid} can be replaced with \code{`isValid<-`}. This 
#'     method is not exported. Only developpers may need to use it. 
#' @inheritParams cnx<-,SASEGDriver-method
#' @rdname SASEGDriver-class
#' @keywords internal
setMethod("isValid<-", "SASEGDriver", function(obj, value) {
  obj@isValid(set = value)
  return(obj)
}) 

#' A driver for SAS Enterprise Guide
#' 
#' \code{SASEG()} generates a new \code{SASEG} driver. You 
#'     have to provide location of file \code{SASEGScripting.dll} following 
#'     your installation of \code{SAS Enterprise Guide}. A \code{SASEGDriver} 
#'     object can be understood as a \code{SAS EG} application.
#' @param DLLPath A character string with the path to file \code{SASEGScripting.dll}.
#' @return \code{SASEG()} returns a \code{\linkS4class{SASEGDriver}} object.
#' @seealso \code{SASEGDriver} methods: 
#'     \code{\link[=dbGetInfo,SASEGDriver-method]{dbGetInfo}},
#'     \code{\link[=dbIsValid,SASEGDriver-method]{dbIsValid}},
#'     \code{\link[=dbListConnections,SASEGDriver-method]{dbListConnections}},
#'     \code{\link[=dbUnloadDriver,SASEGDriver-method]{dbUnloadDriver}},
#'     \code{\link[=dbDataType,SASEGDriver-method]{dbDataType}}
#' @export
SASEG <- function(DLLPath = "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll") {
  if(!file.exists(DLLPath)) stop("cannot find file ", DLLPath)
  # Load SAS.EG.Scripting namespace:
  loadSASEGScripting(DLLPath)
  # Create a new SASEGApplication object:
  app <- SASEGApplication()
  
  new("SASEGDriver", 
      app = app, 
      cnx = cnx_list_generator(init = NULL), 
      isValid = state_generator(init = TRUE)
      )
}

#             /methods ---------------------------------------------------------
#                     //dbGetInfo -----------------------------------------------

#' Informations about a SASEGDriver
#' 
#' \code{dbGetInfo} method is called to get:
#' \itemize{
#'     \item \code{driver.version}: version of \code{RSASEG} package.
#'     \item \code{client.version}: version of \code{SAS Enterprise Guide}.
#'     \item \code{max.connections}: equals to 1.
#'     \item \code{available.profiles}: a list with informations about 
#'         available profiles.
#' }
#' @param dbObj An object created by a call to \code{\link[=SASEG]{SASEG()}}.
#' @inheritParams app,SASEGDriver-method
#' @examples
#' \dontrun{
#' library(DBI)
#' library(rClr)
#' 
#' # Modify the path below following your install:
#' path <- "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"
#' drv <- RSASEG::SASEG(path)
#' 
#' dbGetInfo(drv)
#' dbUnloadDriver(drv)}
#' @seealso \code{SASEGDriver} methods: 
#'     \code{\link[=dbIsValid,SASEGDriver-method]{dbIsValid}},
#'     \code{\link[=dbListConnections,SASEGDriver-method]{dbListConnections}},
#'     \code{\link[=dbUnloadDriver,SASEGDriver-method]{dbUnloadDriver}},
#'     \code{\link[=dbDataType,SASEGDriver-method]{dbDataType}}
#' @export
setMethod("dbGetInfo", "SASEGDriver", function(dbObj, ...) {
  if(!dbIsValid(dbObj)) stop("Invalid driver")
  
  list(
    "driver.version" = utils::packageDescription("RSASEG")$Version,
    "client.version" = getVersion(app(dbObj)),
    "max.connections" = 1,
    "available.profiles" = getListAvailableProfiles(app(dbObj))
  )
})

#                     //dbListConnections --------------------------------------

#' List active SAS EG connections
#' 
#' \code{dbListConnections} method is used to retrieve the list of active 
#'     connections. Since \code{SASEGDriver} allows a single connection, the 
#'     list contains either a single element or is empty.
#' @param drv An object created by a call to \code{\link[=SASEG]{SASEG()}}.
#' @param ... Other parameters. Not used.
#' @return \code{dbListConnections} returns an empty list if there is no active
#'     connection or a list with a single \code{\linkS4class{SASEGConnection}} 
#'     object.
#' @examples
#' \dontrun{
#' library(DBI)
#' library(rClr)
#' 
#' # Modify the path below following your install:
#' path <- "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"
#' drv <- RSASEG::SASEG(path)
#' 
#' dbListConnections(drv)
#' 
#' my_profile <- "PROFILE"
#' my_server <- "SASPROD"
#' conn <- dbConnect(drv, my_profile, my_server)
#' dbListConnections(drv)
#' 
#' dbDisconnect(conn)
#' dbListConnections(drv)
#' 
#' dbUnloadDriver(drv)}
#' @seealso \code{SASEGDriver} methods: 
#'     \code{\link[=dbGetInfo,SASEGDriver-method]{dbGetInfo}},
#'     \code{\link[=dbIsValid,SASEGDriver-method]{dbIsValid}},
#'     \code{\link[=dbUnloadDriver,SASEGDriver-method]{dbUnloadDriver}},
#'     \code{\link[=dbDataType,SASEGDriver-method]{dbDataType}}
#' @export
setMethod("dbListConnections", "SASEGDriver", function(drv, ...) {
  if(!dbIsValid(drv)) stop("invalid driver.")
  
  drv@cnx$get()
})

#                     //dbIsValid ----------------------------------------------

#' Is this SASEGDriver still valid ?
#' 
#' \code{dbIsValid} tests if a \code{SASEGDriver} object is still valid. A call
#'     to \code{\link[=dbUnloadDriver,SASEGDriver-method]{dbUnloadDriver}} 
#'     invalidates the object.
#' @inheritParams dbGetInfo,SASEGDriver-method
#' @return \code{dbIsValid} returns a \code{logical}.
#' @seealso \code{SASEGDriver} methods: 
#'     \code{\link[=dbGetInfo,SASEGDriver-method]{dbGetInfo}},
#'     \code{\link[=dbListConnections,SASEGDriver-method]{dbListConnections}},
#'     \code{\link[=dbUnloadDriver,SASEGDriver-method]{dbUnloadDriver}},
#'     \code{\link[=dbDataType,SASEGDriver-method]{dbDataType}}
#' @examples
#' \dontrun{
#' library(DBI)
#' library(rClr)
#' 
#' # Modify the path below following your install:
#' path <- "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"
#' drv <- RSASEG::SASEG(path)
#' dbIsValid(drv)
#' 
#' dbUnloadDriver(drv)
#' dbIsValid(drv)}
#' @export
setMethod("dbIsValid", "SASEGDriver", function(dbObj, ...) {
  dbObj@isValid()
})

#                     //dbUnloadDriver -----------------------------------------

#' Unload SASEGDriver
#' 
#' \code{dbUnloadDriver} usage is recommended for \code{SASEGDriver} objects. 
#'     Since this method is deprecated, a finalizer is registered upon garbage
#'     collector and at the end of the \code{R} session. The finalizer sends an 
#'     informative message to the user.
#' @inheritParams dbListConnections,SASEGDriver-method
#' @return \code{dbUnloadDriver} method returns \code{TRUE}.
#' @seealso \code{SASEGDriver} methods: 
#'     \code{\link[=dbGetInfo,SASEGDriver-method]{dbGetInfo}},
#'     \code{\link[=dbIsValid,SASEGDriver-method]{dbIsValid}},
#'     \code{\link[=dbListConnections,SASEGDriver-method]{dbListConnections}},
#'     \code{\link[=dbDataType,SASEGDriver-method]{dbDataType}}
#' @examples
#' \dontrun{
#' library(DBI)
#' library(rClr)
#' 
#' # Modify the path below following your install:
#' path <- "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"
#' drv <- RSASEG::SASEG(path)
#' 
#' my_profile <- "PROFILE"
#' my_server <- "SASPROD"
#' conn <- dbConnect(drv, my_profile, my_server)
#' 
#' dbWriteTable(conn, "mtcars", mtcars)
#' dbGetQuery(conn, "SELECT * FROM mtcars WHERE cyl = 4")
#' 
#' # Important: you have to disconnect from SAS EG and
#' #            unload driver.
#' # 
#' # When disconnecting, you also can save your work to 
#' # a SAS EG Project file.
#' 
#' dbDisconnect(conn)
#' dbUnloadDriver(drv)}
#' @export
setMethod("dbUnloadDriver", "SASEGDriver", function(drv, ...) {
  if(dbIsValid(drv)) {
    terminate(app(drv))
    isValid(drv) <- FALSE
  } else {
    warning("Invalid driver")
  }
  TRUE
})

#                     //dbDataType ---------------------------------------------

#' Find the SAS data type associated with an R object
#' 
#' \code{dbDataType} method finds the \code{SAS} data type associated with an 
#' \code{R} object.
#' @param dbObj An object resulting from a call to \code{\link[=SASEG]{SASEG()}} 
#'     or \code{\link[=dbConnect,SASEGDriver-method]{dbConnect()}}.
#' @param obj An \code{R} object whose \code{SAS} type we want to determine.
#' @param ... Other parameters. Not used.
#' @return A character string or a character vector.
#' @examples 
#' \dontrun{
#' library(DBI)
#' library(rClr)
#' 
#' # Modify the path below following your install:
#' path <- "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"
#' drv <- RSASEG::SASEG(path)
#' 
#' dbDataType(drv, 1:5)
#' dbDataType(drv, 1)
#' dbDataType(drv, TRUE)
#' dbDataType(drv, Sys.Date())
#' dbDataType(drv, Sys.time())
#' dbDataType(drv, Sys.time() - as.POSIXct(Sys.Date()))
#' dbDataType(drv, c("x", "abc"))
#' dbDataType(drv, list(raw(10), raw(20)))
#' dbDataType(drv, I(3))
#' dbDataType(drv, iris)
#' 
#' my_profile <- "PROFILE"
#' my_server <- "SASPROD"
#' conn <- dbConnect(drv, my_profile, my_server)
#'
#' dbDataType(conn, 1:5)
#' dbDataType(conn, 1)
#' dbDataType(conn, TRUE)
#' dbDataType(conn, Sys.Date())
#' dbDataType(conn, Sys.time())
#' dbDataType(conn, Sys.time() - as.POSIXct(Sys.Date()))
#' dbDataType(conn, c("x", "abc"))
#' dbDataType(conn, list(raw(10), raw(20)))
#' dbDataType(conn, I(3))
#' dbDataType(conn, iris)
#' 
#' dbDisconnect(conn, projectPath = RSASEG_project)
#' dbUnloadDriver(drv)}
#' @seealso Generic: \code{\link[DBI]{dbDataType}}.
#' @seealso \code{SASEGDriver} methods: 
#'     \code{\link[=dbGetInfo,SASEGDriver-method]{dbGetInfo}},
#'     \code{\link[=dbIsValid,SASEGDriver-method]{dbIsValid}},
#'     \code{\link[=dbListConnections,SASEGDriver-method]{dbListConnections}},
#'     \code{\link[=dbUnloadDriver,SASEGDriver-method]{dbUnloadDriver}}
#' @export
setMethod("dbDataType", "SASEGDriver", function(dbObj, obj, ...) {
  getSASType(obj)
})


# SAS Class and Methods --------------------------------------------------------

#' SAS program class
#' 
#' An S4 class for \code{SAS} programs. This class extends the \code{character} class.
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
#' @param ... Other parameters passed on to methods.
#' @return An object of class \code{SAS}.
#' @keywords internal
#' @family SAS-methods
#' @export
setMethod("SAS", "character", function(x, ...) {
  new("SAS", x)
})

#' Transform an SQL statement into a SAS statement
#' 
#' This method wraps an \code{SQL} statement in a \code{PROC SQL}. For instance,
#' \code{SAS(SQL("SELECT * FROM SASHELP.CLASS"))} returns:
#' \tabular{l}{
#'    \code{PROC SQL DQUOTE=ANSI;} \cr
#'    \code{ods output SQL_Results=WORK.SQLOUT;} \cr
#'    \code{SELECT * FROM SASHELP.CLASS;} \cr
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
setMethod("SAS", "SQL", function(x, SQLResult = "WORK.SQLOUT") {
  if(is.na(SQLResult)|(SQLResult == "")) {
    ods_string <- ""
  } else {
    ods_string <- paste0("ods output SQL_Results=", SQLResult, ";\n")
  }
  new("SAS",
      paste0("PROC SQL DQUOTE=ANSI;\n",
             ods_string,
             x, ";\n",
             "QUIT;\n"))
})

setMethod("show", "SAS", function(object) {
  cat(paste0("<SAS> ", object@.Data, collapse = "\n"))
})

#' Refer to a SAS dataset
#' 
#' \code{dataset} method is used to refer to a \code{SAS} dataset. 
#' @param name A dataset name. Either a character string (unquoted or quoted by
#'     \code{\link[DBI]{dbQuoteIdentifier}}) or a \code{\link[DBI]{Table-class}}
#'     object.
#' @param ... Other argument passed on to method.
#' @keywords internal
setGeneric("dataset", function(name, ...) standardGeneric("dataset"))

#' @rdname dataset
#' @inheritParams dataset
#' @param libname A character string with the libname.
#' @return \code{dataset} function returns a \code{\link[DBI]{Table-class}} object.
#' @examples
#' dataset("SASHELP.CLASS")
#' dataset(libname = "SASHELP", name = "CLASS")
#' 
#' # For a dataset in WORK library, you can use:
#' dataset("TEST")
#' @export
setMethod("dataset", "character", function(name, libname = "WORK") {
  if(length(libname) > 1 || length(name) > 1) stop("You must provide atomic libname/name.", call. = FALSE)
  name <- strsplit(name, ".", fixed = TRUE)[[1]]
  if(length(name) > 2) stop('name argument cannot contain more than one "."', call. = FALSE)
  if(length(name) == 2) return(new("Table", name = name))
  if(grepl(pattern = ".", x = libname, fixed = TRUE)) stop('Libname cannot contain "."', call. = FALSE)
  if(is.null(libname)) {
    warning("Null libname, WORK is provided as libname.", immediate. = TRUE)
    libname <- "WORK"
  }
  
  new("Table", name = c(as.character(libname), as.character(name)))
})

#' @rdname dataset
#' @inheritParams dataset
#' @export
setMethod("dataset", "Table", function(name, ...) {
  stopifnot(length(name@name) %in% 1:2)
  
  if(length(name@name) == 2) {
    return(name)
  } else {
    return(dataset(name = name@name))
  }
})

#' @rdname dataset
#' @inheritParams dataset
#' @export
setMethod("dataset", "SQL", function(name, ...) {
  stopifnot(length(name) == 1)
  name <- as.character(name)
  quoted <- substr(name, 1, 1) == '"' && substr(name, nchar(name), nchar(name)) == '"'
  if(quoted) {
    name <- gsub(pattern = '""', replacement = '"', name, fixed = TRUE)
    name <- substr(name, 2, nchar(name) - 1)
    name <- gsub(pattern = '"."', replacement = '.', name, fixed = TRUE)
  } 
  
  dataset(name)
})


# Connection Class -------------------------------------------------------------
#                 /definition, getters and setters -----------------------------

#' SAS EG connection class
#'
#' This class inherits from \code{\link[DBI]{DBIConnection-class}}.
#' An object of class \code{SASEGConnection} can be understood as a \code{SAS EG} 
#' project.
#' @exportClass SASEGConnection
#' @slot infos An environment that reference different objects. This environment is 
#'     registered. Referenced objects are: \itemize{
#'     \item \code{drv} (the \code{\linkS4class{SASEGDriver}} object used to connect to), 
#'     \item \code{isValid} (a logical), 
#'     \item \code{profile} (a character string), 
#'     \item \code{server} (a character string), 
#'     \item \code{SASProject} (a \code{\linkS4class{SASEGProject}} object), 
#'     \item \code{SASUtil} (a \code{\linkS4class{SASEGCode}} object), 
#'     \item \code{listResults} (an environment),
#'     \item \code{sqlrc} (a numeric) for \code{SAS PROC SQL} return code,
#'     \item \code{syserrortext} (a character string) for \code{SAS PROC SQL} error message, 
#'     \item \code{dbms} (a character string) with DBMS name for \code{SAS/ACCESS} connection. 
#'         Not yet implemented. 
#'     \item \code{dbms.args} a named list with DBMS arguments for \code{SAS/ACCESS} connection. 
#'         Not yet implemented. 
#'         }
#' @seealso \code{\link[=dbGetException,SASEGConnection-method]{dbGetException}}, 
#'     \code{\link[=dbIsValid,SASEGConnection-method]{dbIsValid}}.
#' @keywords internal
setClass("SASEGConnection",
         contains = "DBIConnection",
         slots = list(infos = "environment")
         )

#' Access to slot drv of an object
#' 
#' A generic accessor to a slot named \code{drv}.
#' @param obj An object.
#' @param ... Other parameters passed on to method.
#' @keywords internal
setGeneric("drv", function(obj, ...) standardGeneric("drv"))

#' Get the driver referenced in a SASEGConnection object
#' 
#' \code{drv} method access to the \code{\linkS4class{SASEGDriver}} object 
#'     referenced in a \code{SASEGConnection} object. This method 
#'     is not exported.
#' @param obj An object of class \code{SASEGconnection}.
#' @param ... Other parameters passed on to method. Not used.
#' @return \code{drv} returns a \code{\linkS4class{SASEGDriver}} object.
#' @rdname SASEGConnection-class
#' @keywords internal
setMethod("drv", "SASEGConnection", function(obj, ...) {
  env <- obj@infos
  env$drv
})

#' Set the value isValid of a SASEGConnection object
#' 
#' \code{`isValid<-`} method is used to replace the referenced object \code{isValid}. 
#'     This method is not exported. 
#' @inheritParams drv,SASEGConnection-method
#' @param value A logical.
#' @rdname SASEGConnection-class
#' @keywords internal
setMethod("isValid<-", "SASEGConnection", function(obj, value) {
  env <- obj@infos
  env$isValid <- value
  
  obj
}) 

#' Get the application object referenced in a SASEGConnection object
#' 
#' \code{app} method is a getter to the \code{\linkS4class{SASEGApplication}} 
#'     object referenced in a \code{SASEGConnection} object. This 
#'     method is not exported. 
#' @inheritParams drv,SASEGConnection-method
#' @return \code{app} returns a \code{\linkS4class{SASEGApplication}} object.
#' @rdname SASEGConnection-class
#' @keywords internal
setMethod("app", "SASEGConnection", function(obj, ...) {
  app(drv(obj))
})

#' Get the profile of an object
#' 
#' Methods to get profile of an object.
#' @param obj An object.
#' @keywords internal
setGeneric("getProfile", function(obj) standardGeneric("getProfile"))

#' Get the profile used in a connection
#' 
#' \code{getProfile} method is used to get the profile used in a connection. 
#'     This method is not exported.
#' @inheritParams drv,SASEGConnection-method
#' @return \code{getProfile} returns a character string.
#' @rdname SASEGConnection-class
setMethod("getProfile", "SASEGConnection", function(obj) {
  env <- obj@infos
  env$profile
})

#' Get the server of an object
#' 
#' Methods to get server of an object.
#' @param obj An object.
#' @keywords internal
setGeneric("server", function(obj) standardGeneric("server"))

#' Get the server used in a connection
#' 
#' \code{server} method is used to get the server used in a connection. 
#'     This method is not exported.
#' @inheritParams drv,SASEGConnection-method
#' @return \code{server} method returns a character string.
#' @rdname SASEGConnection-class
setMethod("server", "SASEGConnection", function(obj) {
  env <- obj@infos
  env$server
})

#' Get the project of an object
#' 
#' Methods to get project of an object.
#' @param obj An object.
#' @keywords internal
setGeneric("project", function(obj) standardGeneric("project"))

#' Get the project associated with a connection
#' 
#' \code{project} method is used to get the \code{\linkS4class{SASEGProject}}
#'     referenced in a connection. This method is not exported.
#' @inheritParams drv,SASEGConnection-method
#' @return \code{project} method returns a \code{\linkS4class{SASEGProject}} object.
#' @rdname SASEGConnection-class
setMethod("project", "SASEGConnection", function(obj) {
  env <- obj@infos
  env$SASProject
})

#' Get Util of an object
#' 
#' Methods to get Util of an object.
#' @param obj An object.
#' @keywords internal
setGeneric("getUtil", function(obj) standardGeneric("getUtil"))

#' Get the util code referenced in a connection
#' 
#' \code{getUtil} method is used to get the \code{\linkS4class{SASEGCode}}
#'     object used in a connection to run utils codes. This method is not exported.
#' @inheritParams drv,SASEGConnection-method
#' @return \code{getUtil} method returns a \code{\linkS4class{SASEGCode}} object.
#' @rdname SASEGConnection-class
setMethod("getUtil", "SASEGConnection", function(obj) {
  env <- obj@infos
  env$SASUtil
})

#' Get DBMS slot of an object
#' 
#' Methods to get DBMS slot of an object.
#' @param obj An object.
#' @keywords internal
setGeneric("getDBMS", function(obj) standardGeneric("getDBMS"))

#' Get the DBMS name referenced in a connection
#' 
#' \code{getDBMS} method is used to get the DBMS name used for 
#'     \code{SAS/ACCESS SQL} pass-through. This method is not exported.
#' @inheritParams drv,SASEGConnection-method
#' @return \code{getDBMS} method returns a character string.
#' @rdname SASEGConnection-class
setMethod("getDBMS", "SASEGConnection", function(obj) {
  env <- obj@infos
  env$dbms
})

#' Get DBMS arguments slot of an object
#' 
#' Methods to get DBMS arguments slot of an object.
#' @param obj An object.
#' @keywords internal
setGeneric("getDBMSArgs", function(obj) standardGeneric("getDBMSArgs"))

#' Get the DBMS arguments referenced in a connection
#' 
#' \code{getDBMSArgs} method is used to get the DBMS arguments used for 
#'     \code{SAS/ACCESS SQL} pass-through. This method is not exported.
#' @inheritParams drv,SASEGConnection-method
#' @return \code{getDBMSArgs} method returns a named list of character strings.
#' @rdname SASEGConnection-class
setMethod("getDBMSArgs", "SASEGConnection", function(obj) {
  env <- obj@infos
  env$dbms.args
})

setMethod("show", "SASEGConnection", function(object) {
  cat(
    "<SASEGConnection>\n",
    "Used Profile in Active Connection: ", getProfile(object), "\n",
    "SAS Server to Run Programs in Active Connection: ", server(object), "\n",
    "DBMS SQL Pass-Through: ", if(length(getDBMS(object)) == 0) "NONE" else getDBMS(object),
    sep = ""
  )
})

#' Clear list of results
#' 
#' This method removes invalid results from the list of results.
#' @param conn An object.
#' @param ... Other parameters passed on to method.
#' @keywords internal
setGeneric("dbClearListResults", function(conn, ...) standardGeneric("dbClearListResults"))

#' Clear list of results
#' 
#' This method removes invalid \code{\linkS4class{SASEGSQLResult}} of the list 
#' of results referenced in a \code{\linkS4class{SASEGConnection}} object. This
#' method is not exported.
#' @param conn A \code{\linkS4class{SASEGConnection}} object.
#' @param ... Other parameters passed on to method. Not used.
#' @return \code{TRUE}, invisibly.
#' @keywords internal
setMethod("dbClearListResults", "SASEGConnection", function(conn, ...) {
  listResults <- conn@infos$listResults
  lapply(ls(listResults), function(x) {
    if(!dbIsValid(get(x, envir = listResults))) eval(call("rm", as.name(x), envir = listResults)) 
  })
  invisible(TRUE)
})

#' Get DBMS connect string
#' 
#' Get DBMS connect string
#' @param conn An object.
#' @param ... Other parameters passed on to method.
#' @keywords internal
setGeneric("dbConnectString", function(conn, ...) standardGeneric("dbConnectString"))

#' Get DBMS connect string for SAS/ACCESS
#' 
#' This method returns a character string with the connection string for 
#' \code{SAS/ACCESS}. This method is not exported.
#' @param conn A \code{\linkS4class{SASEGConnection}} object.
#' @param cnx.alias A character string with the alias name for the connection.
#' @param ... Other parameters passed on to method. Not used.
#' @return A character string.
#' @keywords internal
setMethod("dbConnectString", "SASEGConnection", function(conn, cnx.alias = "CNX", ...) {
  dbms <- getDBMS(conn)
  if(length(dbms) == 0) {
    return(DBI::SQL(""))
  } else {
    dbms.args <- getDBMSArgs(conn)
    dbms.args <- lapply(dbms.args, dbQuoteString, conn = conn)
    
    return(SQL(
      paste0(
        "CONNECT TO ", dbms, " AS ", cnx.alias,
        " (",
        paste0(names(dbms.args), "=", dbms.args, collapse = " "),
        ");\n"
      )
    ))
  }
})

# Driver Class -----------------------------------------------------------------
#             /methods ---------------------------------------------------------
#                     //dbConnect ----------------------------------------------

#' A finalizer function for SASEGConnection
#' 
#' A finaliser function for \code{\linkS4class{SASEGConnection}} objects. 
#' This function is not exported.
#' @param e An environment. This is the \code{infos} slot of a 
#'     \code{\linkS4class{SASEGConnection}} object.
#' @return \code{NULL}, invisibly. 
#' @keywords internal
finalize_cnx <- function(e) {
  cnx <- new("SASEGConnection", infos = e)
  if(dbIsValid(cnx)) {
    message("Disconnecting a lost connection.")
    dbDisconnect(cnx)
    }
  
  invisible()
}

#' @description Use \code{dbConnect} to create a new connection to a \code{SAS} 
#'     server through \code{SAS Enterprise Guide}. A single connection is 
#'     allowed by a \code{SASEGDriver} object. If needed, use multiple driver 
#'     objects instead of multiple connections.
#' @param profile A character string with the \code{SAS EG} profile name.
#' @param server A character string with the \code{SAS} server name to run programs.
#' @param dbms Optional. A character string with the DBMS name for 
#'     \code{SAS/ACCESS SQL} pass-through.
#' @param dbms.args A named list with the DBMS arguments for
#'     \code{SAS/ACCESS SQL} pass-through. Compulsory if \code{dbms} argument 
#'     is not \code{NULL}.
#' @inheritParams dbListConnections,SASEGDriver-method
#' @return \code{dbConnect} returns an object of class \code{\linkS4class{SASEGConnection}}.
#' @rdname SASEG
#' @export
#' @examples
#' \dontrun{
#' library(DBI)
#' library(rClr)
#' 
#' # Modify the path below following your install:
#' path <- "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"
#' drv <- RSASEG::SASEG(path)
#' 
#' my_profile <- "PROFILE"
#' my_server <- "SASPROD"
#' conn <- dbConnect(drv, my_profile, my_server)
#' 
#' dbWriteTable(conn, "mtcars", mtcars)
#' dbGetQuery(conn, "SELECT * FROM mtcars WHERE cyl = 4")
#' 
#' # Important: you have to disconnect from SAS EG and
#' #            unload driver.
#' # 
#' # When disconnecting, you also can save your work to 
#' # a SAS EG Project file.
#' 
#' RSASEG_project <- paste(normalizePath("~"), "RSASEG.egp", sep = "\\")
#' dbDisconnect(conn, projectPath = RSASEG_project)
#' dbUnloadDriver(drv)}
#' @family SASEGConnection class methods
setMethod("dbConnect", "SASEGDriver", function(drv, profile, server, dbms = NULL, dbms.args = NULL, ...) {
  if(!dbIsValid(drv)) stop("invalid driver.")
  
  list_cnx <- dbListConnections(drv)
  if(length(list_cnx) >= dbGetInfo(drv)$max.connections) {
    warning("Cannot create a new connection: max. number of connections reached.\n  Existing connection returned.", immediate. = TRUE)
    cnx <- dbListConnections(drv)[[1]]
    if(!getProfile(cnx) == profile) warning("\n  Previous profile used: ", getProfile(cnx), immediate. = TRUE)
    if(!server(cnx) == server) warning("\n  Previous server used: ", server(cnx), immediate. = TRUE)
    return(cnx)
  }
  
  if(!is.null(dbms)) {
    stopifnot(length(dbms) == 1)
    stopifnot(is.character(dbms))
    stopifnot(!is.null(dbms.args))
  }
  
  if(!is.null(dbms.args)) {
    stopifnot(!is.null(dbms))
    stopifnot(is.list(dbms.args), length(dbms.args) > 0)
    stopifnot(!is.null(names(dbms.args)))
    stopifnot(nchar(names(dbms.args)) > 0)
    stopifnot(vapply(dbms.args, is.character, logical(1)))
  }
  
  infos <- new.env(parent = emptyenv())
  infos$drv <- drv
  infos$isValid <- TRUE
  infos$profile <- profile
  infos$server <- server
  infos$listResults <- new.env(parent = emptyenv())
  app <- app(drv)
  # Set profile:
  setProfile(app, profile)
  # Create a new SASEGProject object:
  SASProject <- newProject(app)
  # Create a new SASEGCode object to run utils tasks (e.g. fetch programs) in
  # a non persistent way:
  infos$SASUtil <- newCode(project = SASProject, 
                           server = server, 
                           program = noteUtil, 
                           name = "garbage"
                           )
  infos$SASProject <- SASProject
  infos$dbms <- as.character(dbms)
  infos$dbms.args <- as.list(dbms.args)
  new_cnx <- new("SASEGConnection", infos = infos)
  on.exit(reg.finalizer(infos, finalize_cnx, onexit = TRUE))
  # Add the new connection to the list of connections of the driver:
  cnx(drv) <- new_cnx
  
  new_cnx
})

# Connection Class -------------------------------------------------------------
#                 /SQL methods -------------------------------------------------
#                             /dbQuoteString -----------------------------------

#' Quote literal strings
#' 
#' Quote literal strings. 
#' @param conn A \code{\linkS4class{SASEGConnection}} object.
#' @inheritParams DBI::dbQuoteString
#' @return An object that can be coerced to character of the same length as the 
#'     input.
#' @seealso Generic: \code{\link[DBI]{dbQuoteString}}.
#' @export
setMethod("dbQuoteString", c("SASEGConnection", "character"), function(conn, x, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  dbQuoteString(DBI::ANSI(), x, ...)
})


#' @rdname dbQuoteString-SASEGConnection-character-method
#' @export
setMethod("dbQuoteString", c("SASEGConnection", "SQL"), function(conn, x, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  dbQuoteString(DBI::ANSI(), x, ...)
})

#                             /dbQuoteIdentifier -------------------------------

#' Quote identifiers
#' 
#' Quote identifiers.
#' @param conn A \code{\linkS4class{SASEGConnection}} object.
#' @inheritParams DBI::dbQuoteIdentifier
#' @return An object that can be coerced to character of the same length as the 
#'     input.
#' @seealso Generic: \code{\link[DBI]{dbQuoteIdentifier}}.
#' @export
setMethod("dbQuoteIdentifier", c("SASEGConnection", "character"), function(conn, x, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  dbQuoteIdentifier(DBI::ANSI(), x, ...)
})

#' @rdname dbQuoteIdentifier-SASEGConnection-character-method
#' @export
setMethod("dbQuoteIdentifier", c("SASEGConnection", "SQL"), function(conn, x, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  dbQuoteIdentifier(DBI::ANSI(), x, ...)
})

#' @rdname dbQuoteIdentifier-SASEGConnection-character-method
#' @export
setMethod("dbQuoteIdentifier", c("SASEGConnection", "Table"), function(conn, x, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  dbQuoteIdentifier(DBI::ANSI(), x, ...)
})

#                             /sqlData -----------------------------------------

#' Convert a data frame into form suitable for upload to SAS
#' 
#' Convert a data frame into form suitable for upload to \code{SAS}.
#' 
#' \code{sqlData}:\itemize{
#' \item Converts factors to characters
#' \item Quote all strings
#' \item Converts logical as integers
#' \item Convert dates as \code{SAS DATE9} format
#' \item Convert datetimes as \code{SAS DATETIME} format
#' \item Convert difftimes as \code{SAS TIME} format
#' \item Replaces \code{NA} with \code{NULL}
#' }
#' 
#' @param con A \code{\linkS4class{SASEGConnection}} object.
#' @inheritParams DBI::sqlData
#' @return A data frame with suitable values for \code{SAS}.
#' @seealso Generic: \code{\link[DBI]{sqlData}}.
#' @keywords internal
#' @export
setMethod("sqlData", "SASEGConnection", function(con, value, row.names = NA, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  value <- DBI::sqlRownamesToColumn(value, row.names)
  
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
  
  # Convert datetimes as SAS DATETIME. format
  is_DateTime <- vapply(value, is.DateTime, logical(1))
  value[is_DateTime] <- lapply(value[is_DateTime], SASFormat)
  
  # Convert difftimes as SAS TIME. format
  is_difftime <- vapply(value, is.difftime, logical(1))
  value[is_difftime] <- lapply(value[is_difftime], SASFormat)
  
  # Convert everything to character and turn NAs into NULL
  value[] <- lapply(value, as.character)
  value[is.na(value)] <- "NULL"
  
  value
})

#                             /sqlAppendTable ----------------------------------

#' Insert rows into a dataset
#' 
#' \code{sqlAppendTable} generates a single \code{SQL} string that inserts a data 
#'     frame into an existing \code{SAS} dataset.
#' @param table Name of the dataset.
#' @inheritParams sqlData,SASEGConnection-method
#' @inheritParams DBI::sqlAppendTable
#' @return An \code{\link[DBI]{SQL}} object with \code{SQL} code.
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
  DBI::SQL(paste0(
    "INSERT INTO ", table, "\n",
    "  (", paste(fields, collapse = ", "), ")\n",
    paste0("  VALUES(", rows, ")", collapse = "\n")
  ))
}
)


#                 /methods -----------------------------------------------------
#                         //dbConnect ------------------------------------------

#' Replicate a connection
#' 
#' \code{dbConnect} method replicates a connection. As 
#' \code{\linkS4class{SASEGDriver}} class allows a single connection per 
#' driver object, the behavior of \code{dbConnect} is the following: 
#' \itemize{
#' \item If the \code{\linkS4class{SASEGConnection}} object passed in argument 
#' is active, \code{dbConnect} returns this object. 
#' \item If the \code{\linkS4class{SASEGConnection}} object passed in argument 
#' is inactive, \code{dbConnect} returns a new valid connection with same 
#' profile and server.}
#' @param drv An object created with \code{\link[=dbConnect,SASEGDriver-method]{dbConnect()}}.
#' @param ... Other parameters passed on to method. Not used.
#' @return A \code{\linkS4class{SASEGConnection}} object.
#' @seealso Generic: \code{\link[DBI]{dbConnect}}.
#' @family SASEGConnection class methods
#' @export
setMethod("dbConnect", "SASEGConnection", function(drv, ...) {
  dbConnect(drv(drv), getProfile(drv), server(drv))
})

#                         //dbIsValid ------------------------------------------
#' Test if SAS EG connection is valid
#' 
#' \code{dbIsValid} tests if a \code{\linkS4class{SASEGConnection}} object is valid.
#' @param dbObj An object created with \code{\link[=dbConnect,SASEGDriver-method]{dbConnect()}}.
#' @param ... Other parameters. Not used.
#' @seealso Generic: \code{\link[DBI]{dbIsValid}}.
#' @family SASEGConnection class methods
#' @export
setMethod("dbIsValid", "SASEGConnection", function(dbObj, ...) {
  env <- dbObj@infos
  env$isValid
})

#                         //dbGetException -------------------------------------

#' Get SAS/PROC SQL exceptions
#' 
#' Get the last \code{SAS/PROC SQL} exception.
#' 
#' \code{errorNum} and \code{errorMsg} are given by the \code{SAS} automatic
#'     macro variables \code{SQLRC} and \code{SYSERRORTEXT}.
#' @param conn An object created by \code{\link[=dbConnect,SASEGDriver-method]{dbConnect}}.
#' @inheritParams dbConnect,SASEGConnection-method
#' @return A list with elements \code{errorNum} (an integer error number) and 
#'     \code{errorMsg} (a character string) describing the last error in the 
#'     connection \code{conn}.
#' @seealso Generic: \code{\link[DBI]{dbGetException}}.
#' @family SASEGConnection class methods
#' @export
setMethod("dbGetException", "SASEGConnection", function(conn, ...) {
  list(
    errorNum = conn@infos$sqlrc,
    errorMsg = conn@infos$syserrortext
  )
})

#                         //dbDisconnect ---------------------------------------

#' Disconnect (or close) a SAS EG connection
#' 
#' \code{dbDisconnect} closes a \code{SAS EG} connection. It is important to 
#'    close connection because creation of a new \code{SASEGConnection} launchs 
#'    \code{SAS EG} in memory. You also can save your work in a \code{SAS EG} project.
#' @param conn An object created by \code{\link[=dbConnect,SASEGDriver-method]{dbConnect}}.
#' @param projectPath A character string with the path to save the project 
#'     created by \code{RSASEG}. \strong{Be careful: \code{dbDisconnect} method 
#'     overwrites existing files without confirmation}. If \code{NULL}, no project is saved.
#' @inheritParams dbConnect,SASEGConnection-method
#' @return \code{dbDisconnect} returns \code{TRUE}, invisibly.
#' @examples
#' \dontrun{
#' library(DBI)
#' library(rClr)
#' 
#' # Modify the path below following your install:
#' path <- "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"
#' drv <- RSASEG::SASEG(path)
#'
#' my_profile <- "PROFILE"
#' my_server <- "SASPROD"
#' conn <- dbConnect(drv, my_profile, my_server)
#' 
#' dbWriteTable(conn, "mtcars", mtcars)
#' dbGetQuery(conn, "SELECT * FROM mtcars WHERE cyl = 4")
#' 
#' # Important: you have to disconnect from SAS EG
#' # When disconnecting, you also can save your work
#' RSASEG_project <- paste(normalizePath("~"), "RSASEG.egp", sep = "\\")
#' dbDisconnect(conn, projectPath = RSASEG_project)
#' }
#' @seealso Generic: \code{\link[DBI]{dbDisconnect}}.
#' @family SASEGConnection class methods
#' @export
setMethod("dbDisconnect", "SASEGConnection", function(conn, projectPath = NULL, ...) {
  if(dbIsValid(conn)) {
    if(!is.null(projectPath)) saveAs(project(conn), projectPath)
    # Detach current connection from the list of connections of the driver:
    drv <- drv(conn)
    cnx(drv) <- NULL
    # Close SASEGProject:
    terminate(project(conn))
    # Unvalidate connection:
    isValid(conn) <- FALSE
  } else {
    warning("Connection already disconnected.", immediate. = TRUE)
  }
  invisible(TRUE)
})

#                         //dbGetInfo ------------------------------------------

#' Informations about a connection
#' 
#' \code{dbGetInfo} returns informations about a \code{\linkS4class{SASEGConnection}}
#' object:
#' \itemize{
#' \item \code{profile}: profile used in connection.
#' \item \code{db.version}: version number of \code{SAS} (this is the 
#'     \code{SYSVER} \code{SAS} macro variable).
#' \item \code{dbname}: \code{USER} libname, default: \code{WORK} (this is the 
#'     \code{SAS} \code{USER} option).
#' \item \code{username}: username on \code{SAS} server (this is the 
#'     \code{SYSUSERID} \code{SAS} macro variable).
#' \item \code{host}: host name.
#' \item \code{port}: port number.}
#' @inheritParams dbIsValid,SASEGConnection-method
#' @seealso Generic: \code{\link[DBI]{dbGetInfo}}.
#' @family SASEGConnection class methods
#' @export
setMethod("dbGetInfo", "SASEGConnection", function(dbObj, ...) {
  profile <- getProfile(dbObj)
  
  list_avail_profl <- dbGetInfo(drv(dbObj))$available.profiles
  is.profile <- vapply(list_avail_profl, function(x) x$profilename == profile, logical(1))
  profileinfos <- list_avail_profl[is.profile][[1]]
  host <- profileinfos$host
  port <- profileinfos$port

  # Construct a DATA STEP to retrieve db.version, dbname and username:
  # * pick a name:
  infosFileName <- paste0("WORK.", random_table_name())
  # * elaborate the DATA STEP:
  statement <- SAS(
    paste(
      paste0("DATA ", infosFileName, ";"),
      '  dbversion=SYMGET("sysver");',
      '  dbname=GETOPTION("user");',
      '  username=SYMGET("sysuserid");',
      'RUN;',
      sep = "\n"
    )
  )
  # Run SAS statement and get a SASEGResult object:
  res <- dbSendQuery(dbObj, statement, codeName = NULL, persistent = FALSE)
  l <- getListDatasets(res@SASResult)
  infos <- read(l[[infosFileName]])
  db.version <- infos$dbversion
  if(is.na(infos$dbname)) {
    dbname <- "WORK"
  } else {
    if(infos$dbname == "") {dbname <- "WORK"} else {dbname <- infos$dbname}
  }
  username <- infos$username
  return(list(
    profile = profile,
    db.version = db.version,
    dbname = dbname,
    username = username,
    host = host,
    port = port
  ))
})

#                         //dbListFields ---------------------------------------

#' List columns of a dataset
#' 
#' List column names of a remote dataset.
#' @param conn An object returned by \code{\link[=dbConnect,SASEGDriver-method]{dbConnect()}}.
#' @param name A character string with the name of a dataset (one-level or 
#'     two-levels names). One-level dataset names are interpreted as \code{WORK} 
#'     library datasets.
#' @param ... Other parameters passed on. Not used.
#' @return A character vector.
#' @seealso Generic: \code{\link[DBI]{dbListFields}}.
#' @family SASEGConnection class methods
#' @export
setMethod("dbListFields", c("SASEGConnection", "character"), function(conn, name, ...) {
  name <- dbQuoteIdentifier(conn, name)
  stopifnot(length(name) == 1)
  name <- dataset(name)
  statement <- paste(
    'SELECT name',
    'FROM DICTIONARY.COLUMNS',
    paste0(
      "WHERE libname=", 
      dbQuoteString(conn, stringr::str_to_upper(name@name[1])), 
      " AND memname=", 
      dbQuoteString(conn, stringr::str_to_upper(name@name[2]))
      ),
    sep = "\n"
  )
  d <- dbGetQuery(conn, statement, codeName = NULL, persistent = FALSE, send.dbms = FALSE)
  
  as.character(d$name)
})

#                         //dbListTables ---------------------------------------

#' List datasets available on a SAS server
#' 
#' List datasets available on a \code{SAS} server.
#' @param conn An object returned by \code{\link[=dbConnect,SASEGDriver-method]{dbConnect()}}.
#' @param ... Other parameters passed on. Not used.
#' @return A character vector.
#' @seealso Generic: \code{\link[DBI]{dbListTables}}.
#' @family SASEGConnection class methods
#' @export
setMethod("dbListTables", "SASEGConnection", function(conn, ...) {
  statement <- paste0(
    'SELECT libname, memname\n',
    'FROM DICTIONARY.TABLES'
    )
  d <- dbGetQuery(conn, statement, codeName = NULL, persistent = FALSE, send.dbms = FALSE)
  paste(d$libname, d$memname, sep = ".")
})

#                         //dbListResults --------------------------------------

#' List results
#' 
#' List valid \code{\linkS4class{SASEGSQLResult}} objects.
#' @inheritParams dbListTables,SASEGConnection-method
#' @return A list of \code{\linkS4class{SASEGSQLResult}} objects.
#' @seealso Generic: \code{\link[DBI]{dbListResults}}.
#' @family SASEGConnection class methods
#' @export
setMethod("dbListResults", "SASEGConnection", function(conn, ...) {
  listResults <- conn@infos$listResults
  lapply(ls(listResults), function(x) get(x, envir = listResults))
})

#                         //dbDataType -----------------------------------------

#' Find the SAS data type associated with an R object
#' 
#' Methods are implemented for \code{\linkS4class{SASEGDriver}} and 
#'     \code{\linkS4class{SASEGConnection}} classes.
#' @rdname dbDataType-SASEGDriver-method
#' @inheritParams dbDataType,SASEGDriver-method
#' @export
setMethod("dbDataType", "SASEGConnection", function(dbObj, obj, ...) {
  dbDataType(drv(dbObj), obj)
})

#                         //dbExistsTable --------------------------------------

#' Does a dataset exist on a SAS server ?
#' 
#' \code{dbExistsTable} is used to test if a dataset exists on a \code{SAS} server.
#' @inheritParams dbListFields,SASEGConnection,character-method
#' @return A logical.
#' @seealso Generic: \code{\link[DBI]{dbExistsTable}}.
#' @family SASEGConnection class methods
#' @export
setMethod("dbExistsTable", "SASEGConnection", function(conn, name, send.dbms = TRUE, ...) {
  name <- dbQuoteIdentifier(conn, name)
  stopifnot(length(name) == 1)
  
  if(length(getDBMS(conn)) == 0 || send.dbms == FALSE) {
    name <- dataset(name)
    statement <- paste(
      'SELECT COUNT(*) AS value',
      'FROM DICTIONARY.TABLES',
      paste0(
        "WHERE libname=", 
        dbQuoteString(conn, stringr::str_to_upper(name@name[1])), 
        " AND memname=", 
        dbQuoteString(conn, stringr::str_to_upper(name@name[2]))
        ),
      sep = "\n"
      )
    d <- dbGetQuery(conn, statement, codeName = NULL, persistent = FALSE, send.dbms = FALSE)
    
    return(as.logical(d$value))
  } else {
    statement <- paste("SELECT 1 FROM", name, "LIMIT 1;")
    f <- function(conn, statement) {
      d <- dbGetQuery(conn, statement, codeName = NULL, persistent = FALSE, send.dbms = TRUE)
      if(nrow(d) == 0) return(FALSE) else return(TRUE)
    }
    tryCatch(f(conn = conn, statement = statement), error = function(e) FALSE)
  }
})

#                         //dbRemoveTable --------------------------------------

#' Remove a dataset on a SAS server
#' 
#' \code{dbRemoveTable} method is used to drop a dataset.
#' @inheritParams dbListFields,SASEGConnection,character-method
#' @inheritParams dbSendQuery,SASEGConnection,character-method
#' @return \code{TRUE}, invisibly.
#' @seealso Generic: \code{\link[DBI]{dbRemoveTable}}.
#' @family SASEGConnection class methods
#' @export
setMethod(
  "dbRemoveTable", 
  "SASEGConnection", 
  function(conn, name, codeName = NULL, persistent = TRUE, send.dbms = TRUE, ...) {
    name <- dbQuoteIdentifier(conn, name)
    stopifnot(length(name) == 1)
    if(!dbExistsTable(conn, name, send.dbms = send.dbms)) stop("cannot remove dataset; dataset does not exist.", call. = FALSE)
    name <- dbQuoteIdentifier(conn, dataset(name))
    statement <- paste0("DROP TABLE ", name)
    # la ligne ci-dessous ne fonctionne que dans le cas de send.dmbs=FALSE, il faut modifier
    dbExecute(conn, statement, codeName = codeName, persistent = persistent, send.dbms = send.dbms)
    
    invisible(TRUE)
})


# SAS Results Class ------------------------------------------------------------
#                  /definition, getters and setters ----------------------------

#' SASEGResult class
#' 
#' \code{SASEGResult} class inherits from \code{\link[DBI]{DBIResult-class}}. 
#'    This class represents results of \emph{any} \code{SAS} program. A 
#'    \code{SASEGResult} object results from a call to 
#'    \code{\link[=dbSendQuery,SASEGConnection,SAS-method]{dbSendQuery}} with a 
#'    \code{\linkS4class{SAS}} statement.  It is extended by 
#'    \code{\linkS4class{SASEGSQLResult}} for \code{\link[DBI]{SQL}} statements.
#' 
#' @slot conn An object of class \code{\linkS4class{SASEGConnection}}.
#' @slot SASResult An object of class \code{\linkS4class{SASEGCode}}.
#' @slot fetched A closure.
#' @slot isValid A closure.
#' @seealso \code{\linkS4class{SASEGSQLResult}}, 
#'     \code{\link[=dbSendQuery,SASEGConnection,SAS-method]{dbSendQuery}}
#' @keywords internal
setClass("SASEGResult",
         contains = "DBIResult",
         slots = list(conn = "SASEGConnection",
                      SASResult = "SASEGCode", 
                      fetched = "function", 
                      isValid = "function"
                      )
         )

#' Set the slot fetched as...
#' 
#' \code{`fetched<-`} sets the slot \code{fetched} as a value.
#' @param res A result object.
#' @param value A value.
#' @keywords internal
setGeneric("fetched<-", function(res, value) standardGeneric("fetched<-"))

#' @section Replacement methods:
#' \code{`fetched<-`} sets the slot \code{fetched} of a \code{SASEGResult} 
#'     object as \code{TRUE} or \code{FALSE}. This method is not exported. Only 
#'     developpers may need to use it.
#' @param dbObj,obj,res An object of class \code{SASEGResult} or inheriting from this 
#'     class, as \code{\linkS4class{SASEGSQLResult}}.
#' @param value A logical.
#' @rdname SASEGResult-class
#' @keywords internal
setMethod("fetched<-", "SASEGResult", function(res, value) {
  res@fetched(set = value)
  return(res)
})

#' @section Replacement methods:
#' \code{`isValid<-`} method sets the value \code{isValid}. This method is not 
#'     exported. Only developpers may need to use it. 
#' @inheritParams fetched-set,SASEGResult-method
#' @rdname SASEGResult-class
#' @keywords internal
setMethod("isValid<-", "SASEGResult", function(obj, value) {
  obj@isValid(set = value)
  return(obj)
}) 

# Connection Class -------------------------------------------------------------
#                 /methods -----------------------------------------------------
#                         //dbSendQuery (SAS query)-----------------------------


#' Send a SAS query to SAS EG
#' 
#' This method sends a \code{SAS} query to \code{SAS EG}.
#' 
#' Statements with class \code{\linkS4class{SAS}} are directly sent to the 
#'     \code{SAS} server through \code{SAS EG} escaping any code transformation. 
#' @param conn A \code{SASEGConnection} object.
#' @param statement A \code{\linkS4class{SAS}} object.
#' @param codeName A character string to name the new \code{\linkS4class{SASEGCode}} object.
#' @param persistent A logical. If \code{TRUE}, a new 
#'     \code{\linkS4class{SASEGCode}} is created in the \code{SASEGProject}. If 
#'     \code{FALSE}, \code{garbage} code is used.
#' @param ... Other parameters passed on to methods. Not used.
#' @return A \code{\linkS4class{SASEGResult}} object.
#' @keywords internal
#' @export
setMethod(
  "dbSendQuery", 
  c("SASEGConnection", "SAS"), 
  function(conn, statement, codeName = NULL, persistent = TRUE, ...) {
    if(persistent) {
      # Create a new SAS EG Code object with server and SAS program: 
      SASCode <- newCode(project(conn), 
                         server = server(conn), 
                         program = statement, 
                         name = codeName
                        )
    } else {
      # Use temporary SAS Code
      SASCode <- getUtil(conn)
      setText(SASCode, paste0(noteUtil, statement))
    }
    # Execute SAS program:
    run(SASCode)
    # Only for debugging:
    # cat(getSourceCode(SASCode))
    res <- new("SASEGResult",
               conn = conn,
               SASResult = SASCode,
               fetched = state_generator(init = FALSE),
               isValid = state_generator(init = TRUE)
               )
    if(countOutputDatasets(SASCode) == 0) {fetched(res) <- TRUE}
    return(res)
  }
)

# SAS Results Class ------------------------------------------------------------
#                  /methods ----------------------------------------------------
#                          //dbIsValid -----------------------------------------

#' Is a SASEGResult object valid?
#' 
#' \code{dbIsValid} method tests if a \code{SASEGResult} is valid.
#' \code{TRUE} is returned if the object is valid.
#' @inheritParams fetched-set,SASEGResult-method
#' @return \code{dbIsValid} returns a logical.
#' @seealso Generic: \code{\link[DBI]{dbIsValid}}.
#' @rdname SASEGResult-class
#' @export
setMethod("dbIsValid", "SASEGResult", function(dbObj, ...) {
  dbObj@isValid()
})

#                          //dbHascompleted ------------------------------------

#' Is a SASEGResult object fetched?
#' 
#' \code{dbHasCompleted} method is used to test if a result is totally fetched.
#' @inheritParams fetched-set,SASEGResult-method
#' @param ... Other parameters. Not used.
#' @return \code{dbHasCompleted} returns a logical: \code{TRUE}, if all rows are 
#'     fetched; \code{FALSE}, if there is some rows to be fetched.
#' @seealso Generic: \code{\link[DBI]{dbHasCompleted}}.
#' @rdname SASEGResult-class
#' @keywords internal   
#' @export
setMethod("dbHasCompleted", "SASEGResult", function(res, ...) {
  if(!dbIsValid(res))  stop("result is not valid.")
  
  res@fetched()
})

#                          //dbFetchAll ----------------------------------------

#' Fetch all result tables
#' 
#' In case of a program creates multiple tables, fetch all tables.
#' @param res A result object.
#' @param ... Other parameters passed on to method.
setGeneric("dbFetchAll", function(res, ...) standardGeneric("dbFetchAll"))

#' Fetch all results of a SAS program
#' 
#' \code{dbFetchAll} method retrieves all datasets (with all rows) created by a 
#'     \code{SAS} statement. It returns a named list of 
#'     \code{\link[data.table]{data.table}}. 
#' @inheritParams dbHasCompleted,SASEGResult-method
#' @return \code{dbFetchAll} returns a named (with filenames) list of 
#'     \code{\link[data.table]{data.table}}.
#' @rdname SASEGResult-class
#' @export
setMethod("dbFetchAll", "SASEGResult", function(res, ...) {
  l <- getListDatasets(res@SASResult)
  d <- lapply(l, read)
  fetched(res) <- TRUE
  
  d
})

# Connection Class -------------------------------------------------------------
#                 /methods -----------------------------------------------------
#                         //dbGetQuery (SAS query)------------------------------

#' Send a SAS program and get result datasets
#' 
#' \code{dbGetQuery} method is used to send a \code{SAS} program and get a list 
#' of result datasets.
#' 
#' @inheritParams dbSendQuery,SASEGConnection,SAS-method
#' @return \code{dbGetQuery} returns a named (with filenames) list of 
#'     \code{\link[data.table]{data.table}}.
#' @export
setMethod(
  "dbGetQuery", 
  c("SASEGConnection", "SAS"), 
  function(conn, statement, codeName = NULL, persistent = TRUE, ...) {
    res <- dbSendQuery(conn, statement, codeName = codeName, persistent = persistent)
    dbFetchAll(res)
    isValid(res) <- FALSE
})

# SAS Results Class ------------------------------------------------------------
#                  /methods ----------------------------------------------------
#                          //dbGetLog ------------------------------------------

#' Get the log of a program execution
#' 
#' \code{dbGetLog} methods retrieve the log of a program execution.
#' 
#' \code{dbGetLog} does not belong to the \code{DBI} specification.
#' @param res An object.
#' @param ... Other parameters passed on to method.
#' @keywords internal
setGeneric("dbGetLog", function(res, ...) standardGeneric("dbGetLog"))

#' Get the log ater a SAS run
#' 
#' \code{dbGetLog} method retrieves the log of the \code{\linkS4class{SAS}} 
#' program. \code{dbGetLog} does not belong to the \code{DBI} specification.
#' @inheritParams dbHasCompleted,SASEGResult-method
#' @return \code{dbGetLog} returns a character string with the \code{SAS log}.
#' @rdname SASEGResult-class
#' @export
setMethod("dbGetLog", "SASEGResult", function(res, ...) {
  getLog(res@SASResult)
})

#                          //dbGetStatement ------------------------------------

#' Get the statement that was sent to SAS
#' 
#' \code{dbGetStatement} collect the program that was sent to \code{SAS}.
#' @inheritParams dbHasCompleted,SASEGResult-method
#' @return A character string with the \code{SAS} code.
#' @export
setMethod("dbGetStatement", "SASEGResult", function(res, ...) {
  getSourceCode(res@SASResult)
})

# SQL Results Class ------------------------------------------------------------
#                  /definition -------------------------------------------------


#' SASEG PROC SQL results class
#' 
#' \code{SASEGSQLResult} class inherits from \code{\link[DBI]{DBIResult-class}} 
#'     and extends \code{\linkS4class{SASEGResult}}. This class represents 
#'     results of \code{PROC SQL} programs. 
#' @slot SQLResult A character string. This slot contains the filename of the 
#'     dataset created by the \code{ODS} during a \code{PROC SQL}.
#' @slot rowsFetched A closure.
#' @slot SQLRC A data frame with SAS return codes.
#' @slot RCFileName A character string with the name of the temp dataset where 
#'     return codes are stored.
#' @keywords internal
setClass(
  "SASEGSQLResult",
  contains = "SASEGResult",
  slots = list(
    SQLResult = "character",
    statement = "character",
    rowsFetched = "function",
    SQLRC = "data.frame",
    RCFileName = "character"
    )
  )

# Connection Class -------------------------------------------------------------
#                 /methods -----------------------------------------------------
#                         //dbSendQuery (SQL query)-----------------------------

#' Send an SQL query to SAS EG
#'
#' \code{dbSendQuery} sends an \code{\link[DBI]{SQL}} query to \code{SAS}. The 
#'     query is first embedded in a \code{PROC SQL} and sent to \code{SAS}. 
#'     \code{dbSendQuery} is used to send \code{SELECT ...} queries.
#' @param statement A character string containing an \code{SQL} code 
#'     or an \code{\link[DBI]{SQL}} class object.
#' @param query A logical. \code{TRUE} indicates a \code{SELECT} query. 
#'     \code{FALSE} indicate a data transformation statement. 
#' @inheritParams dbSendQuery,SASEGConnection,SAS-method 
#' @return A \code{\linkS4class{SASEGSQLResult}} object.
#' @seealso Generic: \code{\link[DBI]{dbSendQuery}}
#' @export
#' @examples
#' # This is another good place to put examples
setMethod(
  "dbSendQuery", 
  c("SASEGConnection", "character"), 
  function(conn, 
           statement, 
           codeName = NULL, 
           persistent = TRUE, 
           query = TRUE, 
           send.dbms = TRUE, ...) {
    # Keep original statement:
    sql_statement <- statement
    
    if(length(getDBMS(conn)) > 0 && send.dbms) {
      cnx.alias <- "CNX"
      cnx.string <- dbConnectString(conn, cnx.alias = cnx.alias)
      if(query) {
        statement <- paste0(cnx.string, 
                            "SELECT * FROM CONNECTION TO ", cnx.alias, " (\n", 
                            statement, 
                            "\n);\nDISCONNECT FROM ", cnx.alias
                            )
      } else {
        statement <- paste0(cnx.string, 
                            "EXECUTE (\n", 
                            statement, 
                            "\n) BY ", cnx.alias, 
                            ";\nDISCONNECT FROM ", cnx.alias
                            )
      }
    }
    
    if(query) {
      # Choose a new dataset name to store the result of SQL query:
      SQLResult <- paste0("WORK.", random_table_name()) 
    } else {
      # In case of data manipulation SQL statement:  
      SQLResult <- NA_character_
      }
    # Transform SQL statement into a SAS statement:
    statement <- SAS(DBI::SQL(statement), SQLResult = SQLResult)
    
    # In case of a new SAS Code, construct a DATA STEP to retrieve SAS Return Codes:
    if(persistent) {
      # * pick a name:
      RCFileName <- paste0("WORK.", random_table_name())
      # * elaborate the DATA STEP:
      statement_RC <- SAS(
        paste(
          paste0("DATA ", RCFileName, ";"),
          '  sqlexitcode=SYMGETN("sqlexitcode");',
          '  sqlobs=SYMGETN("sqlobs");',
          '  sqloops=SYMGETN("sqloops");',
          '  sqlrc=SYMGETN("sqlrc");',
          '  syserrortext=SYMGET("syserrortext");',
          '  sqlxmsg=SYMGET("sqlxmsg");',
          '  sqlxrc=SYMGET("sqlxrc");',
          'RUN;',
          sep = "\n"
          )
        )
    } else {
      RCFileName <- NA_character_
      statement_RC <- ""
    }
    # Merge with main statement:
    statement <- SAS(paste(statement, statement_RC, sep = "\n\n"))
    # Run SAS statement and get a SASEGResult object:
    res <- dbSendQuery(conn, statement, codeName = codeName, persistent = persistent)
    # Get the list of output datasets:
    l <- getListDatasets(res@SASResult)
    # Read the SAS return codes:
    if(!is.na(RCFileName)) {
      SQLRC <- data.frame(read(l[[RCFileName]]))
      if(SQLRC$sqlrc > 0 && SQLRC$sqlrc < 8) warning("SAS/PROC SQL warning: ", SQLRC$syserrortext)
      if(SQLRC$sqlrc >= 8) {
        conn@infos$sqlrc <- SQLRC$sqlrc
        conn@infos$syserrortext <- SQLRC$syserrortext
        log <- dbGetLog(res)
        stop("SAS/PROC SQL - ", SQLRC$syserrortext, "\n SAS Log:\n", log, call. = FALSE)
      }
      # In case of SAS/ACCESS SQL pass-through, retrieve the return codes sent by the DBMS:
      if(length(getDBMS(conn)) > 0) {
        if(SQLRC$sqlxrc > 0) {
          conn@infos$sqlrc <- SQLRC$sqlxrc
          conn@infos$syserrortext <- as.character(SQLRC$sqlxmsg)
          log <- dbGetLog(res)
          stop(getDBMS(conn), " SQL - ", as.character(SQLRC$sqlxmsg), "\n SAS Log:\n", log, call. = FALSE)
        }
      }
    } else {
      SQLRC <- data.frame(NULL)
    }
    # In case of end user made a "mistake" (ie. sending a data manipulation 
    #   statement with dbSendQuery instead of dbSendStatement), correct the 
    #   value of SQLResult:
    if(!SQLResult %in% names(l)) {SQLResult <- NA_character_}
    #  If a data manipulation statement was sent, turn result as fetched:
    if(is.na(SQLResult)) {fetched(res) <- TRUE}
    # Construct a SASEGSQLResult object:
    res_sql <- new("SASEGSQLResult",
                   res,
                   SQLResult = SQLResult,
                   statement = as.character(sql_statement),
                   rowsFetched = count_generator(init = 0),
                   SQLRC = SQLRC,
                   RCFileName = RCFileName
                   )
    assign(random_table_name(), res_sql, envir = conn@infos$listResults)
    
    return(res_sql)
  }
)

#                         //dbSendStatement ------------------------------------

#' Send an SQL statement to SAS EG
#'
#' \code{dbSendStatement} sends an \code{\link[DBI]{SQL}} data manipulation 
#'     statement to \code{SAS}. The query is first embedded in a 
#'     \code{PROC SQL} and sent to \code{SAS}. 
#' @inheritParams dbSendQuery,SASEGConnection,character-method
#' @seealso \code{\link[=dbSendQuery,SASEGConnection,character-method]{dbSendQuery()}}
#' @seealso Generic: \code{\link[DBI]{dbSendStatement}}
#' @return A \code{\linkS4class{SASEGSQLResult}} object.
#' @export
setMethod(
  "dbSendStatement", 
  c("SASEGConnection", "character"), 
  function(conn, statement, codeName = NULL, persistent = TRUE, send.dbms = TRUE, ...) {
  dbSendQuery(conn, 
              statement, 
              codeName = codeName, 
              persistent = persistent, 
              query = FALSE, 
              send.dbms = send.dbms, ...)
})

#                         //dbGetQuery -----------------------------------------

#' Send an SQL query to SAS EG and get results
#'
#' \code{dbGetQuery} sends an \code{\link[DBI]{SQL}} query to \code{SAS} and
#'     retrieve results. The query is first embedded in a \code{PROC SQL} and 
#'     sent to \code{SAS}. \code{dbGetQuery} is used to send \code{SELECT ...} 
#'     queries.
#' @inheritParams dbSendQuery,SASEGConnection,character-method
#' @return A \code{data.frame} object.
#' @export
setMethod(
  "dbGetQuery", 
  c("SASEGConnection", "character"), 
  function(conn, statement, codeName = NULL, persistent = TRUE, send.dbms = TRUE, ...) {
    res <- dbSendQuery(conn, 
                       statement, 
                       codeName = codeName, 
                       persistent = persistent, 
                       query = TRUE,
                       send.dbms = send.dbms)
    on.exit(dbClearResult(res))
    d <- dbFetch(res, n = -1)
    return(d)
  })

#                         //dbWriteTable ---------------------------------------

#' Copy data frames to SAS datasets
#' 
#' Writes, overwrites or appends a data frame to a \code{SAS} dataset, 
#' optionally converting row names to a column and specifying \code{SAS} data
#' types for fields.
#' @param conn An object returned by 
#'     \code{\link[=dbConnect,SASEGDriver-method]{dbConnect()}}.
#' @param name A character string specifying the unquoted dataset name, or the 
#'     result of a call to 
#'     \code{\link[=dbQuoteIdentifier,SASEGConnection,character-method]{dbQuoteIdentifier()}}
#' @param overwrite A logical. If \code{TRUE}, an existing dataset of the same 
#'     name will be overwritten.
#' @param append A logical. If \code{TRUE}, the rows in an existing table are 
#'     preserved, and the new data are appended.
#' @param field.types A named character vector with \code{SAS} data types.
#' @param temporary A logical. This is part of the \code{DBI} specification. 
#'     However, this functionality is not supported: one-level named datasets
#'     are created in \code{SAS WORK}. Use two-levels names to permanently save
#'     a dataset.
#' @inheritParams dbSendStatement,SASEGConnection,character-method
#' @inheritParams DBI::dbWriteTable
#' @inheritParams DBI::sqlCreateTable
#' @return \code{TRUE}, invisibly.
#' @seealso Generic: \code{\link[DBI]{dbWriteTable}}.
#' @export
setMethod(
  "dbWriteTable", 
  "SASEGConnection", 
  function(conn, 
           name, 
           value, 
           row.names = NA, 
           overwrite = FALSE, 
           append = FALSE,
           field.types = NULL,
           temporary = FALSE,
           persistent = TRUE, 
           ...) {
    stopifnot(is.data.frame(value))
    stopifnot(length(row.names) == 1)
    stopifnot(is.null(row.names) || is.logical(row.names) || is.character(row.names))
    stopifnot(length(overwrite) == 1)
    stopifnot(is.logical(overwrite))
    stopifnot(length(append) == 1)
    stopifnot(is.logical(append))
    stopifnot(length(temporary) == 1)
    stopifnot(is.logical(temporary))
    if(!is.null(field.types)) {
      stopifnot(length(field.types) == length(value))
      stopifnot(!is.null(names(field.types)))
      stopifnot(names(field.types) == names(value))
    }
    
    if(overwrite && append) stop("overwrite and append cannot be both TRUE.", call. = FALSE)
    
    quoted_name <- dbQuoteIdentifier(conn, name)
    stopifnot(length(quoted_name) == 1)
    
    table_name <- dataset(quoted_name)
    libref <- stringr::str_to_upper(table_name@name[1])
    if(temporary && libref != "WORK") {
      stop("dataset ", table_name@name[2], 
           " cannot be temporary copied in library ", table_name@name[1], 
           call. = FALSE
      )
    }
    
    # Non-temporary one-level named datasets raise a warning.
    # But they are copied in WORK library. 
    if(!temporary && libref == "WORK") {
      warning("Dataset ", table_name@name[2], " is created in ", libref, " library.\n",
              "Use a two-level name to create non-temporary dataset.")
    }
    
    quoted_name <- dbQuoteIdentifier(conn, table_name)
    
    exist <- dbExistsTable(conn, quoted_name)
    if(exist && !append && !overwrite) {
      stop("dataset ", table_name@name[2], 
           " already exists in library ", table_name@name[1], 
           call. = FALSE
      )
    }
    
    if(exist && overwrite) dbRemoveTable(conn, quoted_name, persistent = FALSE, send.dbms = FALSE)
    
    if(!exist || overwrite) {
      if(is.null(field.types)) {
        statement <- DBI::sqlCreateTable(con = conn, 
                                         table = quoted_name, 
                                         fields = value, 
                                         row.names = row.names, 
                                         temporary = FALSE
                                         )
        
      } else {
        statement <- DBI::sqlCreateTable(con = conn, 
                                         table = quoted_name, 
                                         fields = field.types, 
                                         row.names = row.names, 
                                         temporary = FALSE
                                         )
      }
      
      dbExecute(conn, 
                statement, 
                codeName = if(persistent) paste("Create dataset", name) else NULL, 
                persistent = persistent,
                send.dbms = FALSE
                )
    }
    
    if(nrow(value) > 0) {
      statement <- sqlAppendTable(con = conn, 
                                  table = quoted_name, 
                                  values = value, 
                                  row.names = row.names, 
                                  ...)
      dbExecute(conn, 
                statement, 
                codeName = if(persistent) paste("Insert values to dataset", name) else NULL, 
                persistent = persistent,
                send.dbms = FALSE
                )
    }
    
    invisible(TRUE)
  })

#                         //dbReadTable ----------------------------------------

#' Copy data frames from SAS dataset
#' 
#' Reads a \code{SAS} dataset to a data frame, optionally converting a column 
#' to row names and converting the column names to valid \code{R} identifiers.
#' 
#' @inheritParams DBI::sqlColumnToRownames
#' @inheritParams dbWriteTable,SASEGConnection-method
#' @param check.names A logical. Check if column names are valid \code{R} identifiers.
#' @seealso Generic: \code{\link[DBI]{dbReadTable}}.
#' @export
setMethod(
  "dbReadTable", 
  c("SASEGConnection", "character"), 
  function(conn, name, ..., row.names = NA, check.names = TRUE) {
    quoted_name <- dbQuoteIdentifier(conn, name)
    stopifnot(length(quoted_name) == 1)
    
    stopifnot(length(row.names) == 1L)
    stopifnot(is.null(row.names) || is.logical(row.names) || is.character(row.names))
    stopifnot(length(check.names) == 1L)
    stopifnot(is.logical(check.names))
    stopifnot(!is.na(check.names))
    stopifnot(dbExistsTable(conn, name))
    
    quoted_name <- dbQuoteIdentifier(conn, dataset(quoted_name))
    statement <- paste("SELECT * FROM", quoted_name)
    d <- dbGetQuery(conn, statement, codeName = NULL, persistent = FALSE, send.dbms = TRUE)
    d <- DBI::sqlColumnToRownames(d, row.names)
    
    if (check.names) {
      names(d) <- make.names(names(d), unique = TRUE)
    }
    d
  })


# SQL Results Class ------------------------------------------------------------
#                  /methods ----------------------------------------------------
#                          //dbGetRowCount -------------------------------------

#' Get the number of fetched rows
#' 
#' \code{dbGetRowCount} returns the number of rows that was fetched.
#' @param res An object resulting from a call to 
#'     \code{\link[=dbSendQuery,SASEGConnection,character-method]{dbSendQuery}}.
#' @param ... Other parameters passed on to method. Not used.
#' @return Number of fetched rows.
#' @seealso Generic: \code{\link[DBI]{dbGetRowCount}}.
#' @export
setMethod("dbGetRowCount", "SASEGSQLResult", function(res, ...) {
  res@rowsFetched()
})

#                          //dbGetRowsAffected ---------------------------------

#' Get the number of rows affected
#' 
#' \code{dbGetRowsAffected} returns the number of rows that were added, deleted,
#' or updated by a data manipulation statement.
#' @param res An object resulting from a call to 
#'     \code{\link[=dbSendStatement,SASEGConnection,character-method]{dbSendStatement}}.
#' @param ... Other parameters passed on to method. Not used.
#' @return Number of rows affected.
#' @seealso Generic: \code{\link[DBI]{dbGetRowsAffected}}.
#' @export
setMethod("dbGetRowsAffected", "SASEGSQLResult", function(res, ...) {
  as.numeric(res@SQLRC$sqlobs)
})

#                          //dbGetStatement ------------------------------------

#' Get the statement associated with a result object
#' 
#' \code{dbGetStatement} returns the statement that was passed on to 
#'     \code{\link[=dbSendQuery,SASEGConnection,character-method]{dbSendQuery}} or
#'     \code{\link[=dbSendStatement,SASEGConnection,character-method]{dbSendStatement}}.
#' @param res An object resulting from a call to 
#'     \code{\link[=dbSendQuery,SASEGConnection,character-method]{dbSendQuery}} or
#'     \code{\link[=dbSendStatement,SASEGConnection,character-method]{dbSendStatement}}.
#' @param ... Other parameters passed on to method. Not used.
#' @return A character string.
#' @seealso Generic: \code{\link[DBI]{dbGetStatement}}
#' @export
setMethod("dbGetStatement", "SASEGSQLResult", function(res, ...) {
  res@statement
})

#                          //dbClearResult -------------------------------------

#' Clear a result object
#' 
#' \code{dbClearResult} method drops temporary datasets created by a call to 
#' \code{\link[=dbSendQuery,SASEGConnection,character-method]{dbSendQuery}} or
#' \code{\link[=dbSendStatement,SASEGConnection,character-method]{dbSendStatement}}.
#' @inheritParams dbGetStatement,SASEGSQLResult-method
#' @return \code{TRUE}, invisibly.
#' @seealso Generic: \code{\link[DBI]{dbClearResult}}
#' @export
setMethod("dbClearResult", "SASEGSQLResult", function(res, ...) {
  if(!dbIsValid(res)) warning("result object already closed.", call. = FALSE)
  on.exit(dbClearListResults(res@conn))
  # If RCFileName dataset exists, drop it:
  if(!is.na(res@RCFileName)) {
    statement <- paste0("DROP TABLE ", res@RCFileName, ";\n")
  } else {
    statement <- ""
  }
  # If SQLResult dataset exists, drop it:
  if(!is.na(res@SQLResult)) {
    statement <- paste0(statement, "DROP TABLE ", res@SQLResult)
  } else {
    statement <- paste0(statement, "")
  } 
  # If necessary, run drop statement:
  if(any(!is.na(c(res@RCFileName, res@SQLResult)))) dbExecute(res@conn, statement, codeName = NULL, persistent = FALSE, send.dbms = FALSE)
  # In all cases, set isValid to FALSE:
  isValid(res) <- FALSE

  invisible(TRUE)
})

#                          //dbFetch -------------------------------------------

#' Retrieve records from a SQL query
#' 
#' Fetch the next \code{n} rows from the result of a 
#' \code{\link[=dbSendQuery,SASEGConnection,character-method]{dbSendQuery}} and
#' return them as a \code{data.frame}.
#' @inheritParams dbGetRowCount,SASEGSQLResult-method
#' @param n Number of rows to retrieve. Use \code{n = -1} or \code{n = Inf} to
#'     retrieve all pending rows.
#' @return A \code{data.frame} object.
#' @seealso Generic: \code{\link[DBI]{dbFetch}}
#' @export
setMethod("dbFetch", "SASEGSQLResult", function(res, n = -1, ...) {
  if(!length(n) == 1) stop("Argument n must be an atomic vector.", call. = FALSE)
  if(!any(n%%1 == 0, is.infinite(n))) stop("Argument n must be a whole number.", call. = FALSE)
  if(n < -1) stop("Argument n must be greater or equal to -1.", call. = FALSE)
  if(!dbIsValid(res)) stop("cannot fetch a closed result object.")
  # If object res was created by a SQL statement then SQLResult slot is NA:
  if(is.na(res@SQLResult)) {
    warning("No table to fetch: check that there is no error in your SQL code.")
    return(data.frame(NULL))
    }
  if(dbHasCompleted(res)) message("No row to fetch: an empty data.frame is returned.") 
  # Store in a variable whether all remaining rows are claimed:
  allrows <- (n == -1 || is.infinite(n))
  
  if(allrows && dbGetRowCount(res) == 0) {
    # Get all datasets created by the SAS Statement
    # This list can contain other datasets
    l <- getListDatasets(res@SASResult)
    # Read only the dataset pointed by SQLResult
    d <- data.frame(read(l[[res@SQLResult]]))
    if(nrow(d) > 0) d[is.voidstring(d)] <- NA
    fetched(res) <- TRUE
  } else {
    n_min <- dbGetRowCount(res)+1
    n_max <- dbGetRowCount(res)+n
    if(n == 0) {
      n_min <- 0
      n_max <- 0
    }
    if(allrows || dbHasCompleted(res)) {
      condition <- paste0("GE ", n_min)
    } else {
      condition <- paste0("BETWEEN ", n_min, " AND ", n_max)
    }
    query <- paste0("SELECT * FROM ", res@SQLResult, "\n",
                    "WHERE MONOTONIC() ", condition)
    d <- dbGetQuery(res@conn, query, codeName = NULL, persistent = FALSE, send.dbms = FALSE)
    if(nrow(d) < n || allrows) {fetched(res) <- TRUE}
  }
  res@rowsFetched(add = nrow(d))
  return(d)
})

#                          //dbColumnInfo --------------------------------------

#' Get information about result types
#' 
#' \code{dbColumnInfo} produces a \code{data.frame} that describes the result 
#' of a query. Each row of the \code{data.frame} describes a column of the
#' result dataset and columns in the \code{data.frame} describes the following
#' aspects of the result dataset:
#' \itemize{
#'    \item \code{name}: column name (case as-is from query).
#'    \item \code{field.type}: \code{SAS} column type (char or num).
#'    \item \code{field.format}: \code{SAS} column format.
#'    \item \code{field.informat}: \code{SAS} column informat.
#'    \item \code{notnull}: not \code{NULL} ?
#'    \item \code{precision}: precision.
#'    \item \code{scale}: scale.}
#' @inheritParams dbGetRowCount,SASEGSQLResult-method
#' @return A \code{data.frame}.
#' @export
setMethod("dbColumnInfo", "SASEGSQLResult", function(res, ...) {
  if(is.na(res@SQLResult)) return(data.frame(NULL))
  splitted <- strsplit(res@SQLResult, ".", fixed = TRUE)[[1]]
  libname <- splitted[1]
  tablename <- splitted[2]
  statement <- paste(
    'SELECT name,',
    '       type AS "field.type",',
    '       format AS "field.format",',
    '       informat AS "field.informat",',
    '       notnull,',
    '       precision,',
    '       scale',
    'FROM DICTIONARY.COLUMNS',
    paste0("WHERE libname='", libname, "' AND memname='", tablename, "'"),
    sep = "\n"
  )
  d <- dbGetQuery(res@conn, statement, codeName = NULL, persistent = FALSE, send.dbms = FALSE)
  
  SAS2RDataType(d)
})



