#' @import DBI
#' @import methods
#' @importFrom stringr str_to_upper
#' @include SASEGS4.R
#' @include SASEGDataType.R
#' @include utils.R
NULL


# Driver Class ------------------------------------------------------------


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

#' Access to slot app of an object
#' 
#' Method to access a slot named \code{app}.
#' @param obj An object with a slot named \code{app}.
#' @param ... Other parameters passed to method.
#' @keywords internal
#' @seealso \code{\link{app,SASEGDriver-method}}, 
#'     \code{\link{app,SASEGConnection-method}}
setGeneric("app", function(obj, ...) standardGeneric("app"))

#' Access to slot app of a SASEGDriver object
#' 
#' \code{app()} access to the slot \code{app} of a \code{SASEGDriver} object. 
#'     This method is not exported. Only developpers may need to use it.
#' @param obj A \code{SASEGDriver} object.
#' @param ... Other parameters passed to method. Not used.
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

#' Get informations for a SASEGDriver object.
#' 
#' \code{dbGetInfo} method is called to get:
#' \itemize{
#'     \item \code{driver.version}: version of \code{RSASEG} package.
#'     \item \code{client.version}: version of \code{SAS Enterprise Guide}.
#'     \item \code{max.connections}: equals to 1.
#'     \item \code{available.profiles}: a list with informations about 
#'         available profiles.
#' }
#' @rdname SASEGDriver-class
#' @param dbObj A \code{SASEGDriver} object.
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
#' dbGetInfo(drv)}
#' @keywords internal
#' @export
setMethod("dbGetInfo", "SASEGDriver", function(dbObj, ...) {
  if(!dbIsValid(dbObj)) stop("Invalid driver")
  
  list(
    "driver.version" = packageDescription("RSASEG")$Version,
    "client.version" = getVersion(app(dbObj)),
    "max.connections" = 1,
    "available.profiles" = getListAvailableProfiles(app(dbObj))
  )
})

#' A driver for SAS Enterprise Guide
#' 
#' \code{SASEG()} generates a new \code{SASEG} driver. You 
#'     have to provide location of file \code{SASEGScripting.dll} following 
#'     your installation of \code{SAS Enterprise Guide}. A \code{SASEGDriver} 
#'     object can be understood as a \code{SAS EG} application.
#' @param DLLPath A character string with the path to file \code{SASEGScripting.dll}.
#' @return \code{SASEG()} returns a \code{\linkS4class{SASEGDriver}} object.
#' @export
SASEG <- function(DLLPath = "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll") {
  if(!file.exists(DLLPath)) stop("Cannot find file ", DLLPath)
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

#' List active SAS EG connections
#' 
#' \code{dbListConnections} method is used to retrieve the list of active 
#'     connections. Since \code{SASEGDriver} allows a single connection, the 
#'     list contains either a single element or is empty.
#' @param drv An object created by a call to \code{SASEG()}.
#' @param ... Other parameters. Not used.
#' @return \code{dbListConnections} returns an empty list if there is no active
#'     connection or a list with a single \code{\linkS4class{SASEGConnection}} 
#'     object.
#' @rdname SASEG
#' @export
setMethod("dbListConnections", "SASEGDriver", function(drv, ...) {
  if(!dbIsValid(drv)) stop("invalid driver.")
  
  drv@cnx$get()
})

#' Is this SASEGDriver still valid ?
#' 
#' \code{dbIsValid} tests if a \code{SASEGDriver} object is still valid. A call
#'     to \code{dbUnloadDriver} invalidates the object.
#' @param dbObj An object created by a call to \code{SASEG()}.
#' @inheritParams dbListConnections,SASEGDriver-method
#' @return \code{dbIsValid} returns a \code{logical}.
#' @rdname SASEG
#' @export
setMethod("dbIsValid", "SASEGDriver", function(dbObj, ...) {
  dbObj@isValid()
})

#' Unload SASEGDriver
#' 
#' \code{dbUnloadDriver} usage is recommended for \code{SASEGDriver} objects. 
#'     Since this method is deprecated, a finalizer is registered upon garbage
#'     collector and at the end of the \code{R} session. The finalizer sends an 
#'     informative message to the user.
#' @inheritParams dbListConnections,SASEGDriver-method
#' @return \code{dbUnloadDriver} method returns \code{TRUE}.
#' @rdname SASEG
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

setMethod("show", "SASEGDriver", function(object) {
  if(dbIsValid(object)) {
    cat("<SASEGDriver>\n")
    show(app(object))
  } else {
    warning("Invalid driver")
  }
})

#' Find the SAS data type associated with an R object
#' 
#' \code{dbDataType} method finds the \code{SAS} data type associated with an \code{R} object.
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
#' @keywords internal
#' @export
setMethod("dbDataType", "SASEGDriver", function(dbObj, obj, ...) {
  getSASType(obj)
})


# SAS Class and Methods ---------------------------------------------------


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
#' @param ... Other parameters passed to methods.
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

#' @export
dataset <- function(name, libname = "WORK") {
  if(length(libname) > 1 | length(name) > 1) stop("You must provide atomic libname/name.")
  name <- strsplit(name, ".", fixed = TRUE)[[1]]
  if(length(name) > 2) stop('name argument cannot contain more than one "."')
  if(length(name) == 2) return(new("Table", name = name))
  if(grepl(pattern = ".", x = libname, fixed = TRUE)) stop('Libname cannot contain "."')
  if(is.null(libname)) {
    warning("Null libname, WORK is provided as libname.", immediate. = TRUE)
    libname <- "WORK"
  }
  return(new("Table", name = c(as.character(libname), as.character(name))))
}


# Connection Class and Methods --------------------------------------------


#' SAS EG connection class
#'
#' This class inherits from \code{\link[DBI]{DBIConnection-class}}.
#' An object of class \code{SASEGConnection} can be understood a \code{SAS EG} project.
#' @exportClass SASEGConnection
#' @slot infos An environment that reference all slots. This environment is 
#'     registered. Referenced objects are: drv (the SASEGDriver used to connect to), 
#'     isValid (a logical), profile (a character string), server (a character 
#'     string), SASProject (a SASEGProject object), SASUtil (a SASEGCode object), 
#'     listResults (an environment), dbms (a character string)
#' @keywords internal
setClass("SASEGConnection",
         contains = "DBIConnection",
         slots = list(infos = "environment")
         )

#' Access to slot drv of an object
#' 
#' A generic accessor to a slot named \code{drv}.
#' @param obj An object.
#' @param ... Other parameters passed to method.
#' @keywords internal
setGeneric("drv", function(obj, ...) standardGeneric("drv"))

#' Access to slot drv of a SASEGConnection object
#' 
#' \code{drv()} method access to the slot \code{drv} of a 
#'     \code{\linkS4class{SASEGConnection}} object.
#' @param obj An object of class \code{\linkS4class{SASEGconnection}}.
#' @param ... Other parameters passed to method. Not used.
#' @return A \code{\linkS4class{SASEGDriver}} object.
#' @keywords internal
setMethod("drv", "SASEGConnection", function(obj, ...) {
  env <- obj@infos
  env$drv
})

#' Test if SAS EG connection is valid
#' 
#' \code{dbIsValid} tests if a \code{\linkS4class{SASEGConnection}} object is valid.
#' @param dbObj An object of class \code{\linkS4class{SASEGConnection}}.
#' @param ... Other parameters. Not used.
#' @keywords internal
#' @export
setMethod("dbIsValid", "SASEGConnection", function(dbObj, ...) {
  env <- dbObj@infos
  env$isValid
})

#' Set the value isValid of a SASEGConnection object
#' 
#' Value stored in slot \code{isValid} can be replaced with this method. This 
#'     method is not exported. Only developpers may need to use it. 
#' @param obj A \code{\linkS4class{SASEGDriver}} object.
#' @param value A logical.
#' @seealso dbIsValid,SASEGConnection-method
#' @keywords internal
setMethod("isValid<-", "SASEGConnection", function(obj, value) {
  env <- obj@infos
  env$isValid <- value
  return(obj)
}) 

#' Access to the SASEGApplication object stored in a SASEGConnection object
#' 
#' \code{app()} method access to the \code{\linkS4class{SASEGApplication}} 
#'     object stored in a \code{\linkS4class{SASEGConnection}} object. This 
#'     method is not exported. Only developpers may need to use it. 
#' @param obj An object of class \code{\linkS4class{SASEGConnection}}.
#' @param ... Other parameters passed to method. Not used.
#' @return A \code{\linkS4class{SASEGApplication}} object.
#' @keywords internal
setMethod("app", "SASEGConnection", function(obj, ...) {
  app(drv(obj))
})

setGeneric("getProfile", function(obj) standardGeneric("getProfile"))

setMethod("getProfile", "SASEGConnection", function(obj) {
  env <- obj@infos
  env$profile
})

setGeneric("server", function(obj) standardGeneric("server"))

setMethod("server", "SASEGConnection", function(obj) {
  env <- obj@infos
  env$server
})

setGeneric("project", function(obj) standardGeneric("project"))

setMethod("project", "SASEGConnection", function(obj) {
  env <- obj@infos
  env$SASProject
})

setGeneric("getUtil", function(obj) standardGeneric("getUtil"))

setMethod("getUtil", "SASEGConnection", function(obj) {
  env <- obj@infos
  env$SASUtil
})

#' @export
setMethod("dbGetException", "SASEGConnection", function(conn, ...) {
  list(
    errornum = conn@infos$sqlrc,
    errorMsg = conn@infos$syserrortext
  )
})

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
#'     allowed by a \code{SASEGDriver} object. 
#' @param profile A character string with the \code{SAS EG} profile name.
#' @param server A character string with the \code{SAS} server name to run programs.
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
#' show(drv)
#' dbListConnections(drv)
#' 
#' my_profile <- "PROFILE"
#' my_server <- "SASPROD"
#' conn <- dbConnect(drv, my_profile, my_server)
#' dbListConnections(drv)
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
#' dbUnloadDriver(drv)
#' 
#' dbIsValid(drv)}
#' @seealso \code{\link[=dbDisconnect,SASEGConnection-method]{dbDisconnect}}
setMethod("dbConnect", "SASEGDriver", function(drv, profile, server, ...) {
  if(!dbIsValid(drv)) stop("invalid driver.")
  
  list_cnx <- dbListConnections(drv)
  if(length(list_cnx) >= dbGetInfo(drv)$max.connections) {
    warning("Cannot create a new connection: max. number of connections reached.\n  Existing connection returned.", immediate. = TRUE)
    cnx <- dbListConnections(drv)[[1]]
    if(!getProfile(cnx) == profile) warning("\n  Previous profile used: ", getProfile(cnx), immediate. = TRUE)
    if(!server(cnx) == server) warning("\n  Previous server used: ", server(cnx), immediate. = TRUE)
    return(cnx)
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
  infos$dbms <- character(0)
  new_cnx <- new("SASEGConnection", infos = infos)
  on.exit(reg.finalizer(infos, finalize_cnx, onexit = TRUE))
  # Add the new connection to the list of connections of the driver:
  cnx(drv) <- new_cnx
  return(new_cnx)
})

#' @export
setMethod("dbConnect", "SASEGConnection", function(drv, ...) {
  dbConnect(drv(drv), getProfile(drv), server(drv))
})

setMethod("show", "SASEGConnection", function(object) {
  cat(
    "<SASEGConnection>\n",
    "Used Profile in Active Connection: ", getProfile(object), "\n",
    "SAS Server to Run Programs in Active Connection: ", server(object), "\n",
    "DBMS SQL Pass-Through: ", if(length(object@infos$dbms) == 0) "NONE" 
                               else object@infos$dbms,
    sep = ""
    )
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

#' @export
setMethod("dbListFields", c("SASEGConnection", "character"), function(conn, name, ...) {
  name <- dataset(name)
  statement <- paste(
    'SELECT name',
    'FROM DICTIONARY.COLUMNS',
    paste0("WHERE libname='", stringr::str_to_upper(name@name[1]), "' AND memname='", stringr::str_to_upper(name@name[2]), "'"),
    sep = "\n"
  )
  d <- dbGetQuery(conn, statement, codeName = NULL, persistent = FALSE)
  return(as.character(d$name))
})

#' @export
setMethod("dbListTables", "SASEGConnection", function(conn, ...) {
  statement <- paste0(
    'SELECT libname, memname\n',
    'FROM DICTIONARY.TABLES'
    )
  d <- dbGetQuery(conn, statement, codeName = NULL, persistent = FALSE)
  paste(d$libname, d$memname, sep = ".")
})

#' @export
setMethod("dbExistsTable", c("SASEGConnection", "character"), function(conn, name, ...) {
  name <- dataset(name)
  statement <- paste(
    'SELECT COUNT(*) AS value',
    'FROM DICTIONARY.TABLES',
    paste0("WHERE libname='", stringr::str_to_upper(name@name[1]), "' AND memname='", stringr::str_to_upper(name@name[2]), "'"),
    sep = "\n"
  )
  d <- dbGetQuery(conn, statement, codeName = NULL, persistent = FALSE)
  return(as.logical(d$value))
})

#' @export
setMethod("dbListResults", "SASEGConnection", function(conn, ...) {
  listResults <- conn@infos$listResults
  lapply(ls(listResults), function(x) get(x, envir = listResults))
})

##### A TESTER

setGeneric("dbClearListResults", function(conn, ...) standardGeneric("dbClearListResults"))

setMethod("dbClearListResults", "SASEGConnection", function(conn, ...) {
  listResults <- conn@infos$listResults
  lapply(ls(listResults), function(x) {
    if(!dbIsValid(get(x, envir = listResults))) rm(x, envir = listResults) 
    })
})


#' Find the SAS data type associated with an R object
#' 
#' Methods are implemented for \code{\linkS4class{SASEGDriver}} and 
#'     \code{\linkS4class{SASEGConnection}} classes.
#' @rdname dbDataType-SASEGDriver-method
#' @inheritParams dbDataType,SASEGDriver-method
#' @keywords internal
#' @export
setMethod("dbDataType", "SASEGConnection", function(dbObj, obj, ...) {
  dbDataType(drv(dbObj), obj)
})


# SAS Results class and Methods -----------------------------------------------


#' SASEGResult class
#' 
#' \code{SASEGResult} class inherits from \code{\link[DBI]{DBIResult-class}}. 
#'    This class represents results of \emph{any} \code{SAS} program. A 
#'    \code{SASEGResult} results from a call to 
#'    \code{\link[=dbSendQuery,SASEGConnection,SAS-method]{dbSendQuery}} with a 
#'    \code{\linkS4class{SAS}} statement.  It is extended by 
#'    \code{\linkS4class{SASEGSQLResult}} for \code{\link[DBI]{SQL}} results.
#' 
#' @slot conn An object of class \code{\linkS4class{SASEGConnection}}.
#' @slot SASResult An object of class \code{\linkS4class{SASEGCode}}.
#' @slot fetched A closure.
#' @slot isValid A closure.
#' @seealso \code{\linkS4class{SASEGSQLResult}}, 
#'     \code{\link[dbSendQuery,SASEGConnection,SAS-method]{dbSendQuery}}
#' @keywords internal
setClass("SASEGResult",
         contains = "DBIResult",
         slots = list(conn = "SASEGConnection",
                      SASResult = "SASEGCode", 
                      fetched = "function", 
                      isValid = "function"
                      )
         )

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
  if(!dbIsValid(res))  stop("Result is not valid.")
  
  res@fetched()
})

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
#' @param res An object of class \code{SASEGResult} or inheriting from this 
#'     class, as \code{\linkS4class{SASEGSQLResult}}.
#' @param value A logical.
#' @rdname SASEGResult-class
#' @keywords internal
setMethod("fetched<-", "SASEGResult", function(res, value) {
  res@fetched(set = value)
  return(res)
})

#' Is a SASEGResult object valid?
#' 
#' \code{dbIsValid} method tests if a \code{SASEGResult} is valid.
#' \code{TRUE} is returned if the object is valid.
#' @param dbObj An object of class \code{SASEGResult} or inheriting from this 
#'     class, as \code{\linkS4class{SASEGSQLResult}}.
#' @inheritParams dbHasCompleted,SASEGResult-method
#' @return \code{dbIsValid} returns a logical.
#' @seealso Generic: \code{\link[DBI]{dbIsValid}}.
#' @rdname SASEGResult-class
#' @export
setMethod("dbIsValid", "SASEGResult", function(dbObj, ...) {
  dbObj@isValid()
})

#' @section Replacement methods:
#' \code{`isValid<-`} method sets the value \code{isValid}. This method is not 
#'     exported. Only developpers may need to use it. 
#' @param obj An object of class \code{SASEGResult} or inheriting from this 
#'     class, as \code{\linkS4class{SASEGSQLResult}}.
#' @inheritParams fetched-set,SASEGResult-method
#' @rdname SASEGResult-class
#' @keywords internal
setMethod("isValid<-", "SASEGResult", function(obj, value) {
  obj@isValid(set = value)
  return(obj)
}) 

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

#' Fetch all results of a SAS program
#' 
#' \code{dbFetch} method retrieve all datasets (with all rows) created by a 
#'     \code{SAS} statement. It returns a named list of 
#'     \code{\link[data.table]{data.table}}. If there is a single fetched 
#'     dataset, a \code{\link[data.table]{data.table}} is returned.
#' @param n An integer, \bold{not used}. All rows of all datasets are fetched.
#' @inheritParams dbHasCompleted,SASEGResult-method
#' @return \code{dbFetch} returns a named (with filenames) list of 
#'     \code{\link[data.table]{data.table}}. In case of a single table, a 
#'     \code{\link[data.table]{data.table}} object is returned.
#' @rdname SASEGResult-class
#' @export
setMethod("dbFetch", "SASEGResult", function(res, n = -1, ...) {
  l <- getListDatasets(res@SASResult)
  n <- length(l)
  d <- vector("list", n)
  i <- 1
  while(i <= length(l)) {
    d[[i]] <- read(l[[i]])
    i <- i+1
  }
  names(d) <- names(l)
  fetched(res) <- TRUE
  if(n == 1) {d <- d[[1]]}
  return(d)
})

# Ne pas l'enlever, c'est pour que le dbGetQuery fonctionne
#' @export
setMethod("dbClearResult", "SASEGResult", function(res, ...) {
  invisible(TRUE)
})

#' Get the log of a program execution
#' 
#' \code{dbGetLog} methods retrieve the log of a program execution.
#' 
#' \code{dbGetLog} does not belong to the \code{DBI} specification.
#' @param res An object.
#' @param ... Other parameters passed to method.
#' @keywords internal
setGeneric("dbGetLog", function(res, ...) standardGeneric("dbGetLog"))

#' Get the log ater a SAS run
#' 
#' \code{dbGetLog} method retrieves the log of the \code{\linkS4class{SAS}} 
#' program. \code{dbGetLog} does not belong to the \code{DBI} specification.
#' @inheritParams dbHasCompleted,SASEGResult-class
#' @return \code{dbGetLog} returns a character string with the \code{SAS log}.
#' @rdname SASEGResult-class
#' @export
setMethod("dbGetLog", "SASEGResult", function(res, ...) {
  getLog(res@SASResult)
})

#' Get the statement that was sent to SAS
#' 
#' \code{dbGetStatement} collect the program that was sent to \code{SAS}.
#' @export
setMethod("dbGetStatement", "SASEGResult", function(res, ...) {
  getSourceCode(res@SASResult)
})

# SQL Results class and methods -------------------------------------------


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
    rowsFetched = "function",
    SQLRC = "data.frame",
    RCFileName = "character"
    )
  )

#' Get the number of fetched rows
#' 
#' \code{dbGetRowCount} returns the number of rows that was fetched.
#' @export
setMethod("dbGetRowCount", "SASEGSQLResult", function(res, ...) {
  res@rowsFetched()
})

#' @export
setMethod("dbGetRowsAffected", "SASEGSQLResult", function(res, ...) {
  res@SQLRC$sqlobs
})

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
#' @return A \code{SASEGSQLResult} object.
#' @seealso Generic: \code{\link[DBI]{dbSendQuery}}
#' @export
#' @examples
#' # This is another good place to put examples
setMethod(
  "dbSendQuery", 
  c("SASEGConnection", "character"), 
  function(conn, statement, codeName = NULL, persistent = TRUE, query = TRUE, ...) {
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
    if(persistent){
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
    res <- dbSendQuery(conn, statement, codeName, persistent)
    # Get the list of output datasets:
    l <- getListDatasets(res@SASResult)
    # Read the SAS return codes:
    if(!is.na(RCFileName)) {
      SQLRC <- data.frame(read(l[[RCFileName]]))
      if(SQLRC$sqlrc > 0 & SQLRC$sqlrc < 8) warning("SAS/PROC SQL warning: ", SQLRC$syserrortext)
      if(SQLRC$sqlrc >= 8) {
        on.exit(stop("SAS/PROC SQL error: ", SQLRC$syserrortext, call. = FALSE))
        conn@infos$sqlrc <- SQLRC$sqlrc
        conn@infos$syserrortext <- SQLRC$syserrortext
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
                   rowsFetched = count_generator(init = 0),
                   SQLRC = SQLRC,
                   RCFileName = RCFileName
                   )
    assign(random_table_name(), res_sql, envir = conn@infos$listResults)
    
    return(res_sql)
  }
)

#' @export
setMethod(
  "dbSendStatement", 
  c("SASEGConnection", "character"), 
  function(conn, statement, codeName = NULL, persistent = TRUE, ...) {
  dbSendQuery(conn, statement, codeName, persistent, query = FALSE)
})

#' @export
setMethod("dbGetRowsAffected", "SASEGSQLResult", function(res, ...) {
  res@SQLRC$sqlobs
})

#' @export
setMethod("dbClearResult", "SASEGSQLResult", function(res, ...) {
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
  if(any(!is.na(c(res@RCFileName, res@SQLResult)))) dbSendStatement(res@conn, statement, codeName = NULL, persistent = FALSE)
  # In all cases, set isValid to FALSE:
  isValid(res) <- FALSE
  
  invisible(TRUE)
})

#' Retrieve records from SAS EG SQL query
#' @export
setMethod("dbFetch", "SASEGSQLResult", function(res, n = -1, ...) {
  if(!length(n) == 1) stop("Argument n must be an atomic vector.")
  if(!any(n%%1 == 0, is.infinite(n))) stop("Argument n must be a whole number.")
  if(n < -1) stop("Argument n must be greater or equal to -1.")
  if(!dbIsValid(res)) stop("Invalid result object.")
  # If object res was created by a SQL statement then SQLResult slot is NA:
  if(is.na(res@SQLResult)) {
    warning("No table to fetch: check that there is no error in your SQL code.")
    return(data.frame(NULL))
    }
  if(dbHasCompleted(res)) message("No row to fetch: an empty data.frame is returned.") 
  # Store in a variable whether all remaining rows are claimed:
  allrows <- (n == -1 | is.infinite(n))
  
  if(allrows & dbGetRowCount(res) == 0) {
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
    if(allrows | dbHasCompleted(res)) {
      condition <- paste0("GE ", n_min)
    } else {
      condition <- paste0("BETWEEN ", n_min, " AND ", n_max)
    }
    query <- paste0("SELECT * FROM ", res@SQLResult, "\n",
                    "WHERE MONOTONIC() ", condition)
    d <- dbGetQuery(res@conn, query, codeName = NULL, persistent = FALSE)
    if(nrow(d) < n | allrows) {fetched(res) <- TRUE}
  }
  res@rowsFetched(add = nrow(d))
  return(d)
})

#' @export
setMethod(
  "dbGetQuery", 
  c("SASEGConnection", "character"), 
  function(conn, statement, codeName = NULL, persistent = TRUE, ...) {
    res <- dbSendQuery(conn, statement, codeName = codeName, persistent = persistent, query = TRUE)
    on.exit(dbClearResult(res))
    d <- dbFetch(res, n = -1)
    return(d)
})

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
  d <- dbGetQuery(res@conn, statement, codeName = NULL, persistent = FALSE)
  d <- SAS2RDataType(d)
  return(d)
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
  x <- strsplit(x, ".", fixed = TRUE)[[1]]
  x <- dbQuoteIdentifier(DBI::ANSI(), x, ...)
  x <- vapply(x, function(y) if(y == '""') NA_character_ else y, character(1))
  x <- paste0(x, collapse = '.')
  new("SQL", gsub('.NA.', '..', x, fixed = TRUE))
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
  
  # Convert datetimes as SAS DATETIME. format
  is_DateTime <- vapply(value, is.DateTime, logical(1))
  value[is_DateTime] <- lapply(value[is_DateTime], SASFormat)
  
  # Convert difftimes as SAS TIME. format
  is_difftime <- vapply(value, is.difftime, logical(1))
  value[is_difftime] <- lapply(value[is_difftime], SASFormat)
  
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
  DBI::SQL(paste0(
    "INSERT INTO ", table, "\n",
    "  (", paste(fields, collapse = ", "), ")\n",
    paste0("  VALUES(", rows, ")", collapse = "\n")
    ))
  }
)

#' @export
setMethod("dbWriteTable", "SASEGConnection", function(conn, name, value, row.names = NA, ...) {
  # Ce programme sera à modifier si on veut faire du SAS SQL pass-through
  statement <- paste0(
    sqlCreateTable(con = conn, table = name, fields = value, row.names = row.names, temporary = FALSE),
    ";\n",
    sqlAppendTable(con = conn, table = name, values = value, row.names = row.names, ...)
    )
  dbSendStatement(conn, statement, codeName = paste("Create dataset", name))
  
  invisible(TRUE)
})
