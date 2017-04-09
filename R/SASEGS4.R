# S4 classes to handle SAS Enterprise Guide Scripting API.
# This file contains all calls to CLR methods.
# A basic knowledge of the rClr package
# and of the SAS EG Scripting API is recommended before
# reading this file.

#' @import methods
#' @import rClr
NULL

#' Load SAS EG Scripting API
#'
#' Load \code{SAS Enterprise Guide Scripting} DLL.
#' You have to provide the path to the file \code{SASEGScripting.dll} that comes with your installation.
#'
#' This function is a simple wrapper over \code{\link[rClr]{clrLoadAssembly}}.
#'
#' @param DLLFilePath A string with the path to the \code{SASEGScripting.dll} file.
#' @examples
#' \dontrun{
#'
#' # Modify the path below following your install:
#' path <- "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"
#' loadSASEGScripting(path)
#' }
#' @export
loadSASEGScripting <- function(DLLFilePath) {
  rClr::clrLoadAssembly(DLLFilePath)
}


#' An S4 virtual class to represent any SAS EG Scripting object
#'
#' The \code{SASObjRef} S4 class represents any \code{SAS EG Scripting} object.
#' This class and its methods are devoted to developers.
#'
#' A \code{SASObjRef} object has one slot, named \code{ptr}, where \code{ptr} is
#' a \code{cobjRef} object. For more information about \code{CLR} objects and methods, see the \code{rClr} package
#' documentation.
#' @slot ptr A \code{cobjRef} object.
#' @exportClass SASObjRef
#' @keywords internal
setClass("SASObjRef", slots = list(ptr = "cobjRef"))

#' @rdname SASObjRef-class
#' @param obj A \code{CLR} object. Any other \code{R} object can be passed on argument.
#' @return \code{SASObjRef(obj)} returns a \code{SASObjRef} object if \code{obj} is a \code{CLR} object; returns \code{obj} otherwise.
#' @examples
#' \dontrun{
#'
#' # Modify the path below following your install:
#' path <- "C:/Program Files/SAS94/SASEnterpriseGuide/7.1/SASEGScripting.dll"
#'
#' loadSASEGScripting(path)
#'
#' app_clrobj <- rClr::clrNew("SAS.EG.Scripting.Application")
#' app_sasobj <- SASObjRef(app_clrobj)
#'
#' show(app_clrobj)
#' show(ptr(app_sasobj))
#'
#' clrGet(app_sasobj, "Name")
#'
#' clrCall(app_sasobj, "Quit")
#' }
#' @export
#' @keywords internal
SASObjRef <- function(obj) {
  if(class(obj) == "cobjRef") {
    out <- new("SASObjRef", ptr = obj)
  } else {
    out <- obj
    }
  return(out)
}

#' @exportMethod ptr
#' @keywords internal
setGeneric("ptr", function(SASObj) standardGeneric("ptr"))

#' @rdname SASObjRef-class
#' @param SASObj A \code{SASObjRef} object.
#' @return \code{ptr()} returns a \code{cobjRef} object.
#' @export
#' @keywords internal
setMethod("ptr", "SASObjRef", function(SASObj) {return(SASObj@ptr)})

#' @exportMethod clrCall
#' @keywords internal
setGeneric("clrCall", package = "rClr")

#' @rdname SASObjRef-class
#' @export
#' @keywords internal
setMethod("clrCall", "SASObjRef", function(obj, methodName, ...) {
  SASObjRef(rClr::clrCall(ptr(obj), methodName, ...))
})

#' @exportMethod clrGet
#' @keywords internal
setGeneric("clrGet", package = "rClr")

#' @rdname SASObjRef-class
#' @export
#' @keywords internal
setMethod("clrGet", "SASObjRef", function(objOrType, name) {
  SASObjRef(rClr::clrGet(ptr(objOrType), name))
})

#' @exportMethod clrSet
#' @keywords internal
setGeneric("clrSet", package = "rClr")

#' @rdname SASObjRef-class
#' @export
#' @keywords internal
setMethod("clrSet", "SASObjRef", function(objOrType, name, value) {
  rClr::clrSet(ptr(objOrType), name, value)
})


#' An S4 class to represent a SAS EG Application object
#'
#' The \code{SASEGApplication} class is an S4 class to represent a \code{SAS EG Scripting Application} object.
#' @exportClass SASEGApplication
setClass("SASEGApplication", contains = "SASObjRef")

#' @rdname SASEGApplication-class
#' @param application A \code{SASEGApplication} object.
#' @param profile A character string with the SAS profile name.
#' @return A \code{SASEGApplication} object.
#' @examples
#' \dontrun{
#' # Modify the path below following your install:
#' path <- "C:/Program Files/SAS94/SASEnterpriseGuide/7.1/SASEGScripting.dll"
#' loadSASEGScripting(path)
#'
#' app <- SASEGApplication()
#' show(app)
#'
#' my_profile <- "PROFILE"
#' setProfile(app, my_profile)
#'
#' my_new_project <- newProject(app)
#'
#' # Important: you have to quit SAS EG Application
#' terminate(app)
#' }
#' @seealso \code{\linkS4class{SASEGProject}}
#' @export
SASEGApplication <- function() {
  ptr <- rClr::clrNew("SAS.EG.Scripting.Application")
  new("SASEGApplication", ptr = ptr)
}

#' @rdname SASEGApplication-class
setMethod("show", "SASEGApplication", function(object) {
  cat(clrGet(object, "Name"),
      ", Version: ",
      clrGet(object, "Version"),
      "\n", sep=""
  )
  for(i in 0:(clrGet(clrCall(object, "Profiles"), "Count")-1)) {
    cat(
      "Profile available: ",
      clrGet(clrCall(clrCall(object, "Profiles"), "Items", i), "Name"),
      ", Host: ",
      clrGet(clrCall(clrCall(object, "Profiles"), "Items", i), "HostName"),
      ", Port: ",
      clrGet(clrCall(clrCall(object, "Profiles"), "Items", i), "Port"),
      "\n", sep=""
    )
  }
})


#' @exportMethod setProfile
setGeneric("setProfile",
           function(application, profile) standardGeneric("setProfile"))

#' @rdname SASEGApplication-class
setMethod("setProfile", "SASEGApplication", function(application, profile) {
  clrCall(application, "SetActiveProfile", profile)
})

#' @exportMethod terminate
setGeneric("terminate", function(application) standardGeneric("terminate"))

#' @rdname SASEGApplication-class
setMethod("terminate", "SASEGApplication", function(application) {
  clrCall(application, "Quit")
  return(TRUE)
})


#' An S4 class to represent a SAS EG Project object
#'
#' The \code{SASEGProject} class is an S4 class to represent a \code{SAS EG Scripting Project} object.
#'
#' @section Constructor:
#' There is no constructor. To generate a \code{SASEGProject} object, you have to call the \code{SASEGApplication} method \code{newProject()}.
#' @examples
#' \dontrun{
#' # Modify the path below following your install:
#' path <- "C:/Program Files/SAS94/SASEnterpriseGuide/7.1/SASEGScripting.dll"
#' loadSASEGScripting(path)
#' app <- SASEGApplication()
#'
#' my_profile <- "PROFILE"
#' setProfile(app, my_profile)
#'
#' my_new_project <- newProject(app)
#'
#' my_server <- "SASPROD"
#' sas_program <- "data a; set sashelp.class; run;"
#' program_name <- "Test RSASEG"
#' my_new_code <- newCode(my_new_project,
#'                        my_server,
#'                        sas_program,
#'                        program_name)
#' saveAs(my_new_project, "~/RSASEG.egp")
#'
#' # Important: you have to quit SAS EG Application
#' terminate(app)
#' }
#' @seealso \code{\linkS4class{SASEGApplication}}, \code{\linkS4class{SASEGCode}}
#' @exportClass SASEGProject
setClass("SASEGProject", contains = "SASObjRef")

#' @exportMethod newProject
setGeneric("newProject", function(application) standardGeneric("newProject"))

#' @rdname SASEGApplication-class
setMethod("newProject", "SASEGApplication", function(application) {
  new("SASEGProject", clrCall(application, "New"))
})

#' @exportMethod saveAs
setGeneric("saveAs", function(object, ...) standardGeneric("saveAs"))

#' @rdname SASEGProject-class
#' @param filepath A character string with the path to the file project, \code{NULL} by default.
#'   \strong{Be careful: \code{saveAs()} method overwrites existing files without confirmation.}
setMethod("saveAs", "SASEGProject", function(object, filepath, ...) {
  clrCall(object, "SaveAs", filepath)
  message("SAS Enterprise Guide Project saved to: ", filepath)
})


#' An S4 class to represent a SAS EG Code object
#'
#' The \code{SASEGCode} class is an S4 class to represent a \code{SAS EG Scripting Code} object.
#' @section Constructor:
#' There is no constructor. To generate a \code{SASEGCode} object,
#' you have to call the \code{SASEGProject} method \code{newCode()}.
#' @examples
#' \dontrun{
#' # Modify the path below following your install:
#' path <- "C:/Program Files/SAS94/SASEnterpriseGuide/7.1/SASEGScripting.dll"
#' loadSASEGScripting(path)
#' app <- SASEGApplication()
#' setProfile(app, "PROFILE")
#' my_new_project <- newProject(app)
#' my_new_code <- newCode(my_new_project)
#'
#' setServer(my_new_code, "SASPROD")
#' setText(my_new_code, "data a; set sashelp.class; run;")
#' setName(my_new_code, "Test RSASEG")
#' run(my_new_code)
#' log <- getLog(my_new_code)
#' cat(log)
#' a <- getListDatasets(my_new_code)
#' n <- countOutputDatasets(my_new_code)
#' check_my_code <- getSourceCode(my_new_code)
#' cat(check_my_code)
#'
#' # Important: you have to quit SAS EG Application
#' terminate(app)
#' }
#' @seealso \code{\linkS4class{SASEGProject}}, \code{\linkS4class{SASEGDataset}}
#' @exportClass SASEGCode
setClass("SASEGCode", contains = "SASObjRef")

#' @exportMethod setServer
setGeneric("setServer", function(object, server, ...) standardGeneric("setServer"))

#' @rdname SASEGCode-class
#' @param object A \code{SASEGCode} object.
#' @param server A character string with the \code{SAS} server name.
#' @param ... Other parameters. Not used.
setMethod("setServer", "SASEGCode", function(object, server, ...) {
  clrSet(object, "Server", server)
})

#' @exportMethod setText
setGeneric("setText", function(object, text, ...) standardGeneric("setText"))

#' @rdname SASEGCode-class
#' @param text A character string with the code of the \code{SAS} program.
setMethod("setText", "SASEGCode", function(object, text, ...) {
  clrSet(object, "Text", text)
})

#' @exportMethod setName
setGeneric("setName", function(object, name, ...) standardGeneric("setName"))

#' @rdname SASEGCode-class
#' @param name A character string to set the name of the program.
setMethod("setName", "SASEGCode", function(object, name, ...) {
  clrSet(object, "Name", name)
})

#' @exportMethod newCode
setGeneric("newCode", function(project, ...) standardGeneric("newCode"))

#' @rdname SASEGProject-class
#' @param project A \code{SASEGProject} object.
#' @param server A character string with the server name.
#' @param program A character string with the \code{SAS} code of the program.
#'   Optional, \code{NULL} by default.
#' @param name A character string with the name of the program. Optional, \code{NULL} by default.
setMethod("newCode", "SASEGProject", function(project, server = NULL, program = NULL, name = NULL, ...) {
  code <- new("SASEGCode", clrCall(clrGet(project, "CodeCollection"), "Add"))
  if(!is.null(server)) setServer(object = code, server = server)
  if(!is.null(program)) setText(object = code, text = program)
  if(!is.null(name)) setName(object = code, name = name)
  return(code)
})

#' @exportMethod run
setGeneric("run", function(code) standardGeneric("run"))

#' @rdname SASEGCode-class
#' @param code A \code{SASEGCode} object.
setMethod("run", "SASEGCode", function(code) {
  clrCall(code, "Run")
})

#' @exportMethod getLog
setGeneric("getLog", function(code) standardGeneric("getLog"))

#' @rdname SASEGCode-class
setMethod("getLog", "SASEGCode", function(code) {
  return(clrGet(clrGet(code, "Log"), "Text"))
})

#' @exportMethod countOutputDatasets
setGeneric("countOutputDatasets", function(code) standardGeneric("countOutputDatasets"))

#' @rdname SASEGCode-class
setMethod("countOutputDatasets", "SASEGCode", function(code) {
  return(clrGet(clrGet(code, "OutputDatasets"), "Count"))
})

#' @exportMethod getSourceCode
setGeneric("getSourceCode", function(code) standardGeneric("getSourceCode"))

#' @rdname SASEGCode-class
setMethod("getSourceCode", "SASEGCode", function(code) {
  return(clrGet(code, "Text"))
})


#' An S4 class to represent a SAS EG Dataset object
#'
#' @exportClass SASEGDataset
setClass("SASEGDataset", contains = "SASObjRef")

#' @exportMethod getListDatasets
setGeneric("getListDatasets", function(code) standardGeneric("getListDatasets"))

#' @rdname SASEGCode-class
#' @return \code{getListDatasets} returns a list of \code{SASEGDataset} objects.
setMethod("getListDatasets", "SASEGCode", function(code) {
  n <- countOutputDatasets(code)
  l <- vector("list", n)
  enum <- clrCall(clrGet(code, "OutputDatasets"), "GetEnumerator")
  clrCall(enum, "MoveNext")
  i <- 1
  while(i <= n) {
    l[[i]] <- new("SASEGDataset", clrGet(enum, "Current"))
    clrCall(enum, "MoveNext")
    i <- i+1
  }
  return(l)
})

#' @exportMethod getName
setGeneric("getName", function(object, ...) standardGeneric("getName"))
setMethod("getName", "SASEGDataset", function(object) {
  clrGet(object, "Name")
})

#' @exportMethod getFileName
setGeneric("getFileName", function(object, ...) standardGeneric("getFileName"))
setMethod("getFileName", "SASEGDataset", function(object) {
  clrGet(object, "FileName")
})

setMethod("saveAs", "SASEGDataset", function(object, dir = NULL, name = NULL, type = "csv", fsep = "\\") {
  if(is.null(dir)) {
    dir <- normalizePath(tempdir())
  }
  if(is.null(name)) {
    name <- getName(object)
  }
  path <- file.path(dir, paste0(name, ".", type), fsep = fsep)
  clrCall(object, "SaveAs", path)
  return(path)
})
