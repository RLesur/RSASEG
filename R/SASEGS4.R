# S4 classes to handle SAS Enterprise Guide Scripting API.
# This file contains all calls to CLR methods.
# A basic knowledge of the rClr package and of the SAS EG Scripting API is 
# recommended before reading this file.

#' @import methods
#' @import rClr
NULL

#' Load SAS EG Scripting API
#'
#' Load \code{SAS Enterprise Guide Scripting} DLL.
#' You have to provide the path to the file \code{SASEGScripting.dll} that comes 
#' with your installation.
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


# SASObjRef Class and Methods ---------------------------------------------


#' An S4 virtual class to represent any SAS EG Scripting object
#'
#' The \code{SASObjRef} S4 class represents any \code{SAS EG Scripting} object.
#' This is a virtual class.
#' This class and its methods are internals.
#'
#' A \code{SASObjRef} object has only one slot, named \code{ptr}, where \code{ptr} is
#' a \code{cobjRef} object. For more information about \code{CLR} objects and methods,
#' see the \code{rClr} package documentation.
#' @slot ptr A \code{cobjRef} object.
#' @keywords internal
#' @exportClass SASObjRef
setClass("SASObjRef", slots = list(ptr = "cobjRef"))

#' @rdname SASObjRef-class
#' @param ptr A \code{CLR} object. Any other \code{R} object can be passed on argument.
#' @return \code{SASObjRef(ptr)} returns a \code{SASObjRef} object if \code{ptr} is a 
#'     \code{CLR} object; returns \code{ptr} otherwise.
#' @examples
#' \dontrun{
#'
#' # Modify the path below following your install:
#' path <- "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"
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
#' @seealso Package \code{rClr}: \code{\link[rClr]{clrCall}}, \code{\link[rClr]{clrGet}}, 
#'     \code{\link[rClr]{clrSet}}
#' @seealso Generics:  \code{\link{clrCall}}, \code{\link{clrGet}}, \code{\link{clrSet}}
#' @export
#' @keywords internal
SASObjRef <- function(ptr) {
  if(class(ptr) == "cobjRef") {
    out <- new("SASObjRef", ptr = ptr)
  } else {
    out <- ptr
    }
  return(out)
}

#' Get the ptr slot of an object
#' 
#' Get the \code{ptr} slot of an object.
#' @param SASObj An object with a \code{ptr} slot.
#' @keywords internal
#' @exportMethod ptr
setGeneric("ptr", function(SASObj) standardGeneric("ptr"))

#' @rdname SASObjRef-class
#' @param SASObj A \code{SASObjRef} object.
#' @return \code{ptr(SASObj)} returns a \code{cobjRef} object from a \code{SASObjRef} object.
#' @export
#' @keywords internal
setMethod("ptr", "SASObjRef", function(SASObj) {return(SASObj@ptr)})

#' Call a method on an object (generic)
#' 
#' This is a generic version of the \code{\link[rClr]{clrCall}} function of the \code{rClr} 
#' package.
#' @param obj An object.
#' @param methodName A character string with the name of a method of the object.
#' @param ... Additional method arguments.
#' @exportMethod clrCall
#' @keywords internal
#' @seealso \code{cobjRef} method: \code{\link[rClr]{clrCall}}
#' @seealso \code{SASObjRef} method: \code{\link[=clrCall,SASObjRef-method]{clrCall}}
setGeneric("clrCall", package = "rClr")

#' @rdname SASObjRef-class
#' @param obj A \code{SASObjRef} object.
#' @inheritParams clrCall
#' @return \code{clrCall} returns an object from the call. May be a 
#'     \code{SASObjRef} object, or a native \code{R} object for common types.
#'     Can be \code{NULL}.
#' @export
#' @keywords internal
setMethod("clrCall", "SASObjRef", function(obj, methodName, ...) {
  SASObjRef(rClr::clrCall(ptr(obj), methodName, ...))
})

#' Gets the value of a field or property of an object or class (generic)
#' 
#' This is a generic version of the \code{\link[rClr]{clrGet}} function of the \code{rClr} 
#' package.
#' @param objOrType An object, or type name, possibly namespace and assembly 
#'     qualified type name.
#' @param name The name of a field/property of the object.
#' @seealso \code{cobjRef} method: \code{\link[rClr]{clrGet}}
#' @seealso \code{SASObjRef} method: \code{\link[=clrGet,SASObjRef-method]{clrGet}}
#' @exportMethod clrGet
#' @keywords internal
setGeneric("clrGet", package = "rClr")

#' @rdname SASObjRef-class
#' @param objOrType A \code{SASObjRef} object.
#' @inheritParams clrGet
#' @return \code{clrGet} returns an object from the call. May be a 
#'     \code{SASObjRef} object, or a native \code{R} object for common types. 
#'     Can be \code{NULL}.
#' @export
#' @keywords internal
setMethod("clrGet", "SASObjRef", function(objOrType, name) {
  SASObjRef(rClr::clrGet(ptr(objOrType), name))
})

#' Sets the value of a field or property of an object or class (generic)
#' 
#' This is a generic version of the \code{\link[rClr]{clrSet}} function of the 
#' \code{rClr} package.
#' @param value The value to set the field with.
#' @inheritParams clrGet
#' @seealso \code{cobjRef} method: \code{\link[rClr]{clrSet}}
#' @seealso \code{SASObjRef} method: \code{\link[=clrSet,SASObjRef-method]{clrSet}}
#' @exportMethod clrSet
#' @keywords internal
setGeneric("clrSet", package = "rClr")

#' @rdname SASObjRef-class
#' @param value The value to set the field with.
#' @export
#' @keywords internal
setMethod("clrSet", "SASObjRef", function(objOrType, name, value) {
  rClr::clrSet(ptr(objOrType), name, value)
})


# SASEGApplication Class and Methods --------------------------------------


#' An S4 class to represent a SAS EG Application object
#'
#' The \code{SASEGApplication} class is an S4 class to represent a 
#' \code{SAS EG Scripting Application} object.
setClass("SASEGApplication", contains = "SASObjRef")

#' A finalizer function for SASEGApplication
#' 
#' In case of end user would not safely close a SASEGApplication object,
#'    a finalizer is implemented. A call to method "Quit" is done.
#' @param e An external pointer.
#' @return \code{NULL}, invisibly
#' @keywords internal
finalize_app <- function(e) {
  app <- new("cobjRef", clrobj = e, clrtype = "SAS.EG.Scripting.Application")
  rClr::clrCall(app, "Quit")
  invisible()
}

#' @rdname SASEGApplication-class
#' @return \code{SASEGApplication()} is the constructor; it returns a 
#'     \code{SASEGApplication} object.
#' @examples
#' \dontrun{
#' # Modify the path below following your install:
#' path <- "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"
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
  # Get the external pointer:
  e <- rClr::clrGetExtPtr(ptr)
  # Register the finalizer:
  reg.finalizer(e, finalize_app, onexit = FALSE)
  new("SASEGApplication", ptr = ptr)
}

#' @rdname SASEGApplication-class
#' @description \code{show} method provides useful informations including 
#'     \code{SAS EG} version and available profiles.
#' @param  object A \code{SASEGApplication} object.
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

#' Set the profile in an application
#' 
#' \code{setProfile} method is used to set the profile in an application.
#' @param application An application object.
#' @param profile A profile. 
#' @keywords internal
#' @exportMethod setProfile
setGeneric("setProfile",
           function(application, profile) standardGeneric("setProfile"))

#' Set the profile in a SASEGApplication object
#' 
#' \code{setProfile} method is used to set the profile in a \code{SASEGApplication} object.
#' @param application A \code{SASEGApplication} object.
#' @param profile A character string with the \code{SAS EG} profile name.
#' @rdname SASEGApplication-class
#' @export
setMethod("setProfile", "SASEGApplication", function(application, profile) {
  clrCall(application, "SetActiveProfile", profile)
})

#' Terminate (or quit) an application
#' 
#' \code{terminate} method is used to quit an application.
#' 
#' @param application An application object.
#' @keywords internal
#' @exportMethod terminate
setGeneric("terminate", function(application) standardGeneric("terminate"))

#' Quit a SASEGApplication
#' 
#' \code{terminate} method is used to quit a \code{SASEGApplication}.
#' @inheritParams setProfile-method
#' @return \code{terminate} method returns \code{TRUE}. 
#' @rdname SASEGApplication-class
#' @export
setMethod("terminate", "SASEGApplication", function(application) {
  clrCall(application, "Quit")
  return(TRUE)
})


# SASEGProject Class and Methods ------------------------------------------


#' An S4 class to represent a SAS EG Project object
#'
#' The \code{SASEGProject} class is an S4 class to represent a 
#'     \code{SAS EG Scripting Project} object.
#'
#' @section Constructor:
#' There is no constructor. To create a new \code{SASEGProject} object, you have 
#'     to call the \code{\linkS4class{SASEGApplication}} method 
#'     \code{\link{newProject}}.
#' @examples
#' \dontrun{
#' # Modify the path below following your install:
#' path <- "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"
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
#'                        
#' path <- paste(normalizePath("~"), "RSASEG.egp", sep = "\\")
#' saveAs(my_new_project, path)
#'
#' # Important: you have to quit SAS EG Application
#' terminate(app)
#' }
#' @seealso \code{\linkS4class{SASEGApplication}}, \code{\linkS4class{SASEGCode}}
setClass("SASEGProject", contains = "SASObjRef")

#' Create a new project in an application
#' 
#' \code{newProject} method creates a new project in an application.
#' @param application An application object.
#' @keywords internal
#' @exportMethod newProject
setGeneric("newProject", function(application) standardGeneric("newProject"))

#' Create a new SASEGProject
#' 
#' \code{newProject} method opens a new \code{SASEGProject} in a given 
#'     \code{SASEGApplication}.
#' @inheritParams setProfile-method
#' @return \code{newProject} returns a \code{\linkS4class{SASEGProject}} object.
#' @rdname SASEGApplication-class
#' @export
setMethod("newProject", "SASEGApplication", function(application) {
  new("SASEGProject", clrCall(application, "New"))
})


#' Save an object as...
#' 
#' A generic that save an object as...
#' @param object An object to save.
#' @param ... Other parameters passed to method, including filepath.
#' @keywords internal
#' @exportMethod saveAs
setGeneric("saveAs", function(object, ...) standardGeneric("saveAs"))

#' Save a SASEGProject
#' 
#' \code{saveAs} method saves a \code{SASEGProject} to a file.
#' 
#' \code{saveAs} method shows a message after saving the file.
#' @param object A \code{SASEGProject} object.
#' @param filepath A character string with a valid path.
#'   \strong{Be careful: \code{saveAs} method overwrites existing files 
#'   without confirmation.}
#' @param ... Other parameters passed to method (not used).
#' @rdname SASEGProject-class
#' @export
setMethod("saveAs", "SASEGProject", function(object, filepath, ...) {
  clrCall(object, "SaveAs", filepath)
  message("SAS Enterprise Guide Project saved to: ", filepath)
})


# SASEGCode Class and Methods ---------------------------------------------


#' An S4 class to represent a SAS EG Code object
#'
#' The \code{SASEGCode} class is an S4 class to represent a 
#' \code{SAS EG Scripting Code} object.
#' @section Constructor:
#' There is no constructor. To create a new \code{SASEGCode} object,
#' you have to call the \code{\linkS4class{SASEGProject}} method 
#' \code{\link{newCode}}.
#' @examples
#' \dontrun{
#' # Modify the path below following your install:
#' path <- "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"
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
#' n <- countOutputDatasets(my_new_code)
#' check_my_code <- getSourceCode(my_new_code)
#' cat(check_my_code)
#' a <- getListDatasets(my_new_code)
#'
#' # Important: you have to quit SAS EG Application
#' terminate(app)
#' }
#' @seealso \code{\linkS4class{SASEGProject}}, \code{\linkS4class{SASEGDataset}}
setClass("SASEGCode", contains = "SASObjRef")

#' Set the server
#' 
#' \code{setServer} method is used to set the server.
#' @param object An object with a server parameter.
#' @param server A server name.
#' @keywords internal
#' @exportMethod setServer
setGeneric("setServer", function(object, server, ...) standardGeneric("setServer"))

#' Set the SAS server for a SASEGCode
#' 
#' \code{setServer} method is used to set the \code{SAS} server that will be 
#'    used to execute the program contained in the \code{SASEGCode} object. 
#' @param object A \code{SASEGCode} object.
#' @param server A character string with the \code{SAS} server name.
#' @param ... Other parameters. Not used.
#' @rdname SASEGCode-class
#' @export
setMethod("setServer", "SASEGCode", function(object, server, ...) {
  clrSet(object, "Server", server)
})

#' Set the text
#' 
#' \code{setText} method is used to set the text.
#' @param object An object.
#' @param text A text.
#' @keywords internal
#' @exportMethod setText
setGeneric("setText", function(object, text, ...) standardGeneric("setText"))

#' Put a SAS program in a SASEGCode object
#' 
#' \code{setText} method is used to put a \code{SAS} program in a \code{SASEGCode} object.
#' @param text A character string with the code of the \code{SAS} program.
#' @inheritParams setServer-method
#' @rdname SASEGCode-class
#' @export
setMethod("setText", "SASEGCode", function(object, text, ...) {
  clrSet(object, "Text", text)
})

#' Set the name of an object
#' 
#' \code{setName} method is used to set the name of the object.
#' @param object An object to be named.
#' @param name A name.
#' @keywords internal
#' @exportMethod setName
setGeneric("setName", function(object, name, ...) standardGeneric("setName"))

#' Name a SASEGCode object
#' 
#' \code{setName} method is used to give a name to a \code{SASEGCode} object.
#' @param name A character string with the name of the program.
#' @inheritParams setServer-method
#' @rdname SASEGCode-class
#' @export
setMethod("setName", "SASEGCode", function(object, name, ...) {
  clrSet(object, "Name", name)
})

#' Create a new code
#' 
#' \code{newCode} method creates a new code object in an existing project.
#' @param project A project object.
#' @keywords internal
#' @exportMethod newCode
setGeneric("newCode", function(project, ...) standardGeneric("newCode"))

#' Create a new SASEGCode object
#' 
#' \code{newCode} method is used to create a new \code{SASEGCode} object in a 
#'     \code{SASEGProject} object.
#' @rdname SASEGProject-class
#' @param project A \code{SASEGProject} object.
#' @param server A character string with the server name. Optional, 
#'     \code{NULL} by default.
#' @param program A character string with the \code{SAS} code of the program.
#'   Optional, \code{NULL} by default.
#' @param name A character string with the name of the program. Optional, 
#'     \code{NULL} by default.
#' @inheritParams saveAs-method
#' @return \code{newCode} method returns a \code{\linkS4class{SASEGCode}}.
#' @export
setMethod("newCode", 
          "SASEGProject", 
          function(project, server = NULL, program = NULL, name = NULL, ...) {
            code <- new("SASEGCode", clrCall(clrGet(project, "CodeCollection"), "Add"))
            if(!is.null(server)) setServer(object = code, server = server)
            if(!is.null(program)) setText(object = code, text = program)
            if(!is.null(name)) setName(object = code, name = name)
            return(code)
            }
)

#' Run the code
#' 
#' \code{run} method is used to run (or execute) the code.
#' @param code A code object.
#' @keywords internal
#' @exportMethod run
setGeneric("run", function(code) standardGeneric("run"))

#' Run the program of a SASEGCode object
#' 
#' \code{run} method is used to run (or execute) the \code{SAS} program of a 
#'     \code{SASEGCode} object.
#' @param code A \code{SASEGCode} object.
#' @rdname SASEGCode-class
#' @export
setMethod("run", "SASEGCode", function(code) {
  clrCall(code, "Run")
})

#' Get the log
#' 
#' \code{getLog} method is used to retrieve the log after the code execution.
#' @param code A code object.
#' @keywords internal
#' @exportMethod getLog
setGeneric("getLog", function(code) standardGeneric("getLog"))

#' Get the log of a program execution
#' 
#' \code{getLog} method is used to retrieve the log after an execution.
#' @inheritParams run-method
#' @return \code{getLog} returns a character string containing the log.
#' @rdname SASEGCode-class
#' @export
setMethod("getLog", "SASEGCode", function(code) {
  return(clrGet(clrGet(code, "Log"), "Text"))
})

#' Get the number of datasets generated by the code
#' 
#' \code{countOutputDatasets} method is used to get the number of datasets 
#'     created by the code execution.
#' @keywords internal
#' @exportMethod countOutputDatasets
setGeneric("countOutputDatasets", function(code) standardGeneric("countOutputDatasets"))

#' Get the number of datasets created by a program
#' 
#' \code{countOutputDatasets} method is used to get the number of datasets 
#'     created by the code execution.
#' @inheritParams run-method
#' @return \code{countOutputDatasets} returns a numeric.
#' @rdname SASEGCode-class
#' @export
setMethod("countOutputDatasets", "SASEGCode", function(code) {
  return(clrGet(clrGet(code, "OutputDatasets"), "Count"))
})

#' Get the source code
#' 
#' \code{getSourceCode} method is used to retrieve the source code after execution.
#' @param code A code object.
#' @keywords internal
#' @exportMethod getSourceCode
setGeneric("getSourceCode", function(code) standardGeneric("getSourceCode"))

#' Get the source code
#' 
#' \code{getSourceCode} method is used to retrieve the source code that was executed.
#' @inheritParams run-method
#' @return \code{getSourceCode} returns a character string.
#' @rdname SASEGCode-class
#' @export
setMethod("getSourceCode", "SASEGCode", function(code) {
  return(clrGet(code, "Text"))
})


# SASEGDataset Class and Methods ------------------------------------------


#' An S4 class to represent a SAS EG OutputDataset object
#' 
#' The \code{SASEGDataset} object is an S4 class to represent a \code{SAS EG
#' Scripting OutputDataset} object.
#' @section Constructor: There is no constructor. \code{SASEGDataset} objects
#'   result from a \code{\linkS4class{SASEGCode}} execution, after calling the 
#'   \code{\linkS4class{SASEGCode}} method \code{\link{getListDatasets}}.
#' @examples
#' \dontrun{
#' # Modify the path below following your install:
#' path <- "C:\\Program Files\\SAS94\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"
#' loadSASEGScripting(path)
#' app <- SASEGApplication()
#' setProfile(app, "PROFILE")
#' my_new_project <- newProject(app)
#' my_server <- "SASPROD"
#' sas_program <- "data a; set sashelp.class; run;"
#' program_name <- "Test RSASEG"
#' my_new_code <- newCode(my_new_project,
#'                        my_server,
#'                        sas_program,
#'                        program_name)
#' run(my_new_code)
#' a <- getListDatasets(my_new_code)
#' 
#' getName(a[[1]])
#' getFileName(a[[1]])
#' path_to_csv <- saveAs(a[[1]], normalizePath("~"))
#' a.df <- data.table::fread(path_to_csv)                           
#' 
#' # Important: you have to quit SAS EG Application
#' terminate(app)
#' }
#' @seealso \code{\linkS4class{SASEGCode}}
setClass("SASEGDataset", contains = "SASObjRef")

#' Get the filename of an object
#' 
#' \code{getFileName} method is used to get the filename of an object.
#' @param ... Other parameters passed to method.
#' @keywords internal
#' @exportMethod getFileName
setGeneric("getFileName", function(object, ...) standardGeneric("getFileName"))

#' Get the filename of a SASEGDataset
#' 
#' \code{getFileName} method is used to get the filename of a \code{SASEGDataset}.
#' @inheritParams getName-method
#' @return \code{getFileName} returns a character string with the filename of the dataset 
#'     (ie. \emph{with} libname).
#' @rdname SASEGDataset-class
#' @export
setMethod("getFileName", "SASEGDataset", function(object) {
  clrGet(object, "FileName")
})

#' Get the list of the datasets
#' 
#' \code{getListsDatasets} method is used to retrieve a list of the dataset(s) 
#'     created by the code execution.
#' @keywords internal
#' @exportMethod getListDatasets
setGeneric("getListDatasets", function(code) standardGeneric("getListDatasets"))

#' Get the list of the datasets resulting from execution
#' 
#' \code{getListsDatasets} method is used to retrieve a named list of 
#'     \code{SASEGDataset} objects created by the code execution.
#' @inheritParams run-method
#' @return \code{getListDatasets} returns a list of \code{\linkS4class{SASEGDataset}} 
#'     objects named with their filenames (ie. "LIBNAME.NAME").
#' @rdname SASEGCode-class
#' @export
setMethod("getListDatasets", "SASEGCode", function(code) {
  n <- countOutputDatasets(code)
  l <- vector("list", n)
  enum <- clrCall(clrGet(code, "OutputDatasets"), "GetEnumerator")
  clrCall(enum, "MoveNext")
  i <- 1
  while(i <= n) {
    l[[i]] <- new("SASEGDataset", clrGet(enum, "Current"))
    names(l)[i] <- getFileName(l[[i]])
    clrCall(enum, "MoveNext")
    i <- i+1
  }
  return(l)
})

#' Get the name of an object
#' 
#' Get the name of an object.
#' @param object An object.
#' @param ... Other parameters passed to method.
#' @keywords internal
#' @exportMethod getName
setGeneric("getName", function(object, ...) standardGeneric("getName"))

#' Get the name of a SASEGDataset
#' 
#' \code{getName} method is used to get the name of a \code{SAS} dataset.
#' @param object A \code{SASEGDataset} object.
#' @return \code{getName} returns a character string with the name of the dataset 
#'     (ie. \emph{without} libname).
#' @rdname SASEGDataset-class
#' @export
setMethod("getName", "SASEGDataset", function(object) {
  clrGet(object, "Name")
})

#' Save a dataset as...
#' 
#' \code{saveAs} method is used to save a dataset in a local file.
#' @param dir Optional, a character string with the path of a directory. If \code{NULL}, the 
#'     temporary directory of the session is used.
#' @param name Optional, a character string with the name of the file (without extension). If 
#'     \code{NULL}, the name of the dataset is used.
#' @param type Optional, a character with the file format (\code{"csv"}, 
#'     \code{"tab"} or \code{"txt"}). \code{tab} is used by default.
#' @param fsep Optional, a character with a path separator passed to 
#'     \code{\link[base]{file.path}}. As \code{SAS EG} is a \code{Windows} 
#'     application, "\\" is used by default.
#' @inheritParams getName-method
#' @return \code{saveAs} method returns a character string containing the filepath.
#' @rdname SASEGDataset-class
#' @export
setMethod("saveAs", 
          "SASEGDataset", 
          function(object, dir = NULL, name = NULL, type = "tab", fsep = "\\") {
            if(is.null(dir)) {dir <- normalizePath(tempdir())}
            if(is.null(name)) {name <- getName(object)}
            path <- file.path(dir, paste0(name, ".", type), fsep = fsep)
            clrCall(object, "SaveAs", path)
            return(path)
            }
          )

#' Read an object
#' 
#' Read an object.
#' @param object An object.
#' @param ... Other parameters passed to method.
#' @keywords internal
setGeneric("read", function(object, ...) standardGeneric("read"))

#' Read a SASEGDataset object in R
#' 
#' \code{read} method is used to read a \code{SASEGDataset} and retrieve data 
#'     in a \code{data.table}.
#' @details \code{read} method stores data in a local temporary file and then imports 
#'     in \code{R} with \code{\link[data.table]{fread}}.
#' @param object A \code{SASEGDataset} object.
#' @return \code{read} returns a \code{data.table}.
#' @rdname SASEGDataset-class
#' @export
setMethod("read", "SASEGDataset", function(object) {
  path <- saveAs(object)
  d <- data.table::fread(path, sep = "\t")
  return(d)
})
