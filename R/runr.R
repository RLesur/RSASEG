#' @import runr
#' @include RSASEGDriver.R
NULL

#' Runr proc
#' 
#' Function to use \code{SAS} engine with runr.
#' @inheritParams SASEG
#' @inheritParams dbConnect,SASEGDriver-method
#' @keywords internal
#' @export
proc_saseg <- function(DLLPath, profile, server, projectpath = NULL, ...) {
  # Init a void connection:
  conn <- new("SASEGConnection")
  sep = runr:::rand_string()
  started <- FALSE
  exec_code = function(codeName = NULL, out.df = NULL, ...) {
    if (!started) stop('SAS EG has not been started yet')
    code = as.character(paste0(c(...), collapse = "\n"))
    result <- dbSendQuery(conn = conn, statement = SAS(code), codeName = codeName)
    if(!is.null(out.df)) {
      out <- dbFetchAll(result)
      assign(out.df, out, envir = globalenv())
    }
    log <- dbGetLog(result)
    runr:::split_results(paste(code, log, sep = sep), sep)
  }

  list(
    start = function() {
      if (started) {
        warning('SAS EG program is already started')
        return(invisible())
      }
      conn <<- dbConnect(SASEG(DLLPath = DLLPath), profile = profile, server = server)
      started <<- TRUE
      invisible()
    },

    exec = exec_code,

    running = function() started,

    stop = function() {
      dbDisconnect(conn, projectPath = projectpath)
      started <<- FALSE
      invisible()
    }
  )
}
