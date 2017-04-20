#' @import methods
#' @include utils.R
NULL

setGeneric("SASFormat", function(x, ...) standardGeneric("SASFormat"))

setMethod("SASFormat", "Date", function(x) {
  vapply(x, SASDateConstant, character(1))
})

setMethod("SASFormat", "POSIXt", function(x) {
  x <- as.POSIXct(x)
  vapply(x, SASDateTimeConstant, character(1))
})

setMethod("SASFormat", "difftime", function(x) {
  vapply(x, SASTimeConstant, character(1), units(x))
})

setMethod("SASFormat", "logical", function(x) {
  vapply(x, as.numeric, numeric(1))
})


setGeneric("getSASType", function(obj, ...) standardGeneric("getSASType"))

setOldClass("AsIs")

setMethod("getSASType", "AsIs", function(obj) {
  if(length(class(obj)) == 1) {
    obj <- unclass(obj)
  } else {
    class(obj) <- class(obj)[!class(obj) == "AsIs"]
  }
  return(getSASType(obj))
})

setMethod("getSASType", "logical", function(obj) return("SMALLINT"))

setMethod("getSASType", "integer", function(obj) return("INTEGER"))

setMethod("getSASType", "numeric", function(obj) return("REAL"))

setMethod("getSASType", "character", function(obj) return("VARCHAR(32767)"))

setMethod("getSASType", "Date", function(obj) return("NUM INFORMAT=DATE9. FORMAT=E8601DA."))

setMethod("getSASType", "POSIXct", function(obj) return("NUM INFORMAT=DATETIME. FORMAT=E8601DT19."))

setOldClass("difftime")

setMethod("getSASType", "difftime", function(obj) return("NUM INFORMAT=TIME. FORMAT=TIME."))

setMethod("getSASType", "factor", function(obj) return("VARCHAR(32767)"))

setMethod("getSASType", "ordered", function(obj) return("VARCHAR(32767)"))

setMethod("getSASType", "data.frame", function(obj) return(vapply(obj, getSASType, character(1))))

setMethod("getSASType", "list", function(obj) {
  if(all(vapply(obj, is.raw, logical(1)))) {
    stop("BLOBs are not supported by SAS.")
  } else {
    stop("Type not supported by SAS.")
  }
})
