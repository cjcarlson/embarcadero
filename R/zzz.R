
.onLoad <- function(libname, pkgname) {
  setOldClass("bart")
  setMethod("predict", "bart", predict2.bart)
  setMethod("summary", "bart", summary.bart)
  setMethod("predict", "rbart", predict2.bart)
  setMethod("summary", "rbart", summary.bart)
  invisible()
}