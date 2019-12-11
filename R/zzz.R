
.onLoad <- function(libname, pkgname) {
  setOldClass("bart")
  setMethod("predict", "bart", predict2.bart)
  setMethod("summary", "bart", summary.bart)
  invisible()
}