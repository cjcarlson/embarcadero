
.onLoad <- function(libname, pkgname) {
  methods::setOldClass(c("bart","rbart"))
  methods::setMethod("predict", "bart", predict2.bart)
  methods::setMethod("summary", "bart", summary.bart)
  methods::setMethod("predict", "rbart", predict2.bart)
  methods::setMethod("summary", "rbart", summary.bart)
  invisible()
}
