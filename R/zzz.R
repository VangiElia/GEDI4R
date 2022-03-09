.onLoad <- function(libname, pkgname) {
  invisible(suppressPackageStartupMessages(
    sapply(c("snow", "foreach"),
           requireNamespace, quietly = TRUE)
  ))
}

.onUnload <- function (libpath) {
  library.dynam.unload("rGEDI4", libpath)
  invisible()
}
