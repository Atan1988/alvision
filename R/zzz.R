#' @import Rcpp
#' @import methods
#' @importFrom graphics arrows par plot rasterImage points symbols
#' @importFrom stats median.default
#' @import pbapply
#' @import ROpenCVLite
#' @importFrom grDevices col2rgb

cv2 <- NULL
ipywidgets <- NULL
np <- NULL
requests <- NULL
py_time <- NULL
py_built <- NULL

.onLoad <- function(libname, pkgname) {
  cv2 <<- reticulate::import('cv2', delay_load = TRUE)
  #imutils <<- reticulate::import('imutils', delay_load = TRUE)
  pdf2image <<- reticulate::import('pdf2image', delay_load = TRUE)
  ipywidgets <<- reticulate::import('ipywidgets', delay_load = TRUE)
  np <<- reticulate::import('numpy', delay_load = TRUE)
  requests <<- reticulate::import('requests', delay_load = TRUE)
  py_time <<- reticulate::import('time', delay_load = TRUE)
  py_built <<- reticulate::import('builtins', delay_load = T)

}



### Cleanup function ###
.onUnload <- function(libpath) {
  library.dynam.unload("alvision", libpath)
}