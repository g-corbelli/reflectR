#' @keywords internal
#' Handles package startup messages
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("reflectR: a package to automatically code open-ended responses to the Cognitive Reflection Test.")
  packageStartupMessage("For more information on getting started, type ?reflectR.")
  packageStartupMessage("For suggestions or to report issues, please contact Dr. Giuseppe Corbelli at giuseppe.corbelli@uninettunouniversity.net.")
}
