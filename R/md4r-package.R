## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib md4r, .registration = TRUE
## usethis namespace: end
NULL

##########
##
## Utility functions
##
#########

`%||%` = function (x, y) {
  if (is.null(x)) y
  else            x
}
