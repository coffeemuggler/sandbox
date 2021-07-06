#' Convert between phi units and micrometers
#' 
#' The function converts values from the phi-scale to 
#' the micrometer-scale and vice versa.
#' 
#' @param phi \code{Numeric} vector, grain-size class values 
#' in phi to be converted
#' 
#' @param mu \code{Numeric} vector, grain-size class values 
#' in micrometres to be converted
#' 
#' @return \code{Numeric} vector, converted grain-size class values
#' 
#' @author Michael Dietze
#' 
#' @examples
#' ## load example data set
#' ## generate phi-values
#' phi <- -2:5
#' 
#' ## convert and show phi to mu
#' mu  <- convert_units(phi = phi)
#' mu
#' 
#' ## convert and show mu to phi
#' convert_units(mu = mu)
#' 
#' @export convert_units

convert_units <- function (
  
  phi, 
  mu
  
) {
  if (missing(mu) == TRUE) {
    result <- 1000 * 2^-phi
  }
  else if (missing(phi) == TRUE) {
    result <- -log2(mu/1000)
  }
  else {
    stop("No correct variables provided")
  }
  return(result)
}