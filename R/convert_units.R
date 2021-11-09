#' @title Convert between phi units and micrometers
#' 
#' @description The function converts values from the phi-scale to 
#' the micrometer-scale and vice versa.
#' 
#' @param phi [numeric] vector, grain-size class values 
#' in phi to be converted
#' 
#' @param mu [numeric] vector, grain-size class values 
#' in micrometres to be converted
#' 
#' @return [numeric] vector, converted grain-size class values
#' 
#' @author Michael Dietze, GFZ Potsdam (Germany)
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
#' @md
#' @export convert_units
convert_units <- function(
  phi, 
  mu
) {
  if (missing(mu)) return(1000 * 2 ^ -phi)
  if (missing(phi)) return(-log2(mu / 1000))

  stop("[convert_units()] No correct variables provided!", call. = FALSE)
}