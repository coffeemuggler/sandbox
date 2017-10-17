#' Sieve a sample.
#'
#' The function removes grains that are not within the provided sieve
#' interval.
#'
#'
#' @param sample \code{Data frame} sample object to be sieved.
#' 
#' @param interval \code{Numeric} vector, sieve interval.
#' 
#' @return \code{Data frame} with grains that are within the sieve interval.
#' 
#' @author Michael Dietze
#' 
#' @examples
#' ## load example data set
#' data(sample)
#' 
#' ## sieve sample (in phi units)
#' sample_sieved <- prepare_Sieving(sample = sample,
#'                                  interval = c(5, 6))
#'                                  
#' ## plot results
#' plot(density(x = sample$grainsize, 
#'              from = -1, 
#'              to = 11))
#' lines(density(x = sample_sieved$grainsize, 
#'               from = -1, 
#'               to = 11), 
#'       col = 2)
#' 
#'
#'
#' @export prepare_Sieving
#' 
prepare_Sieving <- function(
  sample,
  interval
) {
  
  ## check/adjust input parameters --------------------------------------------
  
  ## set output flag
  output_flag <- TRUE
  
  ## check data format and structure
  if(class(interval) != "numeric" | length(interval) != 2) {
    
    warning("Parameter interval must be numeric of length two!")
    
    interval <- range(sample$grainsize, 
                      na.rm = TRUE)
  }
  
  ## sieve sample -------------------------------------------------------------
  sample <- sample[sample$grainsize > min(interval) & 
                     sample$grainsize <= max(interval),]
  
  ## return output ------------------------------------------------------------
  if(output_flag == TRUE) {
    
    ## return output
    return(sample)
  }
}
