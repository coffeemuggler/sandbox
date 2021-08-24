#' Prepare subsamples from a sample data set
#' 
#' The function splits the master sample in a set of subsamples. The
#' step can be done by creating equally large subsamples in terms of 
#' contained grains (parameter \code{number}), by volume (parameter 
#' \code{volume}) or by weight (parameter \code{weight}).
#' 
#' @param sample \code{Data frame}, sample object to be distributed to 
#' aliquots.
#' 
#' @param number \code{Numeric} value, number of evenly large subsamples to 
#' be created
#' 
#' @param volume \code{Numeric} value, volume of subsamples. Remainder 
#' of the master sample that is too small for the last subsample is 
#' removed. Volume must be given in m^3 and takes packing density of 
#' the sample into account.
#' 
#' @param weight \code{Numeric} value, weight of the subsamples. Remainder 
#' of the master sample that is too small for the last subsample is 
#' removed. Weight is calculated based on density of each grain. Weight 
#' must be given in kg.
#' 
#' @return \code{List} object with grains organised as aliquots, i.e. list 
#' elements.
#' 
#' @author Michael Dietze
#' 
#' @examples
#' ## load example data set
#' data(sample)
#'
#' @export prepare_Subsample
#' 
prepare_Subsample <- function(
  sample,
  number, 
  volume,
  weight
) {
  
  ## check if only one subsampling criterion is given
  if(sum(c(!missing(number), !missing(volume), !missing(weight))) > 1) {
    
    stop("Only one criterion for subsampling is allowed!")
  }
  
  ## option 1, subsampling by number of grains
  if(missing(number) == FALSE) {
    
    ## get number of grains in sample
    n_grains <- length(sample$grains)
    
    ## get number of grains per sample
    n_sub <- floor(n_grains / number)
    
    ## create subsampling vector
    i_sub <- seq(from = 1, to = n_grains, by = n_sub)
    
    ## create and fill slice object with grain depths  
    sample_sub <- vector(mode = "list", 
                         length = length(i_sub) - 1)
    
    for(i in 1:length(sample_sub)) {
      
      sample_sub[[i]] <- sample[(i_sub[i]):(i_sub[i + 1] - 1),]
    }
  }
  
  ## option 2, subsampling by volume
  if(missing(volume) == FALSE) {
    
    ## get grain radius
    r_grains <- EMMAgeo::convert.units(phi = sample$grainsize) / (2 * 10^6)
    
    ## get grain volumes
    v_grains <- cumsum(4 / 3 * pi * r_grains^3 * 1/sample$packing)
    
    ## get class boundaries of individual volumes
    v_subset <- c(seq(from = volume, to = max(v_grains), by = volume))
    
    ## assign grains to subsamples
    sample_sub <- lapply(X = v_subset,
                         FUN = function(x, v_grains, volume, sample) {
                           
                           sample[v_grains > x - volume & v_grains <= x,]
                         }, v_grains, volume,sample)
  }
  
  ## option 2, subsampling by weight
  if(missing(weight) == FALSE) {
    
    ## get grain radius
    r_grains <- EMMAgeo::convert.units(phi = sample$grainsize) / (2 * 10^6)
    
    ## get grain volumes
    v_grains <- 4 / 3 * pi * r_grains^3
    
    ## get grain masses
    m_grains <- cumsum(sample$density * v_grains)
    
    ## get class boundaries of individual volumes
    m_subset <- c(seq(from = weight, to = max(m_grains), by = weight))
    
    ## assign grains to subsamples
    sample_sub <- lapply(X = m_subset,
                         FUN = function(x, m_grains, weight, sample) {
                           
                           sample[m_grains > x - weight & m_grains <= x,]
                         }, m_grains, weight, sample)
  }
  
  ## return output
  return(sample_sub)
}