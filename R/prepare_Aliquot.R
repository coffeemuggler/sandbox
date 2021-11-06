#' Prepare aliquots from sample data set
#' 
#' The function consecutively fills aliquots (i.e., subsamples distributed on 
#' round carrier discs) with grains from an input sample. remaining grains that 
#' are not enough to fill a further aliquot are discarded.
#' 
#' @param sample \code{Data frame}, sample object to be distributed to 
#' aliquots.
#' 
#' @param diameter \code{Numeric} value, diameter of the alquot sample 
#' carriers in mm.
#' 
#' @param density \code{Numeric} value, packing density of the grains on
#' the sample carrier. Default is \code{0.65}.
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
#' A <- prepare_Aliquot(sample = sample, 
#'                      diameter = 5)
#' 
#' B <- prepare_Aliquot(sample = sample, 
#'                      diameter = 2, 
#'                      density = 0.6)
#' @md
#' @export prepare_Aliquot
prepare_Aliquot <- function(
  sample,
  diameter, # mm
  density = 0.65
) {
  
  area_aliqout <- pi * (diameter / 1000)^2 / 4

  diameter_grains <- EMMAgeo::convert.units(phi = sample$grainsize) / 10^6
  
  area_grains <- pi * diameter_grains^2 / 4 / density
  
  area_grains_cum <- cumsum(area_grains)
  
  aliquot_n <- seq(from = area_aliqout, 
                   to = max(area_grains_cum), 
                   by = area_aliqout)
  
  aliquot_i <- seq(from = 1, 
                   to = length(area_grains))
  
  aliquot_cut <- numeric(length = length(aliquot_n))
  
  for(i in 1:length(aliquot_cut)) {
    
    i_cut <- abs(area_grains_cum - aliquot_n[i]) == 
      min(abs(area_grains_cum - aliquot_n[i]))
    
    aliquot_cut[i] <- aliquot_i[i_cut]
  }
  
  aliquot_cut <- c(1, aliquot_cut)

  aliquots <- vector(mode = "list", 
                     length = length(aliquot_n))
  
  for(i in 1:length(aliquots)) {
    
    aliquots[[i]] <- sample[aliquot_cut[i]:aliquot_cut[i + 1],]
  }
  
  return(aliquots)
}