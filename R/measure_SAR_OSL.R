#' Measure an aliquot with the CW SAR OSL protocol
#' 
#' The function models the time-dependent photon counts of an aliquot 
#' according to the specified CW SAR OSL (continuous wave, single aliquot 
#' regenerative dose protocol for optically stimulated luminescence) sequence 
#' and parameters. The modelling is done for each component and photon count 
#' curves are summed to return an RLum.Analysis object as equivalent of 
#' importing a real measurement data set to the R-package 'Luminescence'.
#' 
#' The function uses the package 'RLumModel' to perform the simulation of the 
#' photon count curves. 
#' 
#' @param aliquot \code{Data frame}, a set of grains that are assigned to an 
#' aliquot (sample subset used for measurement), i.e., the result of 
#' \code{make_Aliquot}.
#' 
#' @param sequence \code{List}, Definition of the SAR protocol.
#' 
#' @param dose_rate \code{Numeric} value, Dose rate of the luminescence 
#' reader, in Gy.
#' 
#' @return \code{RLum.Analysis} object. Equivalent of the import result for 
#' a real world measurement file. This object can be evaluated by functions 
#' of the package 'Luminescence'.
#' 
#' @author Michael Dietze
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## create dummy data set
#' 
#' }
#' 
#' @export measure_SAR_OSL
#' 
measure_SAR_OSL <- function(
  
  aliquot,
  sequence,
  dose_rate = 1
) {
  
  ## PART 1 - separate OSL components -----------------------------------------
  
  ## collect model parameters
  N <- colMeans(x = aliquot[,grepl(x = colnames(aliquot), 
                                   pattern = "osl_N")])
  
  En <- colMeans(x = aliquot[,grepl(x = colnames(aliquot), 
                                   pattern = "osl_E")])
  
  s <- colMeans(x = aliquot[,grepl(x = colnames(aliquot), 
                                   pattern = "osl_s")])
  
  A <- colMeans(x = aliquot[,grepl(x = colnames(aliquot), 
                                   pattern = "osl_A")])
  
  B <- colMeans(x = aliquot[,grepl(x = colnames(aliquot), 
                                   pattern = "osl_B")])
  
  Th <- colMeans(x = aliquot[,grepl(x = colnames(aliquot), 
                                   pattern = "osl_Th")])
  
  E_th <- colMeans(x = aliquot[,grepl(x = colnames(aliquot), 
                                   pattern = "osl_E_th")])
  
  R <- mean(x = aliquot[,grepl(x = colnames(aliquot), 
                                      pattern = "osl_R")])

  parameters <- list(N = as.numeric(N),
                     E = as.numeric(En),
                     s = as.numeric(s),
                     A = as.numeric(A),
                     B = as.numeric(B),
                     Th = as.numeric(Th),
                     E_th = as.numeric(E_th),
                     model = "customized",
                     R = as.numeric(R))

  ## calculate mean burial dose
  burial_dose <- mean(aliquot$osl_doserate * aliquot$age)
  
  ## update sequence
  sequence[[7]][2] <- burial_dose

  ## PART 2 - model luminescence ----------------------------------------------
  osl_model <- RLumModel::model_LuminescenceSignals(
    sequence = sequence,
    model = "customized",
    own_parameters = parameters,
    plot = FALSE,
    verbose = FALSE)
  
  ## return function output
  return(osl_model)
}
