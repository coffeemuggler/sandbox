#' @title Measure an aliquot with the CW SAR OSL protocol
#' 
#' @description The function models the time-dependent photon counts of an aliquot 
#' according to the specified CW SAR OSL (continuous wave, single aliquot 
#' regenerative dose protocol for optically stimulated luminescence) sequence 
#' and parameters. The modelling is done for each component and photon count 
#' curves are summed to return an [Luminescence::RLum.Analysis-class] object as equivalent of 
#' importing a real measurement data set to the R-package [Luminescence-package].
#' 
#' The function uses the package [RLumModel-package] to perform the simulation of the 
#' photon count curves. 
#' 
#' @param aliquot [data.frame], a set of grains that are assigned to an 
#' aliquot (sample subset used for measurement), i.e., the result of 
#' [prepare_Aliquot].
#' 
#' @param sequence [list], Definition of the SAR protocol.
#' 
#' @param dose_rate [numeric] value, Dose rate of the luminescence 
#' reader, in Gy.
#' 
#' @return [Luminescence::RLum.Analysis-class] object. Equivalent of the import result for 
#' a real world measurement file. This object can be evaluated by functions 
#' of the package [Luminescence-package].
#' 
#' @author Michael Dietze, GFZ Potsdam (Germany)
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## create dummy data set
#' 
#' }
#' 
#' @md 
#' @export measure_SAR_OSL
measure_SAR_OSL <- function(
  aliquot,
  sequence,
  dose_rate = 0.1
) {
  
  ## PART 1 - separate OSL components -----------------------------------------
  
  ## collect model parameters
  N <- colMeans(x = aliquot[,grepl(x = colnames(aliquot), 
                                   pattern = "osl_N")])
  
  En <- colMeans(x = aliquot[,grepl(x = colnames(aliquot), 
                                    pattern = "osl_E") & 
                               !grepl(x = colnames(aliquot), 
                                      pattern = "osl_E_th")])
  
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
  sequence$Irr_2recover <- burial_dose
  
  ## PART 2 - model luminescence ----------------------------------------------
  osl_model <- RLumModel::model_LuminescenceSignals(
    model = "customized",
    sequence = sequence, 
    lab.dose_rate = dose_rate,
    own_parameters = parameters,
    plot = FALSE,
    verbose = FALSE, 
    simulate_sample_history = TRUE)
  
  ## return function output
  return(osl_model)
}
