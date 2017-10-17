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
#' @param dose_sequence \code{umeric} vector, doses in Gy applied to the  
#' aliquot during each measurement step, according to the SAR protocol. See  
#' documentation of \code{RLumModel::model_LuminescenceSignals} and 
#' \code{Luminescence::analyse_SAR.CWOSL()} for further information.
#' 
#' @param dose_test \code{Numeric} value, Test dose for SAR cycles in Gy.
#' 
#' @param preheat \code{Numeric} value, preheat temperature in Degree Celsius.
#' 
#' @param cutheat \code{Numeric} value, cutheat temperature in Degree Celsius.
#' 
#' @param oslheat \code{Numeric} value, temperature at which OSL is 
#' measuered, in Degree Celsius.
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
#' aliquot <- data.frame(comp_lum = rep(1, 100),
#'                       N1 = runif(100, min = 1e12, max = 1e13),
#'                       N2 = runif(100, min = 1e12, max = 1e13),
#'                       N3 = runif(100, min = 1e12, max = 1e13),
#'                       N4 = runif(100, min = 1e12, max = 1e13),
#'                       E1 = rep(0, 100),
#'                       E2 = rep(0, 100),
#'                       E3 = rep(0, 100),
#'                       E4 = rep(0, 100),
#'                       s1 = rep(0, 100),
#'                       s2 = rep(0, 100),
#'                       s3 = rep(0, 100),
#'                       s4 = rep(0, 100),
#'                       A1 = rep(2e-8, 100),
#'                       A2 = rep(2e-9, 100),
#'                       A3 = rep(4e-9, 100),
#'                       A4 = rep(1e-8, 100),
#'                       B1 = rep(0, 100),
#'                       B2 = rep(0, 100),
#'                       B3 = rep(5e-11, 100),
#'                       B4 = rep(4e-8, 100),
#'                       K = rep(0, 100),
#'                       model = rep("customized", 100),
#'                       R = rep(1.7e15, 100))
#'                       
#' ## define dose sequence
#' sequence <- c(0, 80, 140, 260, 320, 0, 80)
#' 
#' ## measure aliquot with CW OSL SAR protocol
#' data <- measure_SAR_OSL(aliquot = aliquot,
#'                         dose_sequence = sequence,
#'                         dose_test = 20,
#'                         preheat = 220,
#'                         cutheat = 220,
#'                         oslheat = 125)
#'                         
#' ## analyse measured data set
#' results <- Luminescence::analyse_SAR.CWOSL(data,
#'                                            signal.integral.min = 1,
#'                                            signal.integral.max = 7,
#'                                            background.integral.min = 301,
#'                                            background.integral.max = 401,
#'                                            fit.method = "EXP",
#'                                            dose.points = sequence)
#' 
#' }
#' 
#' @export measure_SAR_OSL
#' 
measure_SAR_OSL <- function(
  
  aliquot,
  dose_sequence,
  dose_test,
  preheat,
  cutheat,
  oslheat
) {
  
  ## PART 1 - separate OSL components -----------------------------------------
  
  ## extract luminescence components
  components <- as.list(unique(aliquot$comp_lum))
  
  ## collect model parameters for each component
  model_luminescence <- lapply(X = components, FUN = function(X, aliquot) {
    
    x <- aliquot[aliquot$comp_lum == X,]
    
    N <- as.numeric(lapply(X = x[,grepl(x = names(x), 
                                        pattern = "N",
                                        fixed = TRUE)], FUN = function(x) {
                                          
                                          sum(x)
                                        }))
    
    E <- as.numeric(lapply(X = x[,grepl(x = names(x), 
                                        pattern = "E",
                                        fixed = TRUE)], FUN = function(x) {
                                          
                                          unique(x)
                                        }))
    
    s <- as.numeric(lapply(X = x[,grepl(x = names(x), 
                                        pattern = "s",
                                        fixed = TRUE)], FUN = function(x) {
                                          
                                          unique(x)
                                        }))
    
    A <- as.numeric(lapply(X = x[,grepl(x = names(x), 
                                        pattern = "A",
                                        fixed = TRUE)], FUN = function(x) {
                                          
                                          unique(x)
                                        }))
    
    B <- as.numeric(lapply(X = x[,grepl(x = names(x), 
                                        pattern = "B",
                                        fixed = TRUE)], FUN = function(x) {
                                          
                                          unique(x)
                                        }))
    
    K <- unique(x$K)
    
    model <- as.character(unique(x$model))
    
    R <- unique(x$R)
    
    parameters <- list(N = N,
                       E = E,
                       s = s,
                       A = A,
                       B = B,
                       K = K,
                       model = model,
                       R = R)
    
    return(parameters)
  },
  aliquot = aliquot)
  
  ## PART 2 - model luminescence ----------------------------------------------
  
  ## define OSL sequence
  sequence <- list(
    RegDose = dose_sequence,
    TestDose = dose_test,
    PH = preheat,
    CH = cutheat,
    OSL_temp = oslheat
  )
  
  ## model luminsecence signals per component and sequence
  osl <- lapply(X = model_luminescence, FUN = function(model_luminescence) {
    
    RLumModel::model_LuminescenceSignals(
      sequence = sequence,
      model = model_luminescence$model, 
      own_parameters = model_luminescence,
      own_state_parameters = c(0, 0, 0, 9.4e15),
      plot = FALSE,
      verbose = FALSE
    )
  })
  
  ## extract shine down curves
  osl_rlum <- lapply(X = osl, FUN = function(osl) {
    
    i <- !grepl(x = names(osl), pattern = "conc.")
    
    return(osl[[i]])
  })
  
  ## sum shine down curves
  osl_sum <- osl_rlum[[1]]
  
  for(i in 1:length(osl_sum)) {
    
    counts <- lapply(X = osl_rlum, FUN = function(x, i) {
      
      x[[i]]@data[,2]
    }, 
    i = i)
    
    osl_sum[[i]]@data[,2] <- colSums(do.call(rbind, counts))
  }
  
  ## build RLum.Analysis-object
  osl_out <- Luminescence::set_RLum(records = osl_sum, 
                                    class = "RLum.Analysis")
  
  ## return function output
  return(osl_out)
}
