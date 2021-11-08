#' Get one of a series of predefined rule books for a model run.
#'
#' The function returns a pre-built model rule book, i.e., a combination of
#' model parameters and rules.
#'
#' It is possible to generate OSL-tailored rule books. For this, the 
#' argument `osl` must be provided with a keyword defining one of the 
#' OSL models from the R package `'RLumModel'`: `"Bailey2001"`, 
#' `"Bailey2004"`, `"Pagonis2008"`, `"Pagonis2007"`, 
#' `"Bailey2002"` and `"Friedrich2017"`. The model parameters will 
#' be appended to the rule book entries and defined by mean and standard 
#' deviation. 
#'
#' @param book [character] value, name of the rule book to be generated.
#' One out of `"empty"`, default is `"empty"`.
#'        
#' @param osl [character] value, optional keyword for an OSL (optical 
#' stimulated luminescence) model of choice. Must be one of the available 
#' models from the R package `'RLumModel'`. See details for full list of 
#' available models.
#' 
#' @return A list object with all rules for a model run.
#' 
#' @author Michael Dietze, GFZ Potsdam (Germany)
#' 
#' @examples
#'
#' ## create simple true age-depth-relationship
#' book_flat <- get_RuleBook(book = "empty")
#'
#' @md
#' @export get_RuleBook
get_RuleBook <- function(
  book = "empty",
  osl = NULL
) {
  
  ## define dummy function/closure
  fun_dummy <- splinefun(x = c(0, 1),
                         y = c(0, 1))
  
  ## definition of rule book "flat" -------------------------------------------
  if (book == "empty") {
    rule_book <- list(
      ## title
      book = "empty",
      
      ## global information
      age = list(
        group = "general",
        age = list(
          type = "exact",
          value = fun_dummy)),

      ## component-specific information
      population = list(
        group = "specific",
        population_1 = list(
          type = "exact",
          value = fun_dummy)),
      
      grainsize = list(
        group = "specific",
        grainsize_1 = list(
          type = "normal",
          mean = fun_dummy,
          sd = fun_dummy)),
      
      packing = list(
        group = "specific",
        packing_1 = list(
          type = "normal",
          mean = fun_dummy,
          sd = fun_dummy)),
      
      density = list(
        group = "specific",
        density_1 = list(
          type = "normal",
          mean = fun_dummy,
          sd = fun_dummy)))

  }
  
  if(!is.null(osl) == TRUE) {
    
    osl_parameters <- RLumModel::.set_pars(model = osl)
    
    osl_parameters <- osl_parameters[1:7]
    
    for(i in 1:length(osl_parameters)){
      
      osl_parameters[[i]] <- 
        paste("osl_", rep(x = names(osl_parameters)[i], 
                          times = length(osl_parameters[[i]])),
              1:length(osl_parameters[[i]]),
              sep = "")
    }
    
    osl_parameters <- c("osl_doserate",
                        as.character(unlist(osl_parameters)),
                        "osl_R")
    
    for(i in 1:length(osl_parameters)) {
      rule_book <- add_Rule(book = rule_book, 
                         name = osl_parameters[i], 
                         group = "specific",
                         type = "normal", 
                         populations = 1)
    }
  }

  ## return output ------------------------------------------------------------
  return(rule_book)
  
}
