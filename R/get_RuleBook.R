#' Get one of a series of predefined rule books for a model run.
#'
#' The function returns a prebuilt model rule book, i.e., a combination of
#' model parameters and rules.
#'
#'
#' @param book \code{Character} scalar, name of the rule book to be generated.
#'        One out of \code{"empty"}, default is \code{"empty"}.
#' @return A list object with all rules for a model run.
#' @author Michael Dietze
#' @examples
#'
#' ## create simple true age-depth-relationship
#' book_flat <- get_RuleBook(book = "empty")
#'
#' @export get_RuleBook
get_RuleBook <- function(
  book = "empty"
) {
  
  ## define dummy function/closure
  fun_dummy <- splinefun(x = c(0, 1),
                         y = c(0, 1))
  
  ## definition of rule book "flat" -------------------------------------------
  if(book == "empty") {
    
    rule_book <- list(
      ## title
      book = "empty",
      
      ## global information
      age = list(
        group = "general",
        age = list(
          type = "exact",
          value = fun_dummy)),
      
      dose_rate = list(
        group = "general",
        dose_rate = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy)),
      
      water_content = list(
        group = "general",
        water_content = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy)),
      
      ## component-specific information
      population = list(
        group = "specific",
        population_1 = list(
          type = "exact",
          value = fun_dummy),
        population_2 = list(
          type = "exact",
          value = fun_dummy)),
      
      grainsize = list(
        group = "specific",
        grainsize_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        grainsize_2 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy)),
      
      density = list(
        group = "specific",
        density_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        density_2 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy)),
      
      packing = list(
        group = "specific",
        packing_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        packing_2 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy)),
      
      photon_equivalent = list(
        group = "specific",
        photon_equivalent_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        photon_equivalent_2 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy)),
      
      predose = list(
        group = "specific",
        predose_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        predose_2 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy)),
      
      element_Na = list(
        group = "specific",
        element_Na_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        element_Na_2 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy)),
      
      element_Mg = list(
        group = "specific",
        element_Mg_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        element_Mg_2 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy)),
      
      element_Al = list(
        group = "specific",
        element_Al_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        element_Al_2 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy)),
      
      element_K = list(
        group = "specific",
        element_K_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        element_K_2 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy)),
      
      element_Ca = list(
        group = "specific",
        element_Ca_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        element_Ca_2 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy)),
      
      element_Zn = list(
        group = "specific",
        element_Zn_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        element_Zn_2 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy)),
      
      element_Sr = list(
        group = "specific",
        element_Sr_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        element_Sr_2 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy)))
  } else if(book == "osl") {
    
    ## get empty rule book
    rule_book <- get_RuleBook(book = "empty")
    
    ## assign new book name
    rule_book$book <- "osl"
    
    ## append OSL parameters - N
    for(i in 1:9) {
      
      rule_book[[length(rule_book) + 1]] <- list(
        group = "specific",
        osl_Ni_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        osl_Ni_2 = list(
          type = "rnorm",
          value = fun_dummy))
      
      names(rule_book)[length(rule_book)] <- 
        paste("osl_N",i, sep = "")
      
      names(rule_book[[length(rule_book)]]) <- 
        c("group",
          paste("osl_N",i, "_1", sep = ""),
          paste("osl_N",i, "_2", sep = ""))
    }
    
    ## append OSL parameters - E
    for(i in 1:9) {
      
      rule_book[[length(rule_book) + 1]] <- list(
        group = "specific",
        osl_Ei_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        osl_Ei_2 = list(
          type = "rnorm",
          value = fun_dummy))
      
      names(rule_book)[length(rule_book)] <- 
        paste("osl_E",i, sep = "")
      
      names(rule_book[[length(rule_book)]]) <- 
        c("group",
          paste("osl_E",i, "_1", sep = ""),
          paste("osl_E",i, "_2", sep = ""))
    }

    ## append OSL parameters - s
    for(i in 1:9) {
      
      rule_book[[length(rule_book) + 1]] <- list(
        group = "specific",
        osl_si_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        osl_si_2 = list(
          type = "rnorm",
          value = fun_dummy))
      
      names(rule_book)[length(rule_book)] <- 
        paste("osl_s",i, sep = "")
      
      names(rule_book[[length(rule_book)]]) <- 
        c("group",
          paste("osl_s",i, "_1", sep = ""),
          paste("osl_s",i, "_2", sep = ""))
    }
    
    ## append OSL parameters - A
    for(i in 1:9) {
      
      rule_book[[length(rule_book) + 1]] <- list(
        group = "specific",
        osl_Ai_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        osl_Ai_2 = list(
          type = "rnorm",
          value = fun_dummy))
      
      names(rule_book)[length(rule_book)] <- 
        paste("osl_A",i, sep = "")
      
      names(rule_book[[length(rule_book)]]) <- 
        c("group",
          paste("osl_A",i, "_1", sep = ""),
          paste("osl_A",i, "_2", sep = ""))
    }
    
    ## append OSL parameters - B
    for(i in 1:9) {
      
      rule_book[[length(rule_book) + 1]] <- list(
        group = "specific",
        osl_Bi_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        osl_Bi_2 = list(
          type = "rnorm",
          value = fun_dummy))
      
      names(rule_book)[length(rule_book)] <- 
        paste("osl_B",i, sep = "")
      
      names(rule_book[[length(rule_book)]]) <- 
        c("group",
          paste("osl_B",i, "_1", sep = ""),
          paste("osl_B",i, "_2", sep = ""))
    }
    
    ## append OSL parameters - Th
    for(i in 1:5) {
      
      rule_book[[length(rule_book) + 1]] <- list(
        group = "specific",
        osl_Thi_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        osl_Thi_2 = list(
          type = "rnorm",
          value = fun_dummy))
      
      names(rule_book)[length(rule_book)] <- 
        paste("osl_Th",i, sep = "")
      
      names(rule_book[[length(rule_book)]]) <- 
        c("group",
          paste("osl_Th",i, "_1", sep = ""),
          paste("osl_Th",i, "_2", sep = ""))
    }
    
    ## append OSL parameters - E_th
    for(i in 1:5) {
      
      rule_book[[length(rule_book) + 1]] <- list(
        group = "specific",
        osl_E_thi_1 = list(
          type = "rnorm",
          mean = fun_dummy,
          sd = fun_dummy),
        osl_E_thi_2 = list(
          type = "rnorm",
          value = fun_dummy))
      
      names(rule_book)[length(rule_book)] <- 
        paste("osl_E_th",i, sep = "")
      
      names(rule_book[[length(rule_book)]]) <- 
        c("group",
          paste("osl_E_th",i, "_1", sep = ""),
          paste("osl_E_th",i, "_2", sep = ""))
    }
    
    rule_book$osl_R = list(
      group = "specific",
      osl_R_1 = list(
        type = "exact",
        value = fun_dummy),
      element_Ca_2 = list(
        type = "rnorm",
        value = fun_dummy))
    

  }
  
  ## return output ------------------------------------------------------------
  return(rule_book)
  
}
