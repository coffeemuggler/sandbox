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
          sd = fun_dummy)))

  }

  ## return output ------------------------------------------------------------
  return(rule_book)
  
}
