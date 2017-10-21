#' Add a rule to a rule book
#'
#' The function adds a new rule to an existing rule book. The specified rule 
#' will be appended to the rule book.
#'
#' @param book \code{Character} value, name of the rule book to be modified.
#' 
#' @param name \code{Character} value, name of the rule rule to be added.
#' 
#' @param group \code{Character} value, group to which the rule belongs. One 
#' out of \code{"general"} (covering the sediment section properties) and 
#' \code{"specific"} (relevant for a single grain).
#' 
#' @param type \code{Character} value, generic type of the rule. One out of 
#' \code{"rnorm"} (definition by mean and standard deviation, changing with 
#' depth) and \code{"exact"} (defined by exact value, changing with depth).
#' 
#' @param populations \code{Numeric} value, number of populations to create.
#' The number of populations to add should match the existing number of 
#' populations.
#' 
#' @return A list object with all rules for a model run.
#' 
#' @author Michael Dietze
#' 
#' @examples
#'
#' ## create simple true age-depth-relationship
#' book_1 <- get_RuleBook()
#' 
#' book_2 <- add_Rule(book = book_1, 
#'                           name = "extrarule", 
#'                           group = "general", 
#'                           type = "rnorm", 
#'                           populations = 1)
#'                           
#' @export add_Rule
#' 
add_Rule <- function(
  
  book,
  name,
  group,
  type,
  populations = 1
) {
  
  ## define dummy function/closure
  fun_dummy <- splinefun(x = c(0, 1),
                         y = c(0, 1))
  
  ## define rule to add
  if(type == "exact") {
    
    rule_add = list(
      group = group)
    
    for(i in 1:populations) {
      
      rule_add[[length(rule_add) + 1]] <- list(type = "exact",
                                               value = fun_dummy)
      
      names(rule_add)[i + 1] <- paste(name, i, sep = "_")
    }

  } else if(type == "rnorm") {
    
    rule_add = list(
      group = group)
    
    for(i in 1:populations) {
      
      rule_add[[length(rule_add) + 1]] <- list(type = "rnorm",
                                               mean = fun_dummy,
                                               sd = fun_dummy)
      
      names(rule_add)[i + 1] <- paste(name, i, sep = "_")
    }
  }  else if(type == "runif") {
    
    rule_add = list(
      group = group)
    
    for(i in 1:populations) {
      
      rule_add[[length(rule_add) + 1]] <- list(type = "runif",
                                               min = fun_dummy,
                                               max = fun_dummy)
      
      names(rule_add)[i + 1] <- paste(name, i, sep = "_")
    }
  }
  
  ## append new rule
  book[[length(book) + 1]] <- rule_add
  
  ## remove first book entry
  book_body <- book
  book_body[[1]] <- NULL
  
  ## add name of new rule
  names(book_body)[length(book_body)] <- name

  ## extract book types in raw format  
  book_groups <- lapply(X = book_body, FUN = function(book_body) {
    
    book_body$group
  })
  
  ## assign book types
  book_groups <- as.character(book_groups)
  
  ## get index of groups
  book_index <- match(x = book_groups, 
                      table = c("general", 
                                "specific"))
  
  ## amalgamate book parts
  book_new <- c(book[[1]], 
                book_body[book_index == 1], 
                book_body[book_index == 2])
  
  ## return output
  return(book_new)
}
