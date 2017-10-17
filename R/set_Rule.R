#' Set depth-dependent rule for model parameter.
#'
#' The function defines how the specified model parameter varies with depth.
#' The transfer function uses different interpolation functions to create a
#' continuous representation of a parameter value with depth.
#'
#'
#' @param book \code{List} object, rule book to be edited.
#' @param parameter \code{Character} scalar, parameter name to be edited.
#' @param value \code{Numeric} list, specifying the
#'        parameter values at the corresponding depth points. If a parameter
#'        is defined by more than one argument (e.g., mean and standard
#'        deviation), all the relevant arguments must be defined for each
#'        corresponding depth as separate list element.
#' @param depth \code{Numeric} list, specifying the depths used for the
#'        interpolation. All elements must be of the same lengths as the
#'        corresponding data in \code{value}.
#' @param type \code{Character} scalar, interpolation method. One out of
#'        \code{spline}, default is \code{spline}.
#' @return A list object with all created formula objects.
#' @author Michael Dietze
#' @examples
#'
#' ## create simple true age-depth-relationship
#' ## TO BE DONE.
#'
#'
#' @export set_Rule
set_Rule <- function(
book,
parameter,
value,
depth,
type = "spline"
) {

  ## check/adjust input parameters ---------------------------------------------------

  ## set output flag
  output_flag <- TRUE

  ## check data format of interpolation type
  if(sum(type == c("spline")) < 1) {

    warning("Interpolation method unavailable. Spline is used!")
  }

  ## create function ----------------------------------------------------------

  ## extract book content
  book_content <- names(book)

  ## isolate chapter to edit
  book_edit <- book[book_content == parameter]

  ## adjust parameter length
  n_parameters <- length(book_edit[[1]]) - 1

  if(n_parameters != length(value)) {

    book_edit_new <- vector(mode = "list",
                            length = length(value) + 1)

    book_edit_new[[1]] <- book_edit[[1]][[1]]

    for(i in 2:length(book_edit_new)) {

      book_edit_new[[i]] <- book_edit[[1]][[2]]
    }

    names(book_edit_new) <- c(names(book_edit[[1]])[1],
                              paste(names(book_edit),
                                    1:length(value),
                                    sep = "_"))

    book_edit[[1]] <- book_edit_new
  }

  ## infer group name
  group <- book_edit[[1]]$group

 if(group == "general") {

   ## define rule for general parameter type

    ## spline interpolaton
    if(type == "spline") {

      for(i in 1:length(value)) {

        ## update book_edit object
        book_edit[[1]][[2]][[i + 1]] <- splinefun(x = depth[[i]],
                                                  y = value[[i]])
      }
    }

  } else if(group == "specific") {

    ## define rule for specific parameter type
    for(i in 1:length(value)) {

      for(j in 1:length(value[[i]])) {

        ## update book_edit object
        book_edit[[1]][[i + 1]][[j + 1]] <- splinefun(x = depth[[i]][[j]],
                                        y = value[[i]][[j]])
      }
    }
  }


  ## return output ------------------------------------------------------------
  if(output_flag == TRUE) {

    ## update input book
    book[book_content == parameter] <- book_edit

    ## return output
    return(book)
  }
}
