#' Create a virtual sample.
#'
#' The function generates many virtual sediment grains based on the specified
#' sample geometry and depth, using the information from a rule book.
#'
#'
#' @param book \code{list} object, RuleBook to be used.
#' @param depth \code{Numeric} scalar, depth of the sample center (m).
#' @param geometry \code{Character} scalar, keyword defining the geometry of
#'        the sample. One out of \code{"cuboid"} and \code{"cylinder"},
#'        default is \code{"cuboid"}.
#' @param radius \code{Numeric} scalar, radius of the cylinder (m).
#' @param height \code{Numeric} scalar, height of the cuboid (m).
#' @param width \code{Numeric} scalar, width of the cuboid (m).
#' @param length \code{Numeric} scalar, length of the cuboid or cylinder (m).
#' @return A list object.
#' @author Michael Dietze
#' @examples
#'
#' ## To be done
#'
#' @export make_Sample
make_Sample <- function(
  book,
  depth,
  geometry = "cuboid",
  radius,
  height,
  width,
  length
) {

  ## check input parameters ---------------------------------------------------
  ## needs to be programmed, fist.
  output_flag <- TRUE

  ## definition of helper functions -------------------------------------------

  ## function to assign grain population ID
  get_population <- function(x) {

    ## extract arguments
    a <- x[1:(length(x)/2)]
    b <- x[((length(x)/2) + 1):length(x)]

    ## calculate and return population ID
    return(sample(x = a, size = 1, prob = b))
  }

  ## calculations -------------------------------------------------------------

  ## determine depth range and sample volume based on sample geometry
  if(geometry == "cuboid") {

    ## calculate depth interval (m)
    depth_range <- c(depth - height / 2,
                     depth + height / 2)

    ## calculate sample volume (cubic m)
    V_sample <- height * width * length
  } else if(geometry == "cylinder") {
    
    ## calculate depth interval (m)
    depth_range <- c(depth - radius,
                     depth + radius)
    
    ## calculate sample volume (cubic m)
    V_sample <- pi * radius^2 * length
  }

  ## define number of grains for average estimate
  n_estimate <- 1000

  ## draw random depths for sample size
  z_estimate <- runif(n = n_estimate,
                      min = depth_range[1],
                      max = depth_range[2])

  ## get number of populations
  n_population <- length(book$population) - 1

  ## calculate mean diameter for each population
  grains_raw <- numeric(length = n_population)
  for(i in 1:n_population) {

    grains_raw[i] <- 4 / 3 * pi *
      mean(EMMAgeo::convert.units(
        phi = book$grainsize[[i + 1]]$mean(z_estimate)) / 10^6)^3 *
      mean(book$packing[[i + 1]]$mean(z_estimate)) *
      mean(book$population[[i + 1]]$value(z_estimate))
  }

  ## estimate 110 % of total number of grains for sample volume
  n_grains <- V_sample / sum(grains_raw) * 1.1

  ## create grains as data frame ----------------------------------------------

  ## get grain depths for sample
  if(geometry == "cuboid") {
    
    depth <- runif(n = n_grains,
                   min = depth_range[1],
                   max = depth_range[2])
  } else if(geometry == "cylinder") {
    
    depth_i <- runif(n = n_grains, 
                     min = depth_range[1], 
                     max = depth_range[2]) - depth
    
    depth_circle <- sqrt(radius^2 - depth_i^2)
    depth_weight <- depth_circle/max(depth_circle)
    
    depth <- sample(x = depth_i, 
                    size = n_grains, 
                    replace = TRUE, 
                    prob = depth_weight) + depth
  }

  ## create initial grains data frame
  grains <- data.frame(ID = seq(from = 1,
                                to = n_grains),
                       depth = depth)

  ## create grain population probability vector
  p_z <- matrix(nrow = n_grains,
                ncol = n_population)

  ## assign depth-dependent population probabilities
  for(i in 1:ncol(p_z)) {
    p_z[,i] <- book$population[[i + 1]]$value(x = grains$depth)
  }
  
  ## set negative probabilities to zero
  p_z[p_z < 0] <- 0

  ## normalise probabilities
  p_z <- p_z / rowSums(x = p_z)

  ## create population ID matrix
  pop_ID <- matrix(data = rep(seq(from = 1,
                                  to = n_population),
                              times = n_grains),
                   nrow = n_grains,
                   byrow = TRUE)

  ## create helper function input data set
  X_in <- cbind(pop_ID, p_z)

  ## get number of cores
  n_cores <- parallel::detectCores() - 1

  ## initiate cluster
  cl <- parallel::makeCluster(getOption("mc.cores",
                                        n_cores))

  ## assign grain population ID
  population <- parallel::parApply(cl = cl,
                                   X = X_in,
                                   MARGIN = 1,
                                   FUN = get_population)

  ## stop cluster
  parallel::stopCluster(cl)

  ## create matrix for other parameters
  parameters <- matrix(nrow = n_grains,
                       ncol = length(book) - 1)
  colnames(parameters) <- names(book)[-1]

  ## append population and empty columns to grains data frame
  grains <- cbind(grains,
                  population,
                  parameters)

  ## calculate all further parameters
  for(i in 2:length(book)) {

    ## calculate general parameters
    if(book[[i]]$group == "general") {

      if(book[[i]][[2]]$type == "exact") {

        grains[,i + 2] <- book[[i]][[2]][[2]](x = grains$depth)

      } else if(book[[i]][[2]]$type == "rnorm") {

        sd_pre <- book[[i]][[2]]$sd(x = grains$depth)
        sd_pre[sd_pre < 0] <- 0

        grains[,i + 2] <- rnorm(n = nrow(grains),
                                mean = book[[i]][[2]]$mean(x = grains$depth),
                                sd = sd_pre)
      }  else if(book[[i]][[2]]$type == "runif") {
        
        min_pre <- book[[i]][[2]]$min(x = grains$depth)
        max_pre <- book[[i]][[2]]$max(x = grains$depth)
        
        grains[,i + 2] <- runif(n = nrow(grains),
                                min = min_pre,
                                max = max_pre)
      }

    } else {

      ## calculate specific parameters
      for(j in 1:n_population) {

        ## extract ID of grains of appropriate population
        ID_population_j <- seq(from = 1,
                               to = nrow(grains))[grains$population == j]


        if(book[[i]][[2]]$type == "exact") {

          grains[ID_population_j,i + 2] <-
            book[[i]][[j + 1]]$value(x = grains$depth[ID_population_j])

        } else if(book[[i]][[2]]$type == "rnorm") {

          grains[ID_population_j,i + 2] <-
            rnorm(n = length(ID_population_j),
                  mean = book[[i]][[j + 1]]$mean(x = grains$depth),
                  sd = abs(book[[i]][[j + 1]]$sd(x = grains$depth)))
          
        }  else if(book[[i]][[2]]$type == "runif") {
          
          grains[ID_population_j,i + 2] <-
            runif(n = length(ID_population_j),
                  min = book[[i]][[j + 1]]$min(x = grains$depth),
                  max = book[[i]][[j + 1]]$max(x = grains$depth))
        }
      }
    }
  }

  ## calculate cumulative sample volume
  V_grains <- cumsum(4 / 3 * pi * grains$packing *
                       (EMMAgeo::convert.units(phi = grains$grainsize) /
                          10^6)^3)

  ## remove grains that overtop sample volume
  grains <- grains[V_sample <= V_grains,]

  ## return output ------------------------------------------------------------
  if(output_flag == TRUE) {

    return(grains)
  }
}
