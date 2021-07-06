#' Create a virtual sample.
#'
#' The function generates many virtual sediment grains based on the specified
#' sample geometry and depth, using the information from a rule book.
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
#' @param slice \code{Logical} scalar, option to sample in repeated slices of 
#' 10^6 grains until the required sample size is reached. Useful to avoid 
#' memory issues for large numbers of grains per sample volume.
#' @param force \code{Logcial} scalar, option to override the default 
#' maximum number of 10^7 grains per sample, set to avoid memory problems 
#' of the computer.
#' 
#' @return A list object.
#' @author Michael Dietze
#' @examples
#'
#' ## To be done today
#'
#' @export make_Sample
make_Sample <- function(
  book,
  depth,
  geometry = "cuboid",
  radius,
  height,
  width,
  length,
  slice = TRUE,
  force = FALSE
) {
  
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
  
  ## get test sample descriptions
  v_estimate <- lapply(
    X = z_estimate, 
    FUN = function(z, book) {
      
      ## get population probabilities
      p_z <- lapply(X = book$population[-1], 
                    FUN = function(x, z) {
                      x$value(x = z)
                    }, z)
      
      ## convert list to vector
      p_z <- do.call(c, p_z)
      
      ## account for negative values
      p_z[p_z < 0] <- 0
      
      ## normalise probabilities
      p_z <- p_z / sum(p_z)
      
      ## generate population ID                         
      p_z <- base::sample(x = 1:length(p_z),
                          size = 1,
                          prob = p_z)
      
      ## get grain diameter
      if(book$grainsize[[p_z + 1]]$type == "exact") {
        
        d_z <- book$grainsize[[p_z + 1]][[2]](z)
      } else if(book$grainsize[[p_z + 1]]$type == "normal") {
        
        d_z <- stats::rnorm(n = 1, 
                            mean = book$grainsize[[p_z + 1]]$mean(z), 
                            sd = book$grainsize[[p_z + 1]]$sd(z))
      } else if(book$grainsize[[p_z + 1]]$type == "uniform") {
        
        d_z <- stats::runif(n = 1, 
                            min = book$grainsize[[p_z + 1]]$min(z), 
                            max = book$grainsize[[p_z + 1]]$max(z))
      } else if(book$grainsize[[p_z + 1]]$type == "gamma") {
        
        d_z <- stats::rgamma(n = 1, 
                             shape = book$grainsize[[p_z + 1]]$shape(z), 
                             scale = book$grainsize[[p_z + 1]]$scale(z)) + 
          book$grainsize[[p_z + 1]]$offset(z)
      }
      
      ## get packing density
      if(book$packing[[p_z + 1]]$type == "exact") {
        
        w_z <- book$packing[[p_z + 1]][[2]](z)
      } else if(book$packing[[p_z + 1]]$type == "normal") {
        
        w_z <- stats::rnorm(n = 1, 
                            mean = book$packing[[p_z + 1]]$mean(z), 
                            sd = book$packing[[p_z + 1]]$sd(z))
      } else if(book$packing[[p_z + 1]]$type == "uniform") {
        
        w_z <- stats::runif(n = 1, 
                            min = book$packing[[p_z + 1]]$min(z), 
                            max = book$packing[[p_z + 1]]$max(z))
      }  else if(book$packing[[p_z + 1]]$type == "gamma") {
        
        w_z <- stats::rgamma(n = 1, 
                             shape = book$packing[[p_z + 1]]$shape(z), 
                             scale = book$packing[[p_z + 1]]$scale(z)) + 
          book$packing[[p_z + 1]]$offset(z)
      } 
      
      ## calculate metric grain radius
      r_z <- EMMAgeo::convert.units(phi = d_z) / 2000000
      
      ## calculate packing density-corrected grain volume
      v_z <- 4 / 3 * pi * r_z^3 * 1/w_z
      
      ## return output
      return(v_z)
      
    }, book = book)
  
  ## estimate 110 % of total number of grains for sample volume
  n_grains <- round(V_sample * n_estimate / 
                      sum(do.call(c, v_estimate)) * 1.1)
  
  ## create grains as data frame ----------------------------------------------
  
  ## get grain depths for sample
  if(geometry == "cuboid") {
    
    d_sample <- runif(n = n_grains,
                      min = depth_range[1],
                      max = depth_range[2])
  } else if(geometry == "cylinder") {
    
    depth_i <- runif(n = n_grains,
                     min = depth_range[1],
                     max = depth_range[2]) - depth
    
    depth_circle <- sqrt(radius^2 - depth_i^2)
    depth_weight <- depth_circle/max(depth_circle)
    
    d_sample <- sample(x = depth_i,
                       size = n_grains,
                       replace = TRUE,
                       prob = depth_weight) + depth
  }
  
  ## flag warning for high number of grains
  if(n_grains > 10^7 & force == FALSE) {
    stop(paste0("More than 10^7 grains (", n_grains,
                ") to model. Enable with force = TRUE."))
  }
  
  ## get number of cores
  n_cores <- parallel::detectCores() - 1
  
  ## initiate cluster
  cl <- parallel::makeCluster(getOption("mc.cores",
                                        n_cores))
  
  ## check for slice option
  if(slice == TRUE) {
    
    ## get slice limits in terms of grain size numbers
    i_slice <- c(seq(from = 1, 
                     to = length(d_sample), 
                     by = 10^6), 
                 length(d_sample) + 1)
    
    ## create and fill slice object with grain depths  
    d_sample_slice <- vector(mode = "list", 
                             length = length(i_slice) - 1)
    
    for(i in 1:length(d_sample_slice)) {
      
      d_sample_slice[[i]] <- d_sample[(i_slice[i]):(i_slice[i + 1] - 1)]
    }
  } else {
    
    ## alternatively create one big slice
    d_sample_slice <- list(d_sample)
  }
  
  ## get populations
  population <- vector(mode = "list", 
                       length = length(d_sample_slice))
  
  for(i in 1:length(population)) {
    
    population[[i]] <- parallel::parLapply(
      cl = cl, 
      X = d_sample_slice[[i]], 
      fun = function(d_sample, book) {
        
        ## get population probability
        p_z <- lapply(X = book$population[-1], 
                      FUN = function(x, d_sample) {
                        x$value(x = d_sample)
                      }, d_sample)
        
        ## convert list to vector
        p_z <- do.call(c, p_z)
        
        ## account for negative values        
        p_z[p_z < 0] <- 0
        
        ## normalise probabilities
        p_z <- p_z / sum(p_z)
        
        ## generate population ID 
        base::sample(x = 1:length(p_z),
                     size = 1,
                     prob = p_z)
        
      }, book = book)
    
    ## convert list to vector
    population[[i]] <- do.call(c, population[[i]])
  }
  
  ## stop cluster
  parallel::stopCluster(cl)
  
  ## merge populations
  population <- do.call(c, population)
  
  ## create matrix for other parameters
  parameters <- matrix(nrow = n_grains,
                       ncol = length(book) - 1)
  colnames(parameters) <- names(book)[-1]
  
  ## append population and empty columns to grains data frame
  grains <- 1:n_grains
  grains <- as.data.frame(cbind(grains,
                                d_sample,
                                population,
                                parameters))
  
  ## calculate all further parameters
  for(i in 2:length(book)) {
    
    ## calculate general parameters
    if(book[[i]]$group == "general") {
      
      if(book[[i]][[2]]$type == "exact") {
        
        grains[,i + 2] <- book[[i]][[2]][[2]](x = grains$d_sample)
        
      } else if(book[[i]][[2]]$type == "normal") {
        
        sd_pre <- book[[i]][[2]]$sd(x = grains$d_sample)
        sd_pre[sd_pre < 0] <- 0
        
        grains[,i + 2] <- stats::rnorm(n = nrow(grains),
                                       mean = book[[i]][[2]]$mean(
                                         x = grains$d_sample),
                                       sd = sd_pre)
      }  else if(book[[i]][[2]]$type == "uniform") {
        
        min_pre <- book[[i]][[2]]$min(x = grains$d_sample)
        max_pre <- book[[i]][[2]]$max(x = grains$d_sample)
        
        grains[,i + 2] <- stats::runif(n = nrow(grains),
                                       min = min_pre,
                                       max = max_pre)
      } else if(book[[i]][[2]]$type == "gamma") {
        
        shape_pre <- book[[i]][[2]]$min(x = grains$d_sample)
        scale_pre <- book[[i]][[2]]$max(x = grains$d_sample)
        offset_pre <- book[[i]][[2]]$max(x = grains$d_sample)
        
        grains[,i + 2] <- stats::rgamma(n = nrow(grains),
                                        shape = shape_pre,
                                        scale = scale_pre) + offset_pre
      }
      
    } else {
      
      ## calculate specific parameters
      for(j in 1:(length(book$population) - 1)) {
        
        ## extract ID of grains of appropriate population
        ID_population_j <- seq(from = 1,
                               to = nrow(grains))[grains$population == j]
        
        
        if(book[[i]][[2]]$type == "exact") {
          
          grains[ID_population_j,i + 2] <-
            book[[i]][[j + 1]]$value(x = grains$d_sample[ID_population_j])
          
        } else if(book[[i]][[2]]$type == "normal") {
          
          grains[ID_population_j,i + 2] <-
            stats::rnorm(n = length(ID_population_j),
                         mean = book[[i]][[j + 1]]$mean(x = grains$d_sample),
                         sd = abs(book[[i]][[j + 1]]$sd(x = grains$d_sample)))
          
        } else if(book[[i]][[2]]$type == "uniform") {
          
          grains[ID_population_j,i + 2] <-
            stats::runif(n = length(ID_population_j),
                         min = book[[i]][[j + 1]]$min(x = grains$d_sample),
                         max = book[[i]][[j + 1]]$max(x = grains$d_sample))
          
        } else if(book[[i]][[2]]$type == "gamma") {
          
          grains[ID_population_j,i + 2] <-
            stats::rgamma(
              n = length(ID_population_j),
              shape = book[[i]][[j + 1]]$shape(x = grains$d_sample),
              scale = book[[i]][[j + 1]]$scale(x = grains$d_sample)) +
            book[[i]][[j + 1]]$offset(x = grains$d_sample)
        }
      }
    }
  }
  
  ## calculate cumulative sample volume
  r_grains <- (EMMAgeo::convert.units(phi = grains$grainsize) / (2 * 10^6))
  V_grains <- cumsum(4 / 3 * pi * r_grains^3 * 1/grains$packing)
  
  ## remove grains that overtop sample volume
  grains <- grains[V_sample >= V_grains,]
  
  ## return output ------------------------------------------------------------
  return(grains)
}
