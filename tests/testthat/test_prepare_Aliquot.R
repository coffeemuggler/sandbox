test_that("full example test", {
  testthat::skip_on_cran()
  local_edition(3)

  ## load example dataset (this type of loading is 
  ## needed otherwhise testthat produces an error)
  data(sample, envir = environment())
  input <- sample
  
  ## crash function
  expect_error(prepare_Aliquot(
    sample = input, 
    diameter = 500), 
    "\\[prepare_Aliquot\\(\\)\\] chosen aliquot diameter too large; exceeding area sum of all grains!")
  

})
  