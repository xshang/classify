library('testthat')
library('classify')
context("Variable Selection - BW Method from Dudoit et al. (2002)")
  
test_that("The iris data set has 1 variable selected properly.", { 
  p <- 5
	nk <- 10
	x1 <- replicate(p, rnorm(nk))
	x2 <- replicate(p, rnorm(nk, 4, 2))
	x <- rbind(x1, x2)
	y <- gl(2,nk)
	
	expect_error(var_sel_BW(x, y))
	expect_error(var_sel_BW(x, y, q = 0))
	expect_error(var_sel_BW(x, y, q = -1))
	var_sel_BW(x, y, q = 2)
})
