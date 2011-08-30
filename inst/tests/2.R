library('testthat')
library('classify')
context("Variable Selection - ANOVA Method")
  
sep_length_aov <- summary(aov(Sepal.Length ~ Species, data = iris))[[1]]$F[1]
sep_width_aov <- summary(aov(Sepal.Width ~ Species, data = iris))[[1]]$F[1]
pet_length_aov <- summary(aov(Petal.Length ~ Species, data = iris))[[1]]$F[1]
pet_width_aov <- summary(aov(Petal.Width ~ Species, data = iris))[[1]]$F[1]

F_out <- cbind(sep_length_aov, sep_width_aov, pet_length_aov, pet_width_aov)
F_out <- cbind.data.frame(
  iris_vars = colnames(F_out),
  F_vals = as.vector(F_out[1,]),
  rank = order(F_out, decreasing = TRUE)
)

test_that("The iris data set has 1 variable selected properly.", { 
  q <- 1
  expect_equal(
    var_sel_anova(iris[,-5], iris[,5], q = q)$kept,
    which(F_out$rank %in% seq_len(q))
  )

  expect_equal(
    var_sel(iris[,-5], iris[,5], vs_method = "anova", q = q)$kept,
    which(F_out$rank %in% seq_len(q))
  )

  expect_equal(
    var_sel_anova(iris[,-5], iris[,5], q = q)$F_stats,
    F_out$F_vals
  )

  expect_equal(
    var_sel(iris[,-5], iris[,5], vs_method = "anova", q = q)$F_stats,
    F_out$F_vals
  )
})

test_that("The iris data set has 2 variables selected properly.", { 
  q <- 2
  expect_equal(
    var_sel_anova(iris[,-5], iris[,5], q = q)$kept,
    which(F_out$rank %in% seq_len(q))
  )
  expect_equal(
    var_sel_anova(iris[,-5], iris[,5], q = q)$F_stats,
    F_out$F_vals
  )
})

test_that("The iris data set has 3 variables selected properly.", { 
  q <- 3
  expect_equal(
    var_sel_anova(iris[,-5], iris[,5], q = q)$kept,
    which(F_out$rank %in% seq_len(q))
  )
  expect_equal(
    var_sel_anova(iris[,-5], iris[,5], q = q)$F_stats,
    F_out$F_vals
  )
})

test_that("The iris data set has 4 variables selected properly.", { 
  q <- 4
  expect_equal(
    var_sel_anova(iris[,-5], iris[,5], q = q)$kept,
    which(F_out$rank %in% seq_len(q))
  )
  expect_equal(
    var_sel_anova(iris[,-5], iris[,5], q = q)$F_stats,
    F_out$F_vals
  )
})

test_that("The ANOVA method throws errors when q > p.", {
	# p <- 4
  expect_error(var_sel_anova(iris[,-5], iris[,5], q = 5))
	expect_error(var_sel_anova(iris[,-5], iris[,5], q = 10))
	expect_error(var_sel_anova(iris[,-5], iris[,5], q = 50))
	expect_error(var_sel_anova(iris[,-5], iris[,5], q = 200))
})

test_that("The ANOVA method throws errors when q <= 0.", {
  expect_error(var_sel_anova(iris[,-5], iris[,5], q = 0))
	expect_error(var_sel_anova(iris[,-5], iris[,5], q = -1))
	expect_error(var_sel_anova(iris[,-5], iris[,5], q = -5))
	expect_error(var_sel_anova(iris[,-5], iris[,5], q = -30))
})
