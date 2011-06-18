library('testthat')
library('classify')
context("Does random splits give the correct error rates?")

library('MASS')
data('Vehicle')

# The following two functions partition the iris and vehicle data sets
# into a training and test set.
# The pct determines the amount of data in the training data set.
# We will check their results with those of classify() to verify
# that classify's results are correct.
iris_partition <- function(pct) {
  N <- nrow(iris)
  train <- sample(seq_len(N), pct * N)
  train_x <- iris[train, -5]
  train_y <- iris[train, 5]
  test_x <- iris[-train, -5]
  test_y <- iris[-train, 5]

  # Performing classification of iris data set with LDA and QDA
  lda_out <- lda(x = train_x, grouping = train_y)
  lda_error <- mean(predict(lda_out, test_x)$class != test_y)
  qda_out <- qda(x = train_x, grouping = train_y)
  qda_error <- mean(predict(qda_out, test_x)$class != test_y)
  c(lda_error, qda_error)
}

vehicle_partition <- function(pct) {
  N <- nrow(Vehicle)
  train <- sample(seq_len(N), pct * N)
  train_x <- Vehicle[train, -19]
  train_y <- Vehicle[train, 19]
  test_x <- Vehicle[-train, -19]
  test_y <- Vehicle[-train, 19]

  # Performing classification of Vehicle data set with QDA
  lda_out <- lda(x = train_x, grouping = train_y)
  lda_error <- mean(predict(lda_out, test_x)$class != test_y)
  qda_out <- qda(x = train_x, grouping = train_y)
  qda_error <- mean(predict(qda_out, test_x)$class != test_y)
  c(lda_error, qda_error)
}

# Setup for classify
# Wrapper functions to use LDA and QDA with classify
lda_wrapper <- function(x, y) { lda(x = x, grouping = y) }
qda_wrapper <- function(x, y) { qda(x = x, grouping = y) }
lda_predict_wrapper <- function(object, newdata) { 
  predict(object, newdata)$class
}
qda_predict_wrapper <- function(object, newdata) { 
  predict(object, newdata)$class
}
# Set up lists of data sets and classifiers
list_iris <- list(name = "iris", x = iris[, -5], y = iris[, 5])
list_vehicle <- list(name = "vehicle", x = Vehicle[, -19], y = Vehicle[, 19])
list_data <- list(list_iris, list_vehicle)

list_lda <- list(method = "LDA", classifier = "lda_wrapper", predict = "lda_predict_wrapper")
list_qda <- list(method = "QDA", classifier = "qda_wrapper", predict = "qda_predict_wrapper")
list_classifiers <- list(list_lda, list_qda)

test_that("Test error rates are the same with 50% training/test split", { 
  seed <- 42
  split_pct <- 0.5
  num_splits <- 20

  classify_out <- classify(
    list_data = list_data, 
    list_classifiers = list_classifiers, 
    error_est = "split",
    split_pct = split_pct,
    num_splits = num_splits,
    seed = seed)

  set.seed(seed)
  iris_errors <- replicate(num_splits, iris_partition(split_pct))
  vehicle_errors <- replicate(num_splits, vehicle_partition(split_pct))

  cl_lda_iris <- subset(classify_out$results, data_set == "iris" & method == "LDA")$error_rate
  cl_qda_iris <- subset(classify_out$results, data_set == "iris" & method == "QDA")$error_rate
  cl_lda_vehicle <- subset(classify_out$results, data_set == "vehicle" & method == "LDA")$error_rate
  cl_qda_vehicle <- subset(classify_out$results, data_set == "vehicle" & method == "QDA")$error_rate

  expect_equal(cl_lda_iris, iris_errors[1,])
  expect_equal(cl_qda_iris, iris_errors[2,])
  expect_equal(cl_lda_vehicle, vehicle_errors[1,])
  expect_equal(cl_qda_vehicle, vehicle_errors[2,])
})

test_that("Test error rates are the same with 75% training/test split", { 
  seed <- 42
  split_pct <- 0.75
  num_splits <- 20

  classify_out <- classify(
    list_data = list_data, 
    list_classifiers = list_classifiers, 
    error_est = "split",
    split_pct = split_pct,
    num_splits = num_splits,
    seed = seed)

  set.seed(seed)
  iris_errors <- replicate(num_splits, iris_partition(split_pct))
  vehicle_errors <- replicate(num_splits, vehicle_partition(split_pct))

  cl_lda_iris <- subset(classify_out$results, data_set == "iris" & method == "LDA")$error_rate
  cl_qda_iris <- subset(classify_out$results, data_set == "iris" & method == "QDA")$error_rate
  cl_lda_vehicle <- subset(classify_out$results, data_set == "vehicle" & method == "LDA")$error_rate
  cl_qda_vehicle <- subset(classify_out$results, data_set == "vehicle" & method == "QDA")$error_rate

  expect_equal(cl_lda_iris, iris_errors[1,])
  expect_equal(cl_qda_iris, iris_errors[2,])
  expect_equal(cl_lda_vehicle, vehicle_errors[1,])
  expect_equal(cl_qda_vehicle, vehicle_errors[2,])
})

test_that("Test error rates are the same with 90% training/test split", { 
  seed <- 42
  split_pct <- 0.90
  num_splits <- 20

  classify_out <- classify(
    list_data = list_data, 
    list_classifiers = list_classifiers, 
    error_est = "split",
    split_pct = split_pct,
    num_splits = num_splits,
    seed = seed)

  set.seed(seed)
  iris_errors <- replicate(num_splits, iris_partition(split_pct))
  vehicle_errors <- replicate(num_splits, vehicle_partition(split_pct))

  cl_lda_iris <- subset(classify_out$results, data_set == "iris" & method == "LDA")$error_rate
  cl_qda_iris <- subset(classify_out$results, data_set == "iris" & method == "QDA")$error_rate
  cl_lda_vehicle <- subset(classify_out$results, data_set == "vehicle" & method == "LDA")$error_rate
  cl_qda_vehicle <- subset(classify_out$results, data_set == "vehicle" & method == "QDA")$error_rate

  expect_equal(cl_lda_iris, iris_errors[1,])
  expect_equal(cl_qda_iris, iris_errors[2,])
  expect_equal(cl_lda_vehicle, vehicle_errors[1,])
  expect_equal(cl_qda_vehicle, vehicle_errors[2,])
})
