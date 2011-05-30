library('testthat')
library('classify')
context("Does random splits give the correct error rates?")

library('MASS')
library('mlbench')
data('Vehicle')

seed <- 42
N <- nrow(iris)
set.seed(seed)
train <- sample(seq_len(N), 0.5 * N)

train_x <- iris[train, -5]
train_y <- iris[train ,5]
test_x <- iris[-train, -5]
test_y <- iris[-train ,5]

# Performing classification of iris data set using LDA and QDA
lda_out <- lda(x = train_x, grouping = train_y)
lda_pred <- predict(lda_out, test_x)$class
qda_out <- qda(x = train_x, grouping = train_y)
qda_pred <- predict(qda_out, test_x)$class

# Wrapper functions to use LDA and QDA with classify
lda_wrapper <- function(x, y) { lda(x = x, grouping = y) }
qda_wrapper <- function(x, y) { qda(x = x, grouping = y) }
lda_predict_wrapper <- function(object, newdata) { 
  predict(object, newdata)$class
}
qda_predict_wrapper <- function(object, newdata) { 
  predict(object, newdata)$class
}

# TODO: Manually partition iris and vehicle data sets
# TODO: Apply lda/qda to each of the iris and vehicle data sets
# TODO: Compare the results from classify and manually sim'd. Use testthat!

# Set up lists of data sets and classifiers
list_iris <- list(name = "iris", x = iris[, -5], y = iris[, 5])
list_vehicle <- list(name = "vehicle", x = Vehicle[, -19], y = Vehicle[, 19])
list_data <- list(list_iris, list_vehicle)

list_lda <- list(method = "LDA", classifier = "lda_wrapper", predict = "lda_predict_wrapper")
list_qda <- list(method = "QDA", classifier = "qda_wrapper", predict = "qda_predict_wrapper")
list_classifiers <- list(list_lda, list_qda)

out <- classify(
  list_data = list_data, 
  list_classifiers = list_classifiers, 
  error_est = "split",
  num_splits = 20,
  seed = seed)
print(out)
