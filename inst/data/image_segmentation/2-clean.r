image_segmentation <- list()
image_segmentation$train <- read.csv("segmentation.data",skip=4, header = F)
image_segmentation$test <- read.csv("segmentation.test", skip=4, header = F)
# TODO: Add names
names(image_segmentation$train) <- names(image_segmentation$test) <-
  c("class", "region-centroid-col", "region-centroid-row",
  "region-pixel-count", "short-line-density-5", "short-line-density-2",
  "vedge-mean", "vedge-sd", "hedge-mean", "hedge-sd", "intensity-mean",
  "rawred-mean", "rawblue-mean", "rawgreen-mean", "exred-mean",
  "exblue-mean", "exgreed-mean", "value-mean", "saturation-mean",
  "hue-mean")
image_segmentation$train <- list(
  x = image_segmentation$train[,-1],
  y = factor(image_segmentation$train[,1])
)
image_segmentation$test <- list(
  x = image_segmentation$test[,-1],
  y = factor(image_segmentation$test[,1])
)
save(image_segmentation, file="image_segmentation.RData")
