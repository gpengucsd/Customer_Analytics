# install.packages("gbm")
library(gbm)
library(radiant.data)

## estimating on the dvd data
dvd_wrk <- readr::read_rds(file.path(find_dropbox(), "MGTA455-2019/data/dvd.rds"))
dvd_wrk$buy <- ifelse(dvd_wrk$buy == "yes", 1, 0)
n.trees <- 1000

result <- gbm(
  buy ~ .,
  data = dvd_wrk,
  distribution = "bernoulli",
  n.trees = n.trees,
  interaction.depth = 2,
  shrinkage = 0.01,
  keep.data = FALSE,
  cv.folds = 5
)

## get variable importance
vimp <- summary(result, plotit = FALSE, n.trees = n.trees)
visualize(vimp, type = "bar", xvar = "var", yvar = "rel.inf")

## estimating on the bbb
bbb_wrk <- readr::read_rds(file.path(find_dropbox(), "MGTA455-2019/data/bbb.rds"))
bbb_wrk <- mutate(
  bbb_wrk,
  buyer = ifelse(buyer == "yes", 1, 0),
  gender = ifelse(gender == "M", 1, 0)
)
n.trees <- 1000

result <- gbm(
  buyer ~ gender + last + total + purch + child + youth + cook + do_it + reference + art + geog,
  data = bbb_wrk,
  distribution = "bernoulli",
  n.trees = n.trees,
  interaction.depth = 2,
  shrinkage = 0.01,
  keep.data = FALSE,
  cv.folds = 5
)

## get variable importance
vimp <- summary(result, plotit = FALSE, n.trees = n.trees)
visualize(vimp, type = "bar", xvar = "var", yvar = "rel.inf")

