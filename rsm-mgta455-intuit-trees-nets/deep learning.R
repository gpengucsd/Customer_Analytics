library(tidyverse)
library(keras)
library(recipes)
library(ModelMetrics)
library(tidyverse)
library(radiant)
library(zipcode)

intuit75k <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/intuit75k.rds"))
data("zipcode")
intuit75k <- intuit75k %>%
  left_join(zipcode)
intuit75k[is.na(intuit75k)] <- 0
ggplot(data = intuit75k,mapping = aes(x=dollars))+geom_histogram(binwidth = 5, fill = "lightblue", colour = "black")

intuit75k <-mutate_at(intuit75k, .vars = vars(zip_bins,sex,bizflag,version1, owntaxprod, upgraded),.funs = as.factor)%>%
  mutate(state =as.factor(intuit75k$state))%>%
  mutate(res1 =ifelse(res1 == 'No',0,1))%>%
  select(id,zip_bins,sex,bizflag,numords,dollars,last,sincepurch,version1,owntaxprod,upgraded,state,res1,training)

train <- intuit75k %>%
  filter(training == 1)
validation  <- intuit75k %>%
  filter(training == 0)

dataTransRec <- recipe(res1 ~ zip_bins+sex+bizflag+numords+dollars+last+sincepurch+version1+owntaxprod+upgraded+state, data = train) %>%
  step_dummy(zip_bins,sex,bizflag,version1,owntaxprod,upgraded,state,one_hot = T) %>%
  prep(data = train)

trainTransF <- bake(dataTransRec, newdata = train)
testTransF <- bake(dataTransRec, newdata = validation)

Xtrain <- trainTransF %>% select(-res1)
Xtest <- testTransF %>% select(-res1)

dimX <- dim(Xtrain)[2]

Ytrain <- trainTransF %>% select(res1)
Ytest <- testTransF %>% select(res1)

#validation test can be skipped
perform <- function(key){
  dlDF <- data.frame(predDL=predict(model,as.matrix(Xtest)),
                     res1= Ytest$res1)
  validation_new <- data.frame(validation, dlDF)

  intuit75k <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/intuit75k.rds"))
  data("zipcode")
  intuit75k <- intuit75k %>%
    left_join(zipcode)
  intuit75k[is.na(intuit75k)] <- 0
  intuit75k <-mutate_at(intuit75k, .vars = vars(zip_bins,sex,bizflag,version1, owntaxprod, upgraded),.funs = as.factor)%>%
    mutate(state =as.factor(intuit75k$state))%>%
    mutate(res1 =ifelse(res1 == 'No',0,1))%>%
    select(id,zip_bins,sex,bizflag,numords,dollars,last,sincepurch,version1,owntaxprod,upgraded,state,res1,training)


  intuit75k <- intuit75k %>%
    left_join(validation_new)
  intuit75k[is.na(intuit75k)] <- 0


  cost = 1.41
  margin = 60
  Break_even = cost/margin
  # Lift, Gain, Profit, ROME
  result <- evalbin(
    intuit75k,
    pred = key,
    rvar = "res1",
    lev = "1",
    cost = cost,
    margin = margin,
    train = 'Test',
    data_filter = 'training == 1'
  )
  #plot(result, plots = c("lift", "gains", "profit", "rome"), custom = FALSE)

  # Confusion Matrix and profit&ROME
  cm <- confusion(
    intuit75k,
    pred = key,
    rvar = 'res1',
    lev = '1',
    cost = cost,
    margin = margin,
    train = 'Test',
    data_filter = 'training == 1'
  )

  return(cm$dataset[14])
}

lista <-c()
set.seed(1234)
for (i in 1:10){for (j in 1:10){
  model <- keras_model_sequential() %>%
    layer_dense(units=i,activation = "relu", input_shape = dimX) %>%
    layer_dropout(rate = 0.1) %>%
    layer_dense(units=j,activation = "relu") %>%
    layer_dropout(rate = 0.1) %>%
    layer_dense(units=1,activation= 'sigmoid')



  model%>%
    compile(loss='binary_crossentropy',
            optimizer_adam(lr = 1e-04),
            metrics=c('accuracy'))

  historyT <- model %>% fit(
    as.matrix(Xtrain),
    Ytrain$res1,
    epochs = 20,
    batch_size = 64,
    validation_split = 0.2
  )

  historyT <- model %>% fit(
    as.matrix(Xtrain),
    Ytrain$res1,
    epochs = 20,
    batch_size = 64
  )


  ## get test predictions
  profitnn <- perform('predDL')

  lista <- append(lista,profitnn$profit)}}

max <- max(lista)

#best model
set.seed(1235)
model <- keras_model_sequential() %>%
  layer_dense(units=6,activation = "relu", input_shape = dimX) %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units=16,activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units=1,activation= 'sigmoid')


model%>%
  compile(loss='binary_crossentropy',
          optimizer_adam(lr = 1e-04),
          metrics=c('accuracy'))

historyT <- model %>% fit(
  as.matrix(Xtrain),
  Ytrain$res1,
  epochs = 20,
  batch_size = 64,
  validation_split = 0.2
)

historyT <- model %>% fit(
  as.matrix(Xtrain),
  Ytrain$res1,
  epochs = 20,
  batch_size = 64
)

dlDF <- data.frame(predDL=predict(model,as.matrix(Xtest)),
                   res1= Ytest$res1)
validation_new <- data.frame(validation, dlDF)

intuit75k <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/intuit75k.rds"))
data("zipcode")
intuit75k <- intuit75k %>%
  left_join(zipcode)
intuit75k[is.na(intuit75k)] <- 0
intuit75k <-mutate_at(intuit75k, .vars = vars(zip_bins,sex,bizflag,version1, owntaxprod, upgraded),.funs = as.factor)%>%
  mutate(state =as.factor(intuit75k$state))%>%
  mutate(res1 =ifelse(res1 == 'No',0,1))%>%
  select(id,zip_bins,sex,bizflag,numords,dollars,last,sincepurch,version1,owntaxprod,upgraded,state,res1,training)


intuit75k <- intuit75k %>%
  left_join(validation_new)
intuit75k[is.na(intuit75k)] <- 0

intuit75k <- data1
cost = 1.41
margin = 60
Break_even = cost/margin
intuit75k_validation <- intuit75k%>%
  filter(training ==0)%>%
  mutate(mailto = predDL/2.2 > Break_even)
mail_to_rate = mean(intuit75k_validation$mailto)
intuit75k_validation <- intuit75k_validation%>%
  filter(mailto == TRUE)
response_rate <- mean(intuit75k_validation$res1)/2
cost1<- 1.41*763334*mail_to_rate
profit1 <- 60*763334*mail_to_rate*response_rate-cost1
ROME <-profit1/cost1

data1<- read_rds('prediction.rds')



######get prediction



validation_new <- data.frame(validation, dlDF)

intuit75k <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/intuit75k.rds"))
data("zipcode")
intuit75k <- intuit75k %>%
  left_join(zipcode)
intuit75k[is.na(intuit75k)] <- 0
intuit75k <-mutate_at(intuit75k, .vars = vars(zip_bins,sex,bizflag,version1, owntaxprod, upgraded),.funs = as.factor)%>%
  mutate(state =as.factor(intuit75k$state))%>%
  mutate(res1 =ifelse(res1 == 'No',0,1))%>%
  select(id,zip_bins,sex,bizflag,numords,dollars,last,sincepurch,version1,owntaxprod,upgraded,state,res1,training)


intuit75k <- intuit75k %>%
  left_join(validation_new)
intuit75k[is.na(intuit75k)] <- 0


cost = 1.41
margin = 60
Break_even = cost/margin




perform <- function(key){
  dlDF <- data.frame(predDL=predict(model,as.matrix(Xtest)),
                     res1= Ytest$res1)
  validation_new <- data.frame(validation, dlDF)

  intuit75k <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/intuit75k.rds"))
  data("zipcode")
  intuit75k <- intuit75k %>%
    left_join(zipcode)
  intuit75k[is.na(intuit75k)] <- 0
  intuit75k <-mutate_at(intuit75k, .vars = vars(zip_bins,sex,bizflag,version1, owntaxprod, upgraded),.funs = as.factor)%>%
    mutate(state =as.factor(intuit75k$state))%>%
    mutate(res1 =ifelse(res1 == 'No',0,1))%>%
    select(id,zip_bins,sex,bizflag,numords,dollars,last,sincepurch,version1,owntaxprod,upgraded,state,res1,training)


  intuit75k <- intuit75k %>%
    left_join(validation_new)
  intuit75k[is.na(intuit75k)] <- 0


  cost = 1.41
  margin = 60
  Break_even = cost/margin
  # Lift, Gain, Profit, ROME
  result <- evalbin(
    intuit75k,
    pred = key,
    rvar = "res1",
    lev = "1",
    cost = cost,
    margin = margin,
    train = 'Test',
    data_filter = 'training == 1'
  )
  #plot(result, plots = c("lift", "gains", "profit", "rome"), custom = FALSE)

  # Confusion Matrix and profit&ROME
  cm <- confusion(
    intuit75k,
    pred = key,
    rvar = 'res1',
    lev = '1',
    cost = cost,
    margin = margin,
    train = 'Test',
    data_filter = 'training == 1'
  )

  return(cm$dataset[14])
}
profitnn<- perform('predDL')

#best model

saveRDS(intuit75k,file ='prediction.rds')


dlDF <- data.frame(predDL=predict(model,as.matrix(Xtest)),
                   res1= Ytest$res1)
validation_new <- data.frame(validation, dlDF)

intuit75k <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/intuit75k.rds"))
data("zipcode")
intuit75k <- intuit75k %>%
  left_join(zipcode)
intuit75k[is.na(intuit75k)] <- 0
intuit75k <-mutate_at(intuit75k, .vars = vars(zip_bins,sex,bizflag,version1, owntaxprod, upgraded),.funs = as.factor)%>%
  mutate(state =as.factor(intuit75k$state))%>%
  mutate(res1 =ifelse(res1 == 'No',0,1))%>%
  select(id,zip_bins,sex,bizflag,numords,dollars,last,sincepurch,version1,owntaxprod,upgraded,state,res1,training)

test_set <-test_set%>%
  mutate(res1 =ifelse(res1 == 'No',0,1))
intuit75k <- intuit75k %>%
  left_join(test_set)
intuit75k[is.na(intuit75k)] <- 0

for (i in 1:40){
cost = 1.41
margin = 60
Break_even = cost/margin
intuit75k_validation <- intuit75k%>%
  filter(training ==0)%>%
  mutate(mailto = prob_nnk/(1.8+i*0.01) > Break_even)
mail_to_rate = mean(intuit75k_validation$mailto)
intuit75k_validation <- intuit75k_validation%>%
  filter(mailto == TRUE)
response_rate <- mean(intuit75k_validation$res1)/2
cost1<- 1.41*763334*mail_to_rate
profit1 <- 60*763334*mail_to_rate*response_rate-cost1
print(c(i,profit1))}
#so i = 26 reach maximum 455103.8




