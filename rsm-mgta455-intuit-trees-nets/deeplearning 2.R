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

intuit75k <-mutate_at(intuit75k, .vars = vars(zip_bins,sex,bizflag,sincepurch,last,numords,version1, owntaxprod, upgraded),.funs = as.factor)%>%
  mutate(state =as.factor(intuit75k$state))%>%
  mutate(res1 =ifelse(res1 == 'No',0,1))%>%
  select(id,zip_bins,sex,bizflag,numords,dollars,last,sincepurch,version1,owntaxprod,upgraded,state,res1,training)%>%
  mutate(dollars = cut(dollars,
                       breaks=c(0,50,100,150,200,250,300,1200),
                       include.lowest=T,
                       labels = c('<= 50','50-100','100-150','150-200','200-250','250-300','>300')))

train_new <- intuit75k %>%
  filter(training == 1)
validation  <- intuit75k %>%
  filter(training == 0)

validation_id <- data.frame(id = validation$id)
for(i in 1:10){
train <-sample_n(train_new,52500,replace= TRUE)



dataTransRec <- recipe(res1 ~ zip_bins+sex+bizflag+numords+dollars+last+sincepurch+version1+owntaxprod+upgraded+state, data = train) %>%
  step_dummy(zip_bins,sex,bizflag,numords,dollars,last,sincepurch,version1,owntaxprod,upgraded,state,one_hot = T) %>%
  prep(data = train)

trainTransF <- bake(dataTransRec, newdata = train)
testTransF <- bake(dataTransRec, newdata = validation)

Xtrain <- trainTransF %>% select(-res1)
Xtest <- testTransF %>% select(-res1)

dimX <- dim(Xtrain)[2]

Ytrain <- trainTransF %>% select(res1)
Ytest <- testTransF %>% select(res1)

model <- keras_model_sequential() %>%
  layer_dense(units=128,activation = "relu", input_shape = dimX) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units=64,activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units=32,activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units=1,activation= 'sigmoid')


model%>%
  compile(loss='binary_crossentropy',
          optimizer_adam(lr = 1e-04),
          metrics=c('accuracy'))

historyT <- model %>% fit(
  as.matrix(Xtrain),
  Ytrain$res1,
  epochs = 20,
  batch_size = 128,
)

dlDF <- data.frame(predDL=predict(model,as.matrix(Xtest)),
                   res1= Ytest$res1)
validation_id <- cbind(validation_id, dlDF)
print(i)}

saveRDS(validation_id,file ='test.rds' )
validation2<- validation_id[,c(1,1:10*2)]
quan_list <-c()
quan_list1 <- validation2 %>%
  select(-id)%>%
  apply(1,mean)
for (i in 1 : nrow(validation2)){
  quan_list <- append(quan_list,quantile(validation2[i,],probs = c(0.6)))}
nn_result <- data.frame(id = validation2$id,prob_nn2 = as.numeric(quan_list),prob_nn1 =quan_list1)
intuit75k<- left_join(intuit75k,nn_result,by ="id")
intuit75k[is.na(intuit75k)] <- 0

cost = 1.41
margin = 60
Break_even = cost/margin

perform <- function(key){
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

  return(list(result$dataset[,12:13],cm$dataset))
}
average_nn<- perform("prob_nn1")
quan_nn<-perform("prob_nn2")
quan_nn
average_nn
