library(ModelMetrics)
intuit75k <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/intuit75k.rds"))

train <- intuit75k %>%
  filter(training == 1)
validation  <- intuit75k %>%
  filter(training == 0)
breakeven_response_rate <-1.41/60
#map zip into state
library(zipcode)
data("zipcode")
train_zip<- train %>%
  left_join(zipcode,by = 'zip')
train_zip[is.na(train_zip)] <- 0
validation_zip <-validation %>%
  left_join(zipcode,by ='zip')
validation_zip[is.na(validation_zip)] <- 0

#write a function to get all the evaluation of a model
perf_cal <-function(model,validation){

  validation_nb <- model %>%
    predict(pred_data = validation)
  threshold <- 0.5
  confusion_matrix <- data.frame(pred=validation_nb,fact=validation$res1)%>%
    mutate(Pred.res1 = pred.Yes > threshold)%>%
    count(Pred.res1,fact)
  acc_nb <- (confusion_matrix[2,3]+confusion_matrix[3,3])/sum(confusion_matrix$n)
  validation_mail <- data.frame(pred=validation_nb,fact=validation$res1)%>%
    mutate(mailto_nb = pred.Yes > breakeven_response_rate)%>%
    filter(mailto_nb ==TRUE)
  sample <- validation %>%
    select(res1)%>%
    mutate(res1 = as.numeric(res1=='Yes'))
  offer_send <- nrow(validation_mail)
  offer_upgate <- validation_mail %>%
    filter(fact =='Yes')%>%
    nrow()
  profit <-60*offer_upgate-1.41*offer_send
  ROME <- profit/(1.41*offer_send)
  auc1<- auc(sample$res1,validation_nb$Yes)

  report1 <- data.frame(pred=validation_nb,fact=validation$res1)%>%
    mutate(fact = factor(x= fact ,level= c('Yes','No'),label= c(1,0)))%>%
    mutate(prob_dec = radiant.data::xtile(pred.Yes, 10,rev=TRUE)/10)%>%
    group_by(prob_dec)%>%
    summarise(response_rate = mean(as.numeric(as.character(fact))),number_of_customer = n(),number_of_buyer = sum(as.numeric(as.character(fact))))%>%
    mutate(Lift = response_rate / (sum(number_of_buyer)/sum(number_of_customer)))%>%
    mutate(cumulative_response_rate = cumsum(number_of_buyer)/cumsum(number_of_customer))%>%
    mutate(Cum_Lift = cumulative_response_rate / (sum(number_of_buyer)/sum(number_of_customer)))
  report2 <- report1 %>%
    mutate(cum_buyer = cumsum(number_of_buyer))%>%
    mutate(Gains = number_of_buyer /sum(number_of_buyer))%>%
    mutate(Cum_gains = cum_buyer/sum(number_of_buyer))
  plot3 <- report1%>%
    ggplot(aes(x=prob_dec,y=Cum_Lift))+geom_point()+geom_line()
  reference<- data.frame(x= 1:10/10,y=1:10/10)
  plot4 <- report2%>%
    ggplot(aes(x=prob_dec,y=Cum_gains))+geom_point()+geom_line()+geom_line(aes(x=reference$x,y=reference$y))
  result <- data.frame(Acc = unname(acc_nb),profit = profit,Rome =ROME,auc=auc1)
  output <- list(result = result,plot_lift = plot3,plot_gain = plot4)
  return(output)
}
result_nb <- nb(train, rvar = "res1", evar =
                  c("zip_bins","sex","bizflag","numords","dollars","last","sincepurch","version1","owntaxprod","upgraded" ))
evaluation_nb_basic <- perf_cal(result_nb,validation)

result_nb2 <- nb(train_zip, rvar = "res1", evar =
                   c('state',"sex","bizflag","numords","dollars","last","sincepurch","version1","owntaxprod","upgraded" ))
evaluation_nb2 <- perf_cal(result_nb2,validation_zip)
evaluation_nb2
