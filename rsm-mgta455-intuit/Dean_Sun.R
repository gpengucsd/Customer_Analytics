library(tidyverse)
library(radiant)

intuit75k <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/intuit75k.rds"))
cost = 1.41
margin = 60
Break_even = cost/margin


perform = function(key){
  # Lift, Gain, Profit, ROME
  result <- evalbin(
    intuit75k,
    pred = key,
    rvar = "res1",
    lev = "Yes",
    cost = cost,
    margin = margin,
    train = 'Test',
    data_filter = 'training == 1'
  )
  #plot(result, plots = c("lift", "gains", "profit", "rome"), custom = FALSE)

  # Confusion Matrix and profit&ROME
  cm = confusion(
    intuit75k,
    pred = key,
    rvar = 'res1',
    lev = 'Yes',
    cost = cost,
    margin = margin,
    train = 'Test',
    data_filter = 'training == 1'
  )

  return(list(result$dataset[,12:13],cm$dataset))

}


# SQ_RFM
intuit75k = intuit75k%>%
  mutate(rec_iq = xtile(last,5))%>%
  group_by(rec_iq)%>%
  mutate(freq_sq = xtile(numords,5,TRUE))%>%
  ungroup()%>%
  group_by(rec_iq, freq_sq)%>%
  mutate(mon_sq = xtile(dollars,5,TRUE))%>%
  ungroup()%>%
  mutate(rfm_sq = paste0(rec_iq,freq_sq,mon_sq))%>%
  select(-c(rec_iq, freq_sq, mon_sq))%>%
  group_by(rfm_sq)%>%
  mutate(prob_rfm = mean(res1=='Yes'))%>%
  ungroup()

perform('prob_rfm')


# change data type
intuit75k <- mutate_at(intuit75k, .vars = vars(zip_bins, bizflag, version1, owntaxprod, upgraded),
                       .funs = as.factor)


# Naive Bayes
Bayes = nb(
  intuit75k,
  rvar = "res1",
  evar = c(
    "zip", "zip_bins", "sex", "bizflag", "numords", "dollars",
    "last", "sincepurch", "version1", "owntaxprod", "upgraded"
  ),
  data_filter = "training == 1"
)

Bayes_pred = predict(Bayes, pred_data = intuit75k)
intuit75k$prob_bayes = Bayes_pred$Yes

perform('prob_bayes')


# Logit
Logit <- logistic(
  intuit75k,
  rvar = "res1",
  evar = c(
    "zip_bins", "numords", "dollars", "last",
    "version1", "owntaxprod", "upgraded"
  ),
  lev = "Yes",
  data_filter = "training==1"
)

Logit_pred = predict(Logit, pred_data = intuit75k)
intuit75k$prob_logit = Logit_pred$Prediction

perform('prob_logit')


# Logit upper
Logit_pred_upper = predict(Logit, pred_data = intuit75k, conf_lev=0.95,se=TRUE)
intuit75k$prob_logit_upper = Logit_pred_upper$`97.5%`
perform('prob_logit_upper')


# change zip code
intuit75k$zip = as.numeric(intuit75k$zip)
intuit75k = intuit75k%>% mutate(zip_bins2 = xtile(zip,50))

Logit2 <- logistic(
  intuit75k,
  rvar = "res1",
  evar = c(
    "zip_bins2", "numords", "dollars", "last",
    "version1", "owntaxprod", "upgraded"
  ),
  lev = "Yes",
  data_filter = "training==1"
)

Logit_pred2 = predict(Logit2, pred_data = intuit75k)
intuit75k$prob_logit2 = Logit_pred2$Prediction

perform('prob_logit2')





