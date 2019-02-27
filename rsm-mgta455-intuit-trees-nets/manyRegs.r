library(tm)
library(tidyverse)
library(slam)
library(magrittr)
library(stringr)
library(tidytext)


theReviews  <- bind_rows(
  read_rds('e:/Dropbox/research/bayesNMF/data/trip/raw/aria.rds') %>%
    mutate(hotel='Aria'),
  read_rds('e:/Dropbox/research/bayesNMF/data/trip/raw/venetian.rds') %>%
    mutate(hotel='Venetian'),
  read_rds('e:/Dropbox/research/bayesNMF/data/trip/raw/luxor.rds') %>%
    mutate(hotel='Luxor'),
  read_rds('e:/Dropbox/research/bayesNMF/data/trip/raw/flamingo.rds') %>%
    mutate(hotel='Flamingo'),
  read_rds('e:/Dropbox/research/bayesNMF/data/trip/raw/vdara.rds') %>%
    mutate(hotel='Vdara'),
  read_rds('e:/Dropbox/research/bayesNMF/data/trip/raw/caesers.rds') %>%
    mutate(hotel='Caesars'),
    read_rds('e:/Dropbox/research/bayesNMF/data/trip/raw/palazzo.rds') %>%
    mutate(hotel='Palazzo')
  )


theReviewCounts <- theReviews %>% 
  select(reviewID,reviewText) %>%
  unnest_tokens(word, reviewText) %>%
  count(reviewID) %>%
  left_join(select(theReviews,reviewID,reviewRating,userNumberReviews,hotel),by='reviewID') %>%
  mutate(reviewRating=factor(reviewRating),
         numReviews = factor(ntile(userNumberReviews,10)))


allRegRaw <- theReviewCounts %>%
  group_by(hotel) %>%
  nest() %>%
  mutate(model = map(data, ~lm(log(n) ~ reviewRating+numReviews, data = .)),
         tidied = map(model, tidy))

allReg <- allRegRaw %>%
  unnest(tidied) 


allReg %>%
  filter(term %in% paste0('reviewRating',2:5)) %>%
  ggplot(aes(x=term,y=estimate,group=hotel)) + geom_point() + geom_line(linetype='dotted') + facet_wrap(~hotel)


allReg %>%
  filter(term %in% paste0('numReviews',2:10)) %>%
  mutate(term = factor(term,levels=paste0('numReviews',2:10))) %>%
  ggplot(aes(x=term,y=estimate,group=hotel)) + geom_point() + geom_line(linetype='dotted') + facet_wrap(~hotel) +
  geom_hline(aes(yintercept=0))


