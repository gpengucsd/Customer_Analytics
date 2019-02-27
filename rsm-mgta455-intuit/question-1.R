radiant.model::logistic(
  intuit75k,
  rvar = "res1",
  evar = c("numords", "version1", "upgraded"),
  lev = "Yes",
  data_filter = "training == 1"
) %>% summary()
