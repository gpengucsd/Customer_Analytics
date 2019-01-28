## Re-create the BBB data (individual assignment)

## A table like BBB doesn't exist in companies in its raw form
## Someone has to create it first ... likely from different types of sources!
##
## The goal of this assignment is to re-create the tibble in bbb.rds data EXACTLY
## from its components. Follow the steps outlined below:
##
## 1. Determine how to load the different file types (use readr, readxl, and DBI)
## 2. Determine what data transformations are needed and how the data should be
##    combined into a tibble (aka data.frame). Use dplyr 'verbs' as much as possible.
##    tidyr may also be useful. You must name your re-created tibble 'bbb_rec'
## 3. Your work should be completely reproducible (i.e., generate the same results on
##    another computer). Think about the 'paths' you are using to load the data. Will
##    I or the TA have access to those same directories? Of course you cannot 'copy'
##    any data from bbb into bbb_rec. Read this post for some great suggestions
##    https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## 4. The final coding step will be to check that your code produces a tibble
##    identical to the tibble in the bbb.rds file, using the all.equal command
##    shown below. If the test passes, write bbb_rec to "data/bbb_rec.rds". Do
##    NOT change the test as this will be used in grading/evaluation
## 5. Use "Style active file" from the Addins dropdown to ensure your code is decently 
##    formated and readable. See the post linked below for details
##    https://www.tidyverse.org/articles/2017/12/styler-1.0.0/
## 6. When you are done, save your, code and commit and push your work to GitLab
##    using GitGadget or the the Git tab in Rstudio. Of course you can commit and push
##    as often as you like, but only before the due date. Late assignments will not
##    be accepted
## 7. When testing your (final) code make sure to restart the R-process regularly using
##    Session > Restart R. Hint: Learn the keyboard shortcut for your OS to do this quickly
##    Restarting the R process ensures that all packages and variables your code needs
##    are actually available in your code. Do *not* use rm(list = ls()) in your code as it 
##    will break our evaluation code! Again, read the post linked below
##    https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
##  8. Load key libraries or use :: You may use libraries other than the ones shown below as 
##     well but do NOT use libraries that are not part of the rsm-msba  or rsm-msba-spark 
##     docker container by default

library(dplyr)
library(tidyr)
library(lubridate)
library(DBI)

## load the original tibble from bbb.rds on Dropbox so we can compare with your work
## if the data does not load there is something wrong with the Dropbox share
bbb <- readr::read_rds(
  file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/bbb.rds")
)
# head(bbb)

## view the data description of the original data to determine what
## needs to be created (result will pop-up in the Viewer tab in Rstudio)
radiant.data::describe(bbb)

## load demographics data from bbb_demographics.tsv
library(stringr)
demographics <-read.table('data/bbb_demographics.tsv',sep ='\t',header = TRUE)%>%
  mutate(zip=str_pad(zip,5,'left',pad = '0'))
## load nonbook aggregate spending from bbb_nonbook.xlsx
library(readxl)
nonbook <- read_xlsx("data/bbb_nonbook.xlsx")

## load purchase and buy-no-buy information from bbb.sqlite
## hint: what data type is "date" in the database?
## hint: most systems record dates internally as the number 
## of days since some origin. You can use the lubridate::as_date 
## function to convert the number to a date with argument: origin = "1970-01-01"
bbb_new <-DBI::dbConnect(RSQLite::SQLite(),dbname='data/bbb.sqlite')
column <- dbListTables(bbb_new)
table1 <- dbReadTable(bbb_new,'purchase')
table2 <- dbReadTable(bbb_new,'buyer')
table1 <- table1 %>%
  mutate(date_new = lubridate::as_date(date))
## add the zip3 variable
demographics <-demographics %>%
  mutate(zip3 =substr(zip,1,3))%>%
  mutate(acctnum = as.character(acctnum))
## use the following reference date (i.e., "today" for the analysis)
start_date <- lubridate::ymd("2010-3-8")

## call this function to calculate the difference in months between "today" and
## the first (last) date on which a customer purchased a product
library(reshape2)
diff_months <- function(date1, date2) {
  y <- year(date1) - year(date2)
  m <- month(date1) - month(date2)
  return(y * 12 + m)
}
table3 <- table1 %>%
  mutate(difference = abs(diff_months(date_new,start_date)))%>%
  group_by(acctnum)%>%
  summarize(book =sum(price),first= first(difference),last=last(difference))
table4 <- table1 %>%
  group_by(acctnum,purchase)%>%
  summarize(n=n())
table_finish <- dcast(data=table4,acctnum~purchase,fill=0) %>%
  mutate(purch = art+child+cook+do_it+geog+reference+youth)
## generate the required dplyr code below for `first`, `last`, `book`, `purch`,
## and the purchase frequencies 
## alternatively, you could use tidyr::spread to calculate the purchase frequencies
## hint: check the help for ?dplyr::first and ?dplyr::last

## combine the different tibbles you loaded and `mutated` 

bbb_rec <- right_join(table3,table_finish,by=c('acctnum','acctnum'))%>%
  right_join(demographics,by=c('acctnum','acctnum'))%>%
  right_join(table2,by=c('acctnum','acctnum'))%>%
  right_join(nonbook,by=c('acctnum','acctnum'))%>%
  mutate(total =book+ nonbook,buyer = factor(x=buyer,levels =c('yes','no'),labels = c('yes','no')) ,geog = as.integer(geog),art = as.integer(art))%>%
  mutate(reference = as.integer(reference),do_it = as.integer(do_it),cook = as.integer(cook),youth = as.integer(youth))%>%
  mutate(child = as.integer(child),purch= as.integer(purch),total=as.integer(total),nonbook=as.integer(nonbook),book=as.integer(book))%>%
  mutate(last = as.integer(last),first = as.integer(first))

bbb_rec <- bbb_rec[,c(1,13,14,15,16,3,4,2,19,20,12,6,11,7,8,10,5,9,17,18)]
attr(bbb_rec,'description')<-attr(bbb,'description')
## check if the columns in bbb and bbb_rec are of the same type
## fix as needed
cbind(
  bbb = purrr::map_chr(bbb, class),
  bbb_rec = purrr::map_chr(bbb_rec, class)
)

#############################################
## DO NOT EDIT CODE BELOW THIS LINE
## YOUR CODE MUST PASS BOTH test1 AND test2
#############################################
test1 <- all_equal(bbb_rec, bbb)
test2 <- all_equal(attr(bbb_rec, "description"), attr(bbb, "description"))
if (isTRUE(test1) && isTRUE(test2)) {
  message("Well done! Both tests passed so you can write bbb_rec to the data/ directory")
  readr::write_rds(bbb_rec, path = "data/bbb_rec.rds")
} else {
  if (!isTRUE(test1)) {
    message(paste0("Test of equality of tibbles (data.frames) failed\n\n", paste0(test1, collapse = "\n"), "\n\nUse str(bbb) and str(bbb_rec) and check for differences\n"))
  }
  if (!isTRUE(test2)) {
    message(paste0("Test of equality of attributes failed\n\n", paste0(test2, collapse = "\n"), "\n\nUse str(bbb) and str(bbb_rec) and check for differences\n"))
  }
}
saveRDS(bbb_rec,file='data/bbb_rec.rds') 
dbDisconnect(bbb_new)
