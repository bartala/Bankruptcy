# combine collected datasets of before_covid, covid, and 2012 to 2022 into a single dataset

library(readr)

#--- 2012 to 2022 that were not collected in the two other datasets
after_all_2020_new <- read_csv("Desktop/bankruptcy/after_all_2020_new.csv")
after_all_2020_new$data <- 'after_all_2020_new'
after_all_2020_new$dataset <- 'all_2020_new'

before_all_2020_new <- read_csv("Desktop/bankruptcy/before_all_2020_new.csv")
before_all_2020_new$data <- 'before_all_2020_new'
before_all_2020_new$dataset <- 'all_2020_new'

all_2020_new <- rbind(before_all_2020_new, after_all_2020_new)
rm(after_all_2020_new,before_all_2020_new)
all_2020_new$withheld<-NULL
all_2020_new$edit_history_tweet_ids<-NULL

#--- before covid 
before_covid_after <- read_csv("Desktop/bankruptcy/before_covid_after.csv")
before_covid_after$data<-'before_covid_after'
before_covid_after$dataset<-'before_covid'

before_covid_before <- read_csv("Desktop/bankruptcy/before_covid_before.csv")
before_covid_before$data <- 'before_covid_before'
before_covid_before$dataset<-'before_covid'
before_covid_before$withheld<-NULL

before_covid <- rbind(before_covid_before, before_covid_after)
rm(before_covid_after,before_covid_before)
before_covid$...1<-NULL

#---- covid
covid_after <- read_csv("Desktop/bankruptcy/covid_after.csv")
covid_after$data<-'covid_after'
covid_after$dataset<-'covid'

covid_before <- read_csv("Desktop/bankruptcy/covid_before.csv")
covid_before$data<-'covid_before'
covid_before$dataset<-'covid'
covid_before$withheld<-NULL

covid <- rbind(covid_before, covid_after)
rm(covid_before,covid_after)
covid$...1<-NULL


# check if duplicate companies exist
table(c(
  unique(all_2020_new$company),
  unique(covid$company),
  unique(before_covid$company)
))


# order columns
all_2020_new <- all_2020_new[,names(covid)]
before_covid <- before_covid[,names(covid)]

all_2020_new <- data.frame(all_2020_new)
before_covid <- data.frame(before_covid)
covid<-data.frame(covid)

# combine datasets
newdata <- rbind(all_2020_new, before_covid, covid)
rm(all_2020_new, before_covid, covid)

# remove duplicate rows by tweet_id
newdata <- newdata[!duplicated(newdata$id),]

## statistics
unique(newdata$company) # we analyzed 107 companies between 2012 to 2022 that declared chapter 11 bankruptcy

write.csv(newdata, file="/users/alon/desktop/bankruptcy/2012_2022_bankruptcy.csv", row.names = FALSE)

