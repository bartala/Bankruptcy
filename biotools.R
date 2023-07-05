library(sqldf)
library(readr)
library(rentrez)

EMAIL = 'bartala@gmail.com'
PTH = '/home/bartalab/github/bankruptcy/'

##------ collect missing tool_pubyear data -------------------------------------
collect_pubmed_publiction_year <- function(pmids, EMAIL, DB = "pubmed"){
  
    summary <- entrez_summary(db = DB, id = pmids, email = EMAIL)
    return(substr(summary$pubdate,0,4))
    
}


#-------------------- load data ------------------------------------------------

id <- "1PYOHKuxBA8cykJyLlSHkRvz45RVl3_cT" # Google Drive file ID to citations
citations <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))


df<-sqldf("select tool_pmid, count(cited_by_pmcid) as num_citations, cited_by_pubyear from citations group by tool_pmid, cited_by_pubyear")


# complete the publication year of the original (cited) paper
id <- '1Ld6ZedLnxSYO8jvq8ZIA9GcbQEvuB6Dd' # Google Drive file ID to citations
tools_DB <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))

# create updated citation list
df <- merge(df, tools_DB[c('meta.PMID', 'stat', 'meta.Published_On')], by.x = 'tool_pmid', by.y ="meta.PMID", all.x = TRUE)
colnames(df)[colnames(df) == "meta.Published_On"] <- "tool_pubyear"


# create relative age field
df$relative_age <- as.numeric(df$cited_by_pubyear) - as.numeric(df$tool_pubyear)

# keep only cases of papers citing an original paper after it was published
df<-df[df$relative_age>=0,]

#---

for(pmid in unique(df$tool_pmid)){
    q = paste0("select relative_age, avg(num_citations) as avg from df where tool_pmid =", pmid," group by relative_age")
    x <- sqldf(q)
    
    ggplot(x, aes(x = relative_age, y = avg)) +
      geom_point(shape = 1, colour = "black", fill = "black", size = 2, stroke = 2) +
      geom_smooth(se = FALSE, formula = y ~ x, method = 'loess', size = 2) +
      geom_vline(xintercept = 18, linetype = 2, size = 1, colour = "darkgray") +
      ggtitle(paste0("Average citation time series of ",pmid)) +
      ylab("Average tool citation") +
      xlab("Years relative to publication date") +
      theme_bw()
}

#------ plot in Fig.2 ----------------------------------------------------------------------
x_good <- sqldf("select relative_age, sum(num_citations) as count, avg(num_citations) as avg from df where stat = 'Available' group by relative_age having sum(num_citations) > 1")
x_bad <- sqldf("select relative_age, avg(num_citations) as avg from df where stat = 'Unavailable'  group by relative_age")

ggplot(x_good, aes(x = relative_age, y = avg)) +
  geom_point(shape = 1, colour = "black", fill = "black", size = 2, stroke = 2) +
  geom_smooth(se = FALSE, formula = y ~ x, method = 'loess', size = 2) +
  geom_vline(xintercept = 18, linetype = 2, size = 1, colour = "darkgray") +
  ggtitle("Average citation time series of all biomedical tools papers") +
  ylab("Average tool citation") +
  xlab("Years relative to publication date") +
  theme_bw() +
  geom_smooth(data = x_bad, aes(x = relative_age, y =avg), color = "red",size = 2,  method = 'loess', se = FALSE,formula = y ~ x) +
  geom_point(data = x_bad, aes(x = relative_age, y = avg), color = "red", size = 2, shape = 1, stroke = 2) +
  xlim(0, 20)  # Set the x-axis limits from 0 to 19


#-------------- Altmetric ------------------------------------------------------
id <- '1tiwepOhvVXUkh3y5pRjqmBT9F39x_4yI' # Google Drive file ID to citations
altmetric <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
altmetric <- altmetric[!is.na(altmetric$Altmetric_Score),]


# Basic piechart
pi_data <- data.frame(table(tools_DB$url_status))
names(pi_data) <- c("Status","Frequency")

pi_data$Status <- as.numeric(as.character(pi_data$Status))

URL_2xx <- sum(pi_data[pi_data$Status<300, ]$Frequency)
URL_3xx <- sum(pi_data[pi_data$Status>=300 & pi_data$Status<400, ]$Frequency)
URL_4xx <- sum(pi_data[pi_data$Status>=400 & pi_data$Status<500, ]$Frequency)
URL_5xx <- sum(pi_data[pi_data$Status>=500 & pi_data$Status<999, ]$Frequency)
Server_errors <- sum(pi_data[pi_data$Status>=999, ]$Frequency)


pi_data <- data.frame(Status = c('2XX', '3XX', '4XX', '5XX','Server_errors' ),
                      Frequency = c(URL_2xx,URL_3xx, URL_4xx, URL_5xx, Server_errors)
                      )


ggplot(pi_data, aes(x = "", y = Frequency, fill = Status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void()
# You can change the border of each area with the classical parameters:
pie(Prop , labels = c("Gr-A","Gr-B","Gr-C","Gr-D","Gr-E"), border="white", col=myPalette )



tools$age <- 2023 - tools$Publication_Year

plot(tools$URL_status~ tools$age)

