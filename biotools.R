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

#------ plot in Fig.2 ----------------------------------------------------------------------
x_good <- sqldf("select relative_age, avg(num_citations) as avg from df where stat = 'Available' group by relative_age")
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
  xlim(0, 19)  # Set the x-axis limits from 0 to 19


