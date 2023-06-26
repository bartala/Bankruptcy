library(sqldf)
library(readr)
library(rentrez)

citations <- read_csv("biotools/citations.csv")
citations$year<-NULL
names(citations) <- c("tool_pmid","cited_by_pmcid")


cited <- read_csv("biotools/citations2.csv", col_types = cols(...1 = col_skip()))
names(cited) <- c("cited_by_pmcid", "cited_by_pubyear")

cited <- cited[!duplicated(cited$cited_by_pmcid),]
cited$cited_by_pubyear <- as.numeric(substr(cited$cited_by_pubyear,0,4))


x<-merge(citations, cited, on = "cited_by_pmcid", all.x = TRUE)
x<-x[!is.na(x$cited_by_pubyear),]

df<-sqldf("select tool_pmid, count(cited_by_pmcid) as num_citations, cited_by_pubyear from x group by tool_pmid, cited_by_pubyear")



# complete missing publication year for tools
tools <- read_csv("biotools/tools_db.csv")
tools[is.na(tools$Year),'Year'] <- as.numeric(substr(tools[is.na(tools$Year),]$Article_Date,7,10))
tools$tool_pubyear <- tools$Year

tools$tool_pmid <- gsub("\\[|\\]", "", tools$PMID)
              


# merge
# df2 <- merge(df, tools[,c('tool_pmid','tool_pubyear')], on='tool_pmid', all.x  = TRUE)
# df2$Year<-NULL
# 
# # complete missing tool_pubyear data
# i=1
# for(pmid in unique(df[is.na(df2$tool_pubyear),]$tool_pmid)){
#   
#   print(i)
#   i=i+1
#   summary <- entrez_summary(db = "pubmed", id = pmid, email = 'yaelshvili@gmail.com')
#   
#   # Extract the publication year from the summary
#   df2[df2$tool_pmid == pmid,'tool_pubyear']<-substr(summary$pubdate,0,4)
#   
# }


df2$relative_age <- as.numeric(df2$cited_by_pubyear) - as.numeric(df2$tool_pubyear) -1

# remove impossible case of papers citing an original paper before it was published
df2<-df2[df2$relative_age>0,]
df2<-df2[!is.na(df2$tool_pmid),]




#------------ load tools -------------------

tools_2000_2019_2 <- data.frame(read_csv("biotools/drive-download-20230625T123856Z-001/tools_2000_2019_2.csv"))
tools_2000_2019_3 <- data.frame(read_csv("biotools/drive-download-20230625T123856Z-001/tools_2000_2019_3.csv"))

tools_2000_2019_2<-tools_2000_2019_2[,c(-1,-2)]
tools_2000_2019_3<-tools_2000_2019_3[,c(-1,-2,-3, -63)]

tools_DB <- rbind(tools_2000_2019_2,tools_2000_2019_3)
tools_DB <- tools_DB[!duplicated(tools_DB$meta.PMID),]

table(tools_DB$url_status)



tools_DB_url <- merge(tools_DB, df2, by.y = 'tool_pmid', by.x ="meta.PMID")


# ------ plot in Fig.2 ----------------------------------------------------------------------
x_good <- sqldf("select relative_age, avg(num_citations) as avg from tools_DB_url where url_status < 400 or url_status > 900 group by relative_age")
x_bad <- sqldf("select relative_age, avg(num_citations) as avg from tools_DB_url where url_status >= 400 and  url_status < 900  group by relative_age")


ggplot(x_good, aes(x = relative_age, y = avg)) +
  geom_point(shape = 1, colour = "black", fill = "black", size = 2, stroke = 2) +
  geom_smooth(se = FALSE, formula = y ~ x, method = 'loess', size = 2) +
  geom_vline(xintercept = 14.5, linetype = 2, size = 1, colour = "darkgray") +
  ggtitle("Average citation time series of all biomedical tools papers") +
  ylab("Average tool citation") +
  xlab("Years relative to publication date") +
  theme_bw() +
  geom_smooth(data = x_bad, aes(x = relative_age, y =avg), color = "red",size = 2,  method = 'loess', se = FALSE,formula = y ~ x) +
  geom_point(data = x_bad, aes(x = relative_age, y = avg), color = "red", size = 2, shape = 1, stroke = 2)


