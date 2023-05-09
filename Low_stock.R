library(ggplot2)
library(sqldf)
library(readr)

# find the date when the stock price crashed in terms of ( `Adj_Close(t)` )/( `Adj_Close(t-1)` ) - 1

PTH = 'Desktop/'

stocks_no_bankruptcy <- read_csv(paste0("stocks_no_bankruptcy.csv"))
stocks_no_bankruptcy <- data.frame(stocks_no_bankruptcy)

res <- sqldf("select min(`Adj_div`),ticker,shifted_date from stocks_no_bankruptcy group by ticker")

names(res)<-c("MIN_Adj_div","ticker","event_date")

write.csv(res, file=paste0(PTH,'stock_crash.csv'),row.names = FALSE)
