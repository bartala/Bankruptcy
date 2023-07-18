lib_loc <- "/library/dir/"

 library(RSQLite, lib.loc = lib_loc)
 library(proto, lib.loc = lib_loc)
 library(gsubfn, lib.loc = lib_loc)
 library(readr, lib.loc = lib_loc)
 library(ggplot2, lib.loc = lib_loc)
 library(ggpubr, lib.loc = lib_loc)
 library(pROC, lib.loc = lib_loc)
 library(DescTools, lib.loc = lib_loc)
 library(NbClust, lib.loc = lib_loc)
 library(factoextra, lib.loc = lib_loc)
 library(sqldf, lib.loc = lib_loc)
 library(cowplot,lib.loc = lib_loc)
 library(AICcmodavg,lib.loc = lib_loc)
 library(minpack.lm,lib.loc = lib_loc)
 library(gridExtra,lib.loc = lib_loc)
 library(rsq,lib.loc = lib_loc)
 library(caret,lib.loc = lib_loc)
 library(zoo,lib.loc = lib_loc )
 library(broom)
 library(KneeArrower,lib.loc = lib_loc)
 library(googlesheets4,lib.loc = lib_loc)
 library(Ckmeans.1d.dp, lib =lib_loc)


# # before covid data
PTH1 = "/path/to/local/directory/"

"""### Load datasets of tweets posted before and after the announcment"""

data <- read_csv(paste0(PTH1,"2012_2022_bankruptcy2.csv"), show_col_types = FALSE)

metadata <- read_csv('/content/dataset.csv',show_col_types = FALSE)
 
companies <- unique(data[data$dataset %in% c("before_covid", "covid"),]$company)
 
 companies_2 <- unique(data[data$dataset == 'all_2020_new',]$company)
 companies_2 <- companies_2[companies_2 %in% metadata$`Twitter Username`]
 
 companies <- union(companies,companies_2)
 
 rm(companies_2)

 data <- data[data$company %in% companies,]

"""### Descriptive statistics and data cleaning"""

# # nuber of unique users in the posts
paste0("unique users: ", length(unique(data$author_id)) )

# # total number of tweets
print(
     paste0("total number of tweets (before and after the announcement): ",
            nrow(data)
           )
       )
 
 
# # number of companis
 print(paste0("number of companies: ",length(unique(data$company))) )
 
 
# # number of tweets per company
 table(data$company)

 before <- data[grepl( 'before', data$data, fixed = TRUE),]
 
 after <- data[grepl( 'after', data$data, fixed = TRUE),]
 
 
 x_after <- nrow(after)
 x_before <- nrow(before)
 
# # delete messages originated by the companies
 
 after <- after[!after$author_id %in% metadata$UserID,]
 
 before <- before[!before$author_id %in% metadata$UserID,]
 
 
 print( paste0("deleted tweets by companies after: ", x_after - nrow(after) ) )
 print( paste0("deleted tweets by companies before: ", x_before - nrow(before) ) )

"""### Temporal analysis: number of mentions per day and company"""

 before$created_at<-as.character(before$created_at)
 after$created_at<-as.character(after$created_at)
# 
# # keep only day date (no time)
 before$date <- substr(before$created_at,0,10)
 after$date <- substr(after$created_at,0,10)
 
 temp_before <- sqldf("select company, date, count(1) as freq from before group by company, date")
 
 temp_before$when<-'before'
 
 temp_after <-  sqldf("select company, date, count(1) as freq from after group by company, date")
 
 temp_after$when<-'after'
 
 tmp <- rbind(temp_before, temp_after)
 
 tmp <- merge(tmp,metadata, by.x='company', by.y='Twitter Username')
 
 tmp$number <- as.Date(tmp$date) - as.Date(tmp$`Date of Bankruptcy`,format = "%B %d, %Y")  day diff from bankruptcy (in unix timestamp)
 
 tmp <- tmp[(tmp$number) < 31 & (tmp$number) > (-31),]  remove tweets more than 30 pre/post announcement
 
 tmp <- tmp[ tmp$company %in% intersect(tmp[tmp$when == 'before',]$company, tmp[tmp$when == 'after',]$company),]  keep only companies that appear in both the `before` and `after` datasets
 
 tmp <- tmp[tmp$`Bankruptcy Strategy` == 'Chapter 11',]  keep only chapter 11 bankruptcy
 
 names(tmp)[6] <- 'Company Name'
 
# # save temporary file for future analyses
 write.csv(tmp,file='data0.csv', row.names = FALSE)

"""## Average Twitter mention time series of the companies

Plot for all data and companies (before and after announcment)
"""


# # Maximum curvature cutoff

 library(KneeArrower,lib.loc = lib_loc)
 
 Allcompanies <- sqldf("select avg(freq) as avg, number from tmp group by number",method = "name__class")
 
 Allcompanies_post <- Allcompanies[Allcompanies$number>0,]
 
 x = Allcompanies_post$number
 y = Allcompanies_post$avg
 
# # find the point at which the circle tangent to the curve has the smallest radius
# # https://cran.r-project.org/web/packages/KneeArrower/vignettes/Example.html
 cutoff.point <- findCutoff(x, y, method="curvature", 0.5)
 print(cutoff.point)
 
# # plot
 plot(x, y, pch=20, col="gray")
 points(cutoff.point, col="red", cex=3, pch=20)

# # All companies

 p1<-  ggplot(Allcompanies, aes(x=number, y=avg)) +
         geom_point(shape = 1, colour = "black", fill = "black", size = 2, stroke = 2) +
         #scale_x_continuous(trans = "log10") +
         scale_y_continuous(trans = "log10") +
         #geom_line()+
         geom_smooth(se=FALSE, formula = y ~ x, method = 'loess') +
         geom_vline(xintercept = 17, linetype=2, size = 1, colour="darkgray") +
         # pre-announcement mean
         ggtitle("Average mention time series of all companies") +
           ylab("Average company mention [log10]") + xlab("Days relative to bankruptcy announcement")+
         theme_bw()
 
 
 ggsave("mean_curve.pdf", plot = p1, width = 6.67, height = 6.67, units = "in")
 p1

"""## Identify High or Low persistence memory of companies that declared bankruptcy

### Compute the average #tweets before announcement AND average #tweets after the buzz fades out $(t \ge 17)$.
"""

# # mentions(t >= 18) > mean(mentions(t<t0)) --> High persistance
# # mentionds(t > 18) <= mean(mentions(t<t0)) --> Low persistance

# # average number of tweets before announcement
df2 <- sqldf("select company, avg(freq) as avg_freq from tmp where number < 0 group by company") # before
 df2$When = "Before"
 
# # average number of tweets after the buzz
 df1 <- sqldf("select company, max(freq) as avg_freq from tmp where number > 17 group by company") # after
 df1$When = "After"
 
 df_1 <- rbind(df1,df2)


 sqldf("select company, sum(freq) as sm, number from tmp where number == -25 group by company, number",method = "name__class")


 (tmp[tmp$`Company Name` == 'Cumulus Media' & tmp$number == -24 ,'freq'])

 df_2 <- merge(df2,df1, by = 'company', all.x = TRUE)
 
 df_2$When.x <- NULL
 df_2$When.y <- NULL
 
 names(df_2) <- c("Company", "Before", "After")
 
 df_2[is.na(df_2$After), 'After'] <- 0

"""### High persistence memory of companies"""

# # Companies with more mentions (tweets) after the announcement than before the announcment
# # (i.e., they were not forgotten by the public --> High persistence)
 
 unforgotten_comp <- df_1[df_1$company %in% (df_2[df_2$After >= df_2$Before,'Company']),]
 print(unique(unforgotten_comp$company))
 
 df <- merge(unforgotten_comp[unforgotten_comp$When == 'After', ], unforgotten_comp[unforgotten_comp$When == 'Before', ], by = 'company')
 df$diff <- df$avg_freq.x - df$avg_freq.y
 df1 <- df[,c(1,2,3,6)]
 df2 <- df[,c(1,4,5,6)]
 
 names(df1)<-c("company","avg_freq","When","diff")
 names(df2)<-c("company","avg_freq","When","diff")
 df <- rbind(df1,df2)
 
 df <- df[df$company %in% tmp$company,]
 
 p5<- ggplot(df, aes(x = reorder(company, -diff), y = avg_freq, fill = When)) +
   geom_bar(stat = "identity", position = "dodge") +
   theme_bw() +
   theme(legend.position = c(0.8, 0.8)) +
   ggtitle("Companies with High persistence level of memory ordered by the \n difference between mentions after and before bankruptcy") +
   ylab("Average company mention") +
   xlab("Company's Twitter handle") +
   coord_flip() +
   labs(fill = "") +
   scale_fill_manual(labels = c("After", "Before"), values = c("6699CC", "gray32"))
 
 
 rm(df,df1,df2)
 
 ggsave("SI_High_persistence.pdf", plot = p5, width = 6.67, height = 6.67, units = "in")
 p5

"""
### Low persistence memory of companies
"""


 forgotten_comp <- df_1[df_1$company %in% (df_2[df_2$After < df_2$Before,'Company']),]
 print(unique(forgotten_comp$company))
 
 
 df <- merge(forgotten_comp[forgotten_comp$When == 'After', ], forgotten_comp[forgotten_comp$When == 'Before', ], by = 'company', all.y= TRUE)
 df[is.na(df$avg_freq.x),'avg_freq.x'] <- 0
 df[is.na(df$avg_freq.y),'avg_freq.y'] <- 0
 df$When.x  <- "After"
 
 df$diff <- df$avg_freq.y - df$avg_freq.x
 df1 <- df[,c(1,2,3,6)]
 df2 <- df[,c(1,4,5,6)]
 
 names(df1)<-c("company","avg_freq","When","diff")
 names(df2)<-c("company","avg_freq","When","diff")
 df <- rbind(df1,df2)
 
 
# # bar plot
 p6<- ggplot(df, aes(x = reorder(company, -diff), y = avg_freq, fill = When)) +
               geom_bar(stat = "identity", position = "dodge") +
               theme_bw()+
               theme( legend.position = c(0.8, 0.8)) +
               ggtitle("Companies with Low persistence level of memory ordered by the \n difference between mentions after and before bankruptcy") +
               ylab("Average company mention") +
               xlab("Company's Twitter handle") +
               coord_flip() +
               labs(fill = "") +
               scale_fill_manual(labels = c("After", "Before"),values = c("#6699CC", "gray32"))
 
 
 rm(df,df1,df2)
 
 ggsave("SI_Low_persistence.pdf", plot = p6, width = 6.67, height = 6.67, units = "in")
 p6

"""## Models fitting

### GOF help functions
"""

# # AIC
 gof <- function(model){
 
     return(
               glance(model) %>%
                   dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)
     )
 
 }
 
 
 
# # R^2
 r_sqared <- function(model,y){
                      Calculate R-squared
                     y_pred <- predict(model) # predicted values
 
                     ss_total <- sum((y - mean(y))^2)
                     ss_residual <- sum((y - y_pred)^2)
 
                     r_squared <- (1 - ss_residual/ss_total) # R-squared
                     return(r_squared)
 }

"""### High persistence memory of companies"""

 Unforgotten_companies <- unique(unforgotten_comp$company)
 
 Unforgotten <- tmp[ (tmp$company) %in% (Unforgotten_companies) & tmp$number >=0, ] # number:= days before/after bankruptcy announcement
 
 Unforgotten <- sqldf("select avg(freq) as avg, number from Unforgotten group by number",method = "name__class")


# # models

 Unforgotten$x = Unforgotten$number
 Unforgotten$y = Unforgotten$avg
 
# # Exponential
 expmodel_uf <- lm(y~exp(x), data = Unforgotten)
 print(summary(expmodel_uf))
 
# # Log
 logmodel_uf <- lm(y~log(x+0.01), data = Unforgotten)
 print(summary(logmodel_uf))
 
# # Hyperbolic
 hyper_fit_uf <- nlsLM(y ~ a/(1 + b * x), data = Unforgotten, start = list(a = 1, b = 1))
 print(summary(hyper_fit_uf))
 
# # Biexponential
 biexp_fit_uf <- nlsLM(y ~ a1*exp(-b1*x) + a2*exp(-b2*x), data = Unforgotten, start = c(a1 = 5, b1 = 2, a2 = 3, b2 = 0.5))
 print(summary(biexp_fit_uf))
 
# # Fit a power law curve using nls
 power_fit_uf <- nls(y ~ a * (x+0.0001)^(-b), data = Unforgotten, start = list(a = 1, b = 1))
 summary(power_fit_uf)

# # AIC


 print(paste0("AIC exp: ", gof(expmodel_uf)$AIC))
 print(paste0("AIC log: ",gof(logmodel_uf)$AIC))
 print(paste0("AIC hypr: ",AIC(hyper_fit_uf)))
 print(paste0("AIC biex: ",AIC(biexp_fit_uf)))
 print(paste0("AIC power: ",AIC(power_fit_uf)))

# # clac R^2 for non-linear models

 print(paste0("R^2 exp: ",   round(r_sqared(expmodel_uf,Unforgotten$y),2)))
 print(paste0("R^2 log: ",   round(r_sqared(logmodel_uf,Unforgotten$y),2)))
 print(paste0("R^2 hypr: ",  round(r_sqared(hyper_fit_uf,Unforgotten$y),2)))
 print(paste0("R^2 biex: ",  round(r_sqared(biexp_fit_uf,Unforgotten$y),2)))
 print(paste0("R^2 power: ", round(r_sqared(power_fit_uf,Unforgotten$y),2)))

 p2<-ggplot(Unforgotten, aes(x=number, y=avg)) +
             geom_point(shape = 1, colour = "black", fill = "black", size = 2, stroke = 1) +
             #scale_x_continuous(trans = "log10", limits = c(1, 30)) +
             scale_y_continuous(trans = "log10", limits = c(28, 400)) +
             geom_smooth(method="lm", aes(color="Exponential"), formula= (y ~ exp(x)), se=FALSE, linetype = 1) +
             geom_smooth(method = "glm", aes(color="Log"), formula = y~x, se=FALSE, method.args = list(family = gaussian(link = 'log')),linetype = 1) +
             geom_smooth(method = "nls", aes(color = "Hyperbolic"), formula = y ~ a/(1 + b * x), start = list(a = 1, b = 1), se = FALSE, linetype = 1) +
             geom_smooth(method = "nls", aes(color = "Power-law"), formula = y ~ a * (x+0.0000001)^(-b), se = FALSE, method.args = list(start = list(a = 1, b = 1))) +
             geom_smooth(method = "nls", aes(color = "Biexponential"), formula = y ~ a1*exp(-b1*x) + a2*exp(-b2*x), se = FALSE, method.args = list(start = c(a1 = 5, b1 = 2, a2 = 3, b2 = 0.5))) +
             geom_smooth(method = "nls", aes(color = "Two-phaze"), formula = y ~ a1*exp^(-b1*x) + a2*x^(-b2), se = FALSE, method.args = list(start = c(a1 = 5, b1 = 2, a2 = 3, b2 = 0.5))) +
             ggtitle("Unforgotten Companies (log-log scale)") +
               ylab("Average company mention [log10]") + xlab("Days since the announcement")+
             annotate("text", x=3, y=255,  label= paste0("AIC Biexponential: ",round(AIC(biexp_fit_uf),2),("; R^2 = ") ,round(r_sqared(biexp_fit_uf,Unforgotten$y),2)),hjust = 0)+
             annotate("text", x=3, y=215, label= paste0("AIC Exponential: ", round(gof(expmodel_uf)$AIC,2)),hjust = 0) +
             annotate("text", x=3, y=182, label= paste0("AIC Hyperbolic: ",round(AIC(hyper_fit_uf),2)),hjust = 0)+
             annotate("text", x=3, y=155, label= paste0("AIC Log: ",round(gof(logmodel_uf)$AIC,2)),hjust = 0)+
             annotate("text", x=3, y=130, label= paste0("AIC Power-law: ",round(AIC(power_fit_uf),2)),hjust = 0)+
             theme_bw()
 
 
 ggsave("Unforgotten_models.pdf", plot = p2, width = 6.67, height = 6.67, units = "in")
 p2

"""### Low persistence memory of companies"""

# # Forgotten companies

 forgotten_companies <- unique(forgotten_comp$company)
 
 forgotten <- tmp[ (tmp$company) %in% (forgotten_companies) & tmp$number >=0, ] # number:= number of days since bankruptcy announcement
 
 forgotten <- sqldf("select avg(freq) as avg, number from forgotten group by number",method = "name__class")

# # models

 forgotten$x = forgotten$number
 forgotten$y = forgotten$avg
 
# # Exponential
 expmodel_f <- lm(y~exp(x), data = forgotten)
 print(summary(expmodel_f))
 
# # Log
 logmodel_f <- lm(y~log(x+0.01), data = forgotten)
 print(summary(logmodel_f))
 
# # Hyperbolic
 hyper_fit_f <- nlsLM(y ~ a/(1 + b * x), data = forgotten, start = list(a = 1, b = 1))
 print(summary(hyper_fit_f))
 
# # Biexponential
 biexp_fit_f <- nlsLM(y ~ a1*exp(-b1*x) + a2*exp(-b2*x), data = forgotten, start = c(a1 = 5, b1 = 2, a2 = 3, b2 = 0.5))
 print(summary(biexp_fit_f))
 
# # Fit a power law curve using nls
 power_fit_f <- nls(y ~ a * (x+0.0001)^(-b), data = forgotten, start = list(a = 1, b = 1))
 summary(power_fit_f)


 print(paste0("AIC exp: ", gof(expmodel_f)$AIC))
 print(paste0("AIC log: ",gof(logmodel_f)$AIC))
 print(paste0("AIC hypr: ",AIC(hyper_fit_f)))
 print(paste0("AIC biex: ",AIC(biexp_fit_f)))
 print(paste0("AIC power: ",AIC(power_fit_f)))


 p3<-ggplot(forgotten, aes(x=number, y=avg)) +
             geom_point(shape = 1, colour = "black", fill = "black", size = 2, stroke = 1) +
             #scale_x_continuous(trans = "log10", limits = c(1, 30)) +
             scale_y_continuous(trans = "log10", limits = c(28, 400)) +
             #coord_cartesian(ylim = c(0,500)) +
             geom_smooth(method="lm", aes(color="Exponential"), formula= (y ~ exp(x)), se=FALSE, linetype = 1) +
             geom_smooth(method = "glm", aes(color="Log"), formula = y~x, se=FALSE, method.args = list(family = gaussian(link = 'log')),linetype = 1) +
             geom_smooth(method = "nls", aes(color = "Hyperbolic"), formula = y ~ a/(1 + b * x), start = list(a = 1, b = 1), se = FALSE, linetype = 1) +
             geom_smooth(method = "nls", aes(color = "Power-law"), formula = y ~ a * (x+0.0000001)^(-b), se = FALSE, method.args = list(start = list(a = 1, b = 1))) +
             geom_smooth(method = "nls", aes(color = "Biexponential"), formula = y ~ a1*exp(-b1*x) + a2*exp(-b2*x), se = FALSE, method.args = list(start = c(a1 = 5, b1 = 2, a2 = 3, b2 = 0.5))) +
             ggtitle("Forgotten Companies") +
               ylab("Average company mention [log10]") + xlab("Days since the announcement")+
             annotate("text", x=4, y=265,  label= paste0("AIC Biexponential: ",round(AIC(biexp_fit_f),2),"; ",expression(R^2), "= " ,round(r_sqared(biexp_fit_f,forgotten$y),2)),hjust = 0)+
             annotate("text", x=4, y=225, label= paste0("AIC Exponential: ", round(gof(expmodel_f)$AIC,2)),hjust = 0) +
             annotate("text", x=4, y=192, label= paste0("AIC Hyperbolic: ",round(AIC(hyper_fit_f),2)),hjust = 0)+
             annotate("text", x=4, y=165, label= paste0("AIC Log: ",round(gof(logmodel_f)$AIC,2)),hjust = 0)+
             annotate("text", x=4, y=140, label= paste0("AIC Power-law: ",round(AIC(power_fit_f),2)),hjust = 0)+
             theme_bw()
 
 ggsave("forgotten_models.pdf", plot = p3, width = 6.67, height = 6.67, units = "in")
 p3

"""## Inter tweet time"""

 data <- read_csv(paste0(PTH1,"2012_2022_bankruptcy2.csv"), show_col_types = FALSE)
 
 
# # split data into before and after the announcement
 before <- data[grepl( 'before', data$data, fixed = TRUE),]
 after <- data[grepl( 'after', data$data, fixed = TRUE),]
 
# # delete messages originated by the companies
 after <- after[!after$author_id %in% metadata$UserID,]
 before <- before[!before$author_id %in% metadata$UserID,]
 
 
 
# # keep only companies that we want to analyze
 after<-after[after$company %in% tmp$company,]
 before<-before[before$company %in% tmp$company,]

# # before High persistence (unforgotten)
before_unforgot <- before[before$company %in% (Unforgotten_companies),]
 my_datetime <- as.POSIXct(before_unforgot$created_at, format="%Y-%m-%d %H:%M:%S", tz="UTC")
 my_datetime <- my_datetime[order(as.Date(my_datetime), decreasing = TRUE)]
 my_datetime <- cbind(my_datetime[1:(length(my_datetime)-1)], my_datetime[2:(length(my_datetime))])
 
# # inter Tweet time
 inter_Tweet_time_before_unforgot <- data.frame(my_datetime[,1] - my_datetime[,2])
 names(inter_Tweet_time_before_unforgot) <- "diff_before_unforgot"

# # before Low persistence (forgotten)
 before_forgot <- before[before$company %in% (forgotten_companies),]
 my_datetime <- as.POSIXct(before_forgot$created_at, format="%Y-%m-%d %H:%M:%S", tz="UTC")
 my_datetime <- my_datetime[order(as.Date(my_datetime), decreasing = TRUE)]
 my_datetime <- cbind(my_datetime[1:(length(my_datetime)-1)], my_datetime[2:(length(my_datetime))])
 
# # inter Tweet time
 inter_Tweet_time_before_forgot <- data.frame(my_datetime[,1] - my_datetime[,2])
 names(inter_Tweet_time_before_forgot) <- "diff_before_forgot"

library(ggplot2)
 
# # Create example data with two vectors of unequal lengths
 set.seed(123)
 x <- inter_Tweet_time_before_forgot$diff_before_forgot
 y <- inter_Tweet_time_before_unforgot$diff_before_unforgot
 
# # Merge the two vectors into a single data frame
 df <- data.frame(Inter_Tweet_Time = c(x, y),
                  Group = factor(rep(c("Low", "High"), c(length(x), length(y)))))
 
# # Create ECDF plot
 p4 <- ggplot(df, aes(Inter_Tweet_Time, color = Group)) +
           stat_ecdf(size = 1) +
           scale_x_log10() +
           scale_y_log10() +
           ggtitle("ECDF of inter-tweet time before bankruptcy announcement. \n The tweets mention forgotten and unforgotten companies") +
             ylab(expression(P(X<x))) + xlab("Inter-tweet time [Seconds]")+
           annotate("text", x=1e+3, y=0.5, label= paste0("Two-sample Kolmogorov-Smirnov test \n", "D-statistic = 0.37, p-value < 2.2e-16"),hjust = 0) +
           theme_bw()
 
 
 ggsave("ecdf.pdf", plot = p4, width = 6.67, height = 6.67, units = "in")
 p4


"""To test H1 and reveal whether forgotten and unforgotten companies have different temporal inforamtion spreading patterns, we compare the distribution of the ECDFs by using the Kolmogorov-Smirnov (KS) D-statistic test.
The D-statistic is defined as the maximum distance: D = max(|F1(x) − F2(x)|), where x represents the range of the random variable, and F1 and F2 represent the empirical cumulative distributions functions.
The smaller the distance, the more similar the distribution curves and, hence, the more likely are the two samples to come from the same distribution.
In the KS-test, a p_value < 0.05 indicates that the samples are **not** drawn from the same distribution
"""


# # KS-test
ks.test(x, y)

"""## Generate feture for logistic regression


1. **avg_time_from_first** - Time difference from the posting of the FIRST tweet about the company.
  * Captures bursty user interactions [59, 60], which can explain contagion spread [61].

2. **Pre-announcement mean**: Arithmetic mean of number of mentions in days $t \in [-30, 0]$, prior to bankruptcy announcement.


3. **Followers**


4. **Short-term boost**: Maximum mentions during days $t=0$ through $t=6$ following bankruptcy announcement, minus the pre-announcement mean.

5. **Long-term boost**

6. **number of tweets before**

7. **average tweet length**

8. **average tweet sentiment**

#### data preperation
"""

# # help functions

# # histogram and statistics
 histme<-function(x, xaxis_title=""){
                 if(xaxis_title != ""){
                         pdf(paste0("histogram_",xaxis_title,".pdf"))
                         hist(x, prob = TRUE, breaks = 50, xlab = xaxis_title)
                         lines(density(x), col = "blue", lwd = 3)
                         dev.off()
                 }
                         print(paste0("mean: ",mean(x)))
 
                         print(paste0("mean CI: ", t.test(x)$conf.int))
 
                         MedianCI(x,
                                 conf.level = 0.95,
                                 na.rm = FALSE,
                                 method = "exact",
                                 R = 10000)
 }
 
 
 
 
 
# # remove outliers and recalulate the Pre-announcement mean (PAM)
 
 PAM_remove_outliers <- function(x){
 
      Identify and remove outliers
     outliers <- boxplot(x, plot=FALSE)$out
     x_no_outliers <- x[!x %in% outliers]
     return(mean(x_no_outliers))
 
 }

# # 1) Average time difference betweeb the posting time of a tweet and the posting of the FIRST tweet about the company

 before <- before %>% group_by(company) %>% mutate(min_value = min(created_at)) # find the time of the first post
 
 before$time_from_first <- as.Date(before$created_at) - as.Date(before$min_value)
 
 result <- sqldf("select company, avg(time_from_first) as avg_time_from_first from before group by company")

# # 2) Pre-announcement mean

# # create empty dataframe
 Pre_announcement_mean <- data.frame()
 Pre_announcement_mean$company = character()
 Pre_announcement_mean$Pre_announcement_mean = numeric()
 
 for(comp in unique(tmp$company)){
 
     x <- tmp[tmp$company == comp & tmp$number<0,'freq']
     mean_PAM <- PAM_remove_outliers(x)
     Pre_announcement_mean <- rbind( Pre_announcement_mean,
                                     data.frame(
                                                   company = comp,
                                                   Pre_announcement_mean = mean_PAM
                                                 )
                                       )
 }
 
# #Pre_announcement_mean = sqldf("select avg(freq) as Pre_announcement_mean, company from tmp where number<0 group by company")
 
 result$Pre_announcement_mean <- NULL
 result <- merge(result, Pre_announcement_mean, by = 'company')
 
 histme(result[result$Pre_announcement_mean>0,'Pre_announcement_mean'])

# # 3) Followers

 df <- metadata[,c('Company','Twitter Username','Followers', 'Following','Avg_Google_Trends')]
 
 result <- merge(result, df,  by.x = 'company', by.y = 'Twitter Username')


# # 4) Short-term boost

 Short_term_boost <- sqldf("select max(freq) as shrt_boost, company from tmp where number between -1 and 17 group by company")
 
 result$shrt_boost<-NULL
 
 result <- merge(result, Short_term_boost, by = 'company')
 
 result$shrt_boost <- result$shrt_boost - result$Pre_announcement_mean
 
 histme(result$shrt_boost, "")

# # 5) Long-term Boost

 Long_term_boost <- sqldf("select max(freq) as lng_boost, company from tmp where `when`='after'and number > 17 group by company")
 
 
 result$lng_boost<-NULL
 result <- merge(result, Long_term_boost, by = 'company', all.x = TRUE)
 
 result[is.na(result$lng_boost),'lng_boost'] <- 0
 
 result$lng_boost<-result$lng_boost - result$Pre_announcement_mean
 
 histme(result[!is.na(result$lng_boost),'lng_boost'], "Long-term Boost")

# # 6) number of tweets before

 df <- sqldf("select sum(freq) as number_of_tweets_before, company from tmp where number < 0 group by company")
 
 result$number_of_tweets_before<-NULL
 result <- merge(result, df, by = 'company', all.x = TRUE)

# # 7) Average tweet length

 before$txt_length <- nchar(before$text)
 
 df = sqldf("select company, avg(txt_length) as avg_txt_len from before group by company")
 
 result$avg_txt_len<-NULL
 result <- merge(result, df, by = 'company', all.x = TRUE)

 write.csv(pastr0(PTH, before.csv))


# # 8) Average tweet sentiment

 sentiment = sqldf("select company, avg(sentiment) as avg_sentiment,avg(pos) as avg_pos,avg(neg) as avg_neg,avg(neu) as avg_neu from before group by company")
 
 result <- merge(result, sentiment, by = 'company', all.x = TRUE)


# # 9) public or private company
 result <- merge(result, metadata[,c('Twitter Username','Private_Public')], by.x = 'company' , by.y =  'Twitter Username', all.x = TRUE)
 
 result$Private_Public <- tolower(result$Private_Public)


"""## Sentiment analysis"""

"""### Unforgotten compamies: sentiment after announcment $t>0$"""

 
 after_unforgot <- data[data$company %in% Unforgotten_companies,]
 
 after_unforgot$created_at<-as.character(after_unforgot$created_at)
 
# # keep only day date (no time)
 after_unforgot$date <- substr(after_unforgot$created_at,0,10)
 
 after_unforgot <- merge(after_unforgot,metadata[,c('Twitter Username','Private_Public', 'Date of Bankruptcy')], by.x='company', by.y='Twitter Username')
 
 after_unforgot$number <- as.Date(after_unforgot$date) - as.Date(after_unforgot$`Date of Bankruptcy`,format = "%B %d, %Y")  day diff from bankruptcy (in unix timestamp)
 
 after_unforgot <- after_unforgot[after_unforgot$number >= 0, ]

# 8) Average tweet sentiment
write.csv(paste0(PTH,"after_unforgot.csv"))

# Get the sentiment score of a text vector

after_unforgot = dentiment_analyzer(after_unforgot)

after_unforgot <- read_csv('/content/after_unforgot.csv',show_col_types = FALSE)

a_uf <- sqldf("select company, number, avg(sentiment) as avg_sent, avg(pos) as avg_sent_pos, avg(neg) as avg_sent_neg, avg(neu) as avg_sent_neu from after_unforgot group by company, number")

"""### Forgotten compamies: sentiment after announcment $t>0$"""


 after_forgot <- data[data$company %in% forgotten_companies,]
 
 after_forgot$created_at<-as.character(after_forgot$created_at)
 
# # keep only day date (no time)
 after_forgot$date <- substr(after_forgot$created_at,0,10)
 
 after_forgot <- merge(after_forgot,metadata[,c('Twitter Username','Private_Public', 'Date of Bankruptcy')], by.x='company', by.y='Twitter Username')
 
 after_forgot$number <- as.Date(after_forgot$date) - as.Date(after_forgot$`Date of Bankruptcy`,format = "%B %d, %Y") # day diff from bankruptcy (in unix timestamp)
 
 after_forgot <- after_forgot[after_forgot$number >= 0, ]

# Average tweet sentiment
write.csv(paste0(PTH,"after_forgot.csv"))

# Get the sentiment score of a text vector

after_forgot = dentiment_analyzer(after_forgot)


after_forgot <- after_forgot
a_f <- sqldf("select company, number, avg(sentiment) as avg_sent, avg(pos) as avg_sent_pos, avg(neg) as avg_sent_neg, avg(neu) as avg_sent_neu from after_forgot group by company, number")

"""### Boxplot of High and Low persistence sentiment

### before bankruptcy
"""

# # -------------------  Unforgotten -----------------------------------------------
 
 before_unforgot <- data[data$company %in% Unforgotten_companies,]
 
 before_unforgot$created_at<-as.character(before_unforgot$created_at)
 
# # keep only day date (no time)
 before_unforgot$date <- substr(before_unforgot$created_at,0,10)
 
 before_unforgot <- merge(before_unforgot,metadata[,c('Twitter Username','Private_Public_Clean', 'Date of Bankruptcy')], by.x='company', by.y='Twitter Username')
 
 before_unforgot$number <- as.Date(before_unforgot$date) - as.Date(before_unforgot$`Date of Bankruptcy`,format = "%B %d, %Y")  day diff from bankruptcy (in unix timestamp)
 
 before_unforgot <- before_unforgot[before_unforgot$number < 0, ]
 
 
# # -------------------  Forgotten -----------------------------------------------
 
 before_forgot <- data[data$company %in% forgotten_companies,]
 
 before_forgot$created_at<-as.character(before_forgot$created_at)
 
# # keep only day date (no time)
 before_forgot$date <- substr(before_forgot$created_at,0,10)
 
 before_forgot <- merge(before_forgot,metadata[,c('Twitter Username','Private_Public', 'Date of Bankruptcy')], by.x='company', by.y='Twitter Username')
 
 before_forgot$number <- as.Date(before_forgot$date) - as.Date(before_forgot$`Date of Bankruptcy`,format = "%B %d, %Y")  day diff from bankruptcy (in unix timestamp)
 
 before_forgot <- before_forgot[before_forgot$number < 0, ]

# Average tweet sentiment
write.csv(paste0(PTH,'before_unforgot.csv'))


# Average tweet sentiment
write.csv(paste0(PTH,'before_forgot.csv'))

# Get the sentiment score of a text vector

before_unforgot = dentiment_analyzer(before_unforgot)

#---- before_forgot --------
# Get the sentiment score of a text vector
before_forgot = dentiment_analyzer(before_forgot)

before_unforgot <- read_csv('/content/before_unforgot.csv',show_col_types = FALSE)

before_forgot <- read_csv('/content/before_forgot.csv',show_col_types = FALSE)


# # Low
b_f <- sqldf("select company, number, avg(sentiment) as avg_sent, avg(pos) as avg_sent_pos, avg(neg) as avg_sent_neg, avg(neu) as avg_sent_neu from before_forgot group by company, number")
 
# # High
 b_uf <- sqldf("select company, number, avg(sentiment) as avg_sent, avg(pos) as avg_sent_pos, avg(neg) as avg_sent_neg, avg(neu) as avg_sent_neu from before_unforgot group by company, number")

"""### Boxplots"""

# # combine High and Low into a single data.frame.
 b_f$Persistence_t = "before_Low"
 b_f$Persistence = "Low"
 
 b_uf$Persistence_t = "before_High"
 b_uf$Persistence = "High"
 
 a_f$Persistence_t = "after_Low"
 a_f$Persistence = "Low"
 
 a_uf$Persistence_t = "after_High"
 a_uf$Persistence = "High"
 
 b <- rbind(b_f, b_uf, a_f, a_uf)

# #-------------- Low persistence --------------------------------------
 pdf(file="Low_Sentiment_Boxplot.pdf")
     boxplot(
         b[b$Persistence_t == 'before_Low', 'avg_sent_pos'],
         b[b$Persistence_t == 'after_Low', 'avg_sent_pos'],
         b[b$Persistence_t == 'before_Low', 'avg_sent_neg'],
         b[b$Persistence_t == 'after_Low', 'avg_sent_neg'],
         b[b$Persistence_t == 'before_Low', 'avg_sent_neu'],
         b[b$Persistence_t == 'after_Low', 'avg_sent_neu'],
         b[b$Persistence_t == 'before_Low', 'avg_sent'],
         b[b$Persistence_t == 'after_Low', 'avg_sent'],
         names = c('Pos B', 'Pos A', 'Neg B', 'Neg A', 'Neu B', 'Neu A','All B', 'All A'),
         ylab ='Sentiment',
         main='Sentiment of Events with Low Persistence',
         col = c("red","lightblue")
         )
     legend("topleft", c("After (A)","Before (B)"), border=c("lightblue", "red"), fill = c("lightblue", "red"))
          Add sample size on top
     text(
           x=c(1.5, 3.5, 5.5,7.5),
           y= c(-0.1, -0.25, 0.2, -0.1),
           c("P-value = 5.921e-04",
             "P-value = 0.018 \n Delta = 119%",
             "P-value = 0.1086 \n Delta = 100.69%",
             "P-value = 1.198e-03"),
          cex = 0.9,
         )
 dev.off()
 
 
# #-------------- High persistence --------------------------------------
 pdf(file="High_Sentiment_Boxplot.pdf")
     boxplot(
             b[b$Persistence_t == 'before_High', 'avg_sent_pos'],
             b[b$Persistence_t == 'after_High', 'avg_sent_pos'],
             b[b$Persistence_t == 'before_High', 'avg_sent_neg'],
             b[b$Persistence_t == 'after_High', 'avg_sent_neg'],
             b[b$Persistence_t == 'before_High', 'avg_sent_neu'],
             b[b$Persistence_t == 'after_High', 'avg_sent_neu'],
             b[b$Persistence_t == 'before_High', 'avg_sent'],
             b[b$Persistence_t == 'after_High', 'avg_sent'],
             names = c('Pos B', 'Pos A', 'Neg B', 'Neg A', 'Neu B', 'Neu A','All B', 'All A'),
             ylab ='Sentiment',
             main='Sentiment of Events with High Persistence',
             col = c("red","lightblue")
             )
     legend("topleft", c("After (A)","Before (B)"), border=c("lightblue", "red"), fill = c("lightblue", "red"))
         text(
           x=c(1.5, 3.5, 5.5,7.5),
           y= c(-0.1, -0.25, 0.2, -0.1),
           c("P-value = 3.039e-06",
             "P-value = 3.297e-05 \n Delta = 117.17%",
             "P-value = 0.01256 \n Delta = 101.02%",
             "P-value = 4.832e-08"),
          cex = 0.9,
         )
 dev.off()


 boxplot(
         b[b$Persistence_t == 'before_Low', 'avg_sent_pos'],
         b[b$Persistence_t == 'after_Low', 'avg_sent_pos'],
         b[b$Persistence_t == 'before_Low', 'avg_sent_neg'],
         b[b$Persistence_t == 'after_Low', 'avg_sent_neg'],
         b[b$Persistence_t == 'before_Low', 'avg_sent_neu'],
         b[b$Persistence_t == 'after_Low', 'avg_sent_neu'],
         b[b$Persistence_t == 'before_Low', 'avg_sent'],
         b[b$Persistence_t == 'after_Low', 'avg_sent'],
         names = c('Pos B', 'Pos A', 'Neg B', 'Neg A', 'Neu B', 'Neu A','All B', 'All A'),
         ylab ='Sentiment',
         main='Sentiment of Events with Low Persistence',
         col = c("red","lightblue")
         )
     legend("topleft", c("After (A)","Before (B)"), border=c("lightblue", "red"), fill = c("lightblue", "red"))
      Add sample size on top
     text(
           x=c(1.5, 3.5, 5.5,7.5),
           y= c(-0.1, -0.25, 0.2, -0.1),
           c("P-value = 5.921e-04",
             "P-value = 0.018 \n Delta = 119%",
             "P-value = 0.1086 \n Delta = 100.69%",
             "P-value = 1.198e-03"),
          cex = 0.9,
         )
 
 
 
   boxplot(
         b[b$Persistence_t == 'before_High', 'avg_sent_pos'],
         b[b$Persistence_t == 'after_High', 'avg_sent_pos'],
         b[b$Persistence_t == 'before_High', 'avg_sent_neg'],
         b[b$Persistence_t == 'after_High', 'avg_sent_neg'],
         b[b$Persistence_t == 'before_High', 'avg_sent_neu'],
         b[b$Persistence_t == 'after_High', 'avg_sent_neu'],
         b[b$Persistence_t == 'before_High', 'avg_sent'],
         b[b$Persistence_t == 'after_High', 'avg_sent'],
         names = c('Pos B', 'Pos A', 'Neg B', 'Neg A', 'Neu B', 'Neu A','All B', 'All A'),
         ylab ='Sentiment',
         main='Sentiment of Events with High Persistence',
         col = c("red","lightblue")
         )
 legend("topleft", c("After (A)","Before (B)"), border=c("lightblue", "red"), fill = c("lightblue", "red"))
     text(
       x=c(1.5, 3.5, 5.5,7.5),
       y= c(-0.1, -0.25, 0.2, -0.1),
       c("P-value = 3.039e-06",
         "P-value = 3.297e-05 \n Delta = 117.17%",
         "P-value = 0.01256 \n Delta = 101.02%",
         "P-value = 4.832e-08"),
       cex = 0.9,
     )

"""### Wilcoxon tests

of sentiment within the Low and High persistence groups of tweets Before vs. After bankruptcy announcment.

values are average per day for all companies in each of the High or Low group
"""

# #-------------- Within High persistence ONLY--------------------------------------
 
# # positive
 print("----------- High positive --------------")
 print(wilcox.test(
                     b[b$Persistence_t == 'before_High', 'avg_sent_pos'], # average daily sentiment for days t-30 to t+30
                     b[b$Persistence_t == 'after_High', 'avg_sent_pos']
                   )
       )
 
# # negative
 print("----------- High negative --------------")
 print(wilcox.test(
                     b[b$Persistence_t == 'before_High', 'avg_sent_neg'],
                     b[b$Persistence_t == 'after_High', 'avg_sent_neg']
                   )
       )
 
# # neutral
 print("----------- High neutral --------------")
 print(wilcox.test(
                     b[b$Persistence_t == 'before_High', 'avg_sent_neu'],
                     b[b$Persistence_t == 'after_High', 'avg_sent_neu']
                   )
       )
 
 
# # All
 print("----------- High All --------------")
 print(wilcox.test(
                     b[b$Persistence_t == 'before_High', 'avg_sent'],
                     b[b$Persistence_t == 'after_High', 'avg_sent']
                   )
       )


 median(b[b$Persistence_t == 'after_Low', 'avg_sent'])


# #-------------- Within Low persistence ONLY --------------------------------------
 
# # positive
 print("----------- Low positive --------------")
 print(wilcox.test(
                     b[b$Persistence_t == 'before_Low', 'avg_sent_pos'],
                     b[b$Persistence_t == 'after_Low', 'avg_sent_pos']
                   )
       )
 
# # negative
 print("----------- Low negative --------------")
 print(wilcox.test(
                     b[b$Persistence_t == 'before_Low', 'avg_sent_neg'],
                     b[b$Persistence_t == 'after_Low', 'avg_sent_neg']
                   )
       )
 
# # neutral
 print("----------- Low neutral --------------")
 print(wilcox.test(
                     b[b$Persistence_t == 'before_Low', 'avg_sent_neu'],
                     b[b$Persistence_t == 'after_Low', 'avg_sent_neu']
                   )
       )
 
 
# # All
 print("----------- Low All --------------")
 print(wilcox.test(
                     b[b$Persistence_t == 'before_Low', 'avg_sent'],
                     b[b$Persistence_t == 'after_Low', 'avg_sent']
                   )
       )

"""### Test for significant **percentile** increase for High vs. Low persistence"""

# #----------------------------------------- Low ---------------------------------
# # Pos: diff between before and after for Low
 diff_pos_Low  <-  (b[b$Persistence_t == 'after_Low', 'avg_sent_pos']) - (b[b$Persistence_t == 'before_Low', 'avg_sent_pos'])
 
# # Neg: diff between before and after for Low
 diff_neg_Low  <- (b[b$Persistence_t == 'after_Low', 'avg_sent_neg']) - (b[b$Persistence_t == 'before_Low', 'avg_sent_neg'])
 
# # Neu: diff between before and after for Low
 diff_neu_Low  <- (b[b$Persistence_t == 'after_Low', 'avg_sent_neu']) - (b[b$Persistence_t == 'before_Low', 'avg_sent_neu'])
 
# # All: diff between before and after for Low
 diff_all_Low  <- (b[b$Persistence_t == 'after_Low', 'avg_sent'])  - (b[b$Persistence_t == 'before_Low', 'avg_sent'])
 
 
# # percentile
 diff_pos_Low_percent <- diff_pos_Low/b[b$Persistence_t == 'before_Low', 'avg_sent_pos']
 diff_neg_Low_percent <- diff_neg_Low/b[b$Persistence_t == 'before_Low', 'avg_sent_neg']
 diff_neu_Low_percent <- diff_pos_Low/b[b$Persistence_t == 'before_Low', 'avg_sent_neu']
 diff_all_Low_percent <- diff_all_Low/b[b$Persistence_t == 'before_Low', 'avg_sent']
 
# #----------------------------------------- High ---------------------------------
# # Pos: diff between before and after for High
 diff_pos_High  <- (b[b$Persistence_t == 'after_High', 'avg_sent_pos']) - (b[b$Persistence_t == 'before_High', 'avg_sent_pos'])
 
# # Neg: diff between before and after for High
 diff_neg_High  <- (b[b$Persistence_t == 'after_High', 'avg_sent_neg']) - (b[b$Persistence_t == 'before_High', 'avg_sent_neg'])
 
# # Neu: diff between before and after for High
 diff_neu_High  <- (b[b$Persistence_t == 'after_High', 'avg_sent_neu']) - (b[b$Persistence_t == 'before_High', 'avg_sent_neu'])
 
# # All: diff between before and after for Low
 diff_all_High  <- (b[b$Persistence_t == 'after_High', 'avg_sent']) - (b[b$Persistence_t == 'before_High', 'avg_sent'])
 
# # percentile
 diff_pos_High_percent <- diff_pos_High/b[b$Persistence_t == 'before_High', 'avg_sent_pos']
 diff_neg_High_percent <- diff_neg_High/b[b$Persistence_t == 'before_High', 'avg_sent_neg']
 diff_neu_High_percent <- diff_neu_High/b[b$Persistence_t == 'before_High', 'avg_sent_neu']
 diff_all_High_percent <- diff_all_High/b[b$Persistence_t == 'before_High', 'avg_sent']

 diff_pos_High_percent <- diff_pos_Low_percent[is.finite(diff_pos_High_percent)]
 diff_neg_High_percent <- diff_pos_Low_percent[is.finite(diff_neg_High_percent)]
 diff_neu_High_percent <- diff_pos_Low_percent[is.finite(diff_neu_High_percent)]
 diff_all_High_percent <- diff_pos_Low_percent[is.finite(diff_all_High_percent)]


# #-------------- Wilcoxon percentile between High and Low persistence -----------

# # positive
 print("----------- Pos percentile increase: LOW vs. High --------------")
 print(wilcox.test(diff_pos_Low_percent, diff_pos_High_percent))
 
 
 print("----------- Neg percentile increase: LOW vs. High --------------")
 print(wilcox.test(diff_neg_Low_percent, diff_neg_High_percent))
 
 
 print("----------- Neu percentile increase: LOW vs. High --------------")
 print(wilcox.test(diff_neu_Low_percent, diff_neu_High_percent))
 
 
 print("----------- All percentile increase: LOW vs. High --------------")
 print(wilcox.test(diff_all_High_percent, diff_all_High_percent))

"""#### check the means of sentiment

To discover the increase in the mean for pos, neg, neu, and all
"""

# # ---- High mean sentiment----
 
 print("---- High: mean sentiment Pos before----")
 print(mean(b[b$Persistence_t == 'before_High', 'avg_sent_pos']))
 
 print("---- High: mean sentiment Pos after----")
 print(mean(b[b$Persistence_t == 'after_High', 'avg_sent_pos']))
 
 print("---- High: mean sentiment Neg before----")
 print(mean(b[b$Persistence_t == 'before_High', 'avg_sent_neg']))
 
 print("---- High: mean sentiment Neg after----")
 print(mean(b[b$Persistence_t == 'after_High', 'avg_sent_neg']))
 
 print("---- High: mean sentiment Neu before----")
 print(mean(b[b$Persistence_t == 'before_High', 'avg_sent_neu']))
 
 print("---- High: mean sentiment Neu after----")
 print(mean(b[b$Persistence_t == 'after_High', 'avg_sent_neu']))
 
 
 print("---- High: mean sentiment All before----")
 print(mean(b[b$Persistence_t == 'before_High', 'avg_sent']))
 
 print("---- High: mean sentiment All after----")
 print(mean(b[b$Persistence_t == 'after_High', 'avg_sent']))
 
 
 
# # ---- calc the delta of increas for neg, and neu
 print("---- High: delta %change in the mean before vs after sentiment for Neg ----")
 print( 100*mean(b[b$Persistence_t == 'after_High', 'avg_sent_neg']) / mean(b[b$Persistence_t == 'before_High', 'avg_sent_neg']) )
 
 print("---- Low: delta %change in the mean before vs after sentiment for Neg ----")
 print( 100*mean(b[b$Persistence_t == 'after_Low', 'avg_sent_neg']) / mean(b[b$Persistence_t == 'before_Low', 'avg_sent_neg']) )
 
 
 print("---- High: delta %change in the mean before vs after sentiment for Neu ----")
 print( 100*mean(b[b$Persistence_t == 'after_High', 'avg_sent_neu']) / mean(b[b$Persistence_t == 'before_High', 'avg_sent_neu']) )
 
 print("---- Low: delta %change in the mean before vs after sentiment for Neu ----")
 print( 100*mean(b[b$Persistence_t == 'after_Low', 'avg_sent_neu']) / mean(b[b$Persistence_t == 'before_Low', 'avg_sent_neu']) )

# # ---- Low mean sentiment----
 print("---- Low: mean sentiment Pos before----")
 print(mean(b[b$Persistence_t == 'before_Low', 'avg_sent_pos']))
 
 print("---- Low: mean sentiment Pos after----")
 print(mean(b[b$Persistence_t == 'after_Low', 'avg_sent_pos']))
 
 
 print("---- Low: mean sentiment Neg before----")
 print(mean(b[b$Persistence_t == 'before_Low', 'avg_sent_neg']))
 
 print("---- Low: mean sentiment Neg after----")
 print(mean(b[b$Persistence_t == 'after_Low', 'avg_sent_neg']))
 
 
 
 print("---- Low: mean sentiment Neu before----")
 print(mean(b[b$Persistence_t == 'before_Low', 'avg_sent_neu']))
 
 print("---- Low: mean sentiment Neu after----")
 print(mean(b[b$Persistence_t == 'after_Low', 'avg_sent_neu']))
 
 
 
 print("---- Low: mean sentiment All before----")
 print(mean(b[b$Persistence_t == 'before_Low', 'avg_sent']))
 
 print("---- Low: mean sentiment All after----")
 print(mean(b[b$Persistence_t == 'after_Low', 'avg_sent']))
 

"""## Regression"""

# # set target variable
 result$y <- 0 # zero means High persistence
 result[result$company %in% forgotten_companies, 'y'] <- 1 # means Low persistence
 
 result$company_name <- NULL
 
 result$y <- as.factor(result$y)


# # Specify the proportion of data to be used for train
train_proportion <- 0.7

# # Split the data into training and test sets
 set.seed(123) # Set a random seed for reproducibility
 train_indices <- createDataPartition(y = result$y, p = train_proportion, list = FALSE)
 train_data <- result[train_indices, ]
 test_data <- result[-train_indices, ]

# # logistic regression
# # define training control cross validation
train_control <- trainControl(method = "cv", number = 10)
 
# # train the model on training set
 model <- train(y ~ avg_time_from_first + avg_sentiment + avg_txt_len + Pre_announcement_mean +
                number_of_tweets_before + Followers + Following + shrt_boost + avg_txt_len	+
                avg_sentiment	+ avg_pos	+ avg_neg	+ avg_neu,   Private_Public +
                family = binomial(),
                trControl = train_control,
                method = "glmStepAIC",
                direction ="backward",  forward
                data = train_data,
                trace = 2)

# # print cv scores
 print(summary(model))

# # check varibale importance
varImp(model$finalModel)

# # labeling
# # 0: High persistent
# # 1: Low persistent

# # Evaluation of the best model of the cross validation

 threshold=0.5
 predicted_values<-as.factor(ifelse(predict(model$finalModel,type="response")>threshold,1,0))
 actual_values<-as.factor(model$finalModel$y)
 confusionMatrix(predicted_values, actual_values, mode = "everything", positive="0")

# # Make predictions on the test set

 test_predictions <- predict(model$finalModel, newdata = test_data,type = "response")
 
# # Set a threshold value (e.g., 0.5) for binary classification
 threshold <- 0.5
 
# # Convert predicted probabilities to binary values based on the threshold
 test_predictions <- as.factor(ifelse(test_predictions > threshold, 1, 0))
 
 test_data$y <- as.factor(test_data$y)
 
# # Compute the confusion matrix
 confusionMatrix(test_predictions, test_data$y, mode = "everything", positive="0")
 
 
# # 0:= unforgotten companies
# # 1:= forgotten companies

"""# Other measures"""
# # 3) Long-term boost

 Long_term_boost = sqldf("select avg(freq) as lng_boost, company from tmp where `when`='after' and number between 7 and 40 group by company")
 Long_term_boost$shrt_boost <- Long_term_boost$lng_boost - Pre_announcement_mean$avgfreq
 
 print(Long_term_boost)
 
 library(DescTools)
 MedianCI(Long_term_boost$shrt_boost,
          conf.level = 0.95,
          na.rm = FALSE,
          method = "exact",
          R = 10000)

# # calculate AUC

 comp = 'TrueReligion'
 win = 3
 x = AUC(x=tmp[tmp$when =='after'& tmp$company==comp,]$number[1:win], y=tmp[tmp$when =='after'& tmp$company==comp,]$freq[1:win]) /
                                                                     AUC(x=tmp[tmp$when =='after'& tmp$company==comp,]$number, y=tmp[tmp$when =='after'& tmp$company==comp,]$freq)
 
 print(x)

"""# Plot daily tweet count per company"""

 unique(tmp$company)

# # create a list of ggplots

 figures <- list()
 i = 1
 
 tmp$number <- as.numeric(tmp$number)
 tmp$freq <- as.numeric(tmp$freq)
 
 for(comp in unique(tmp$company)){
   figures[[i]] <- ggplot(data=tmp[tmp$company==comp,], aes(x=number, y=freq)) +
                       geom_line()+
                       theme_bw() +
                       labs(x="Days from bankruptcy announcment", y = "Number of tweets")+
                       ggtitle(paste0("@",comp)) +
                       theme(plot.title = element_text(hjust = 0.5)) +
                       geom_point()
 
   plot(figures[[i]])
   i = i +1
 }


 comp <- 'A123Systems'
 PAM <- mean(tmp[tmp$company== comp & tmp$number < 0,'freq'])
 LTB <- mean(tmp[tmp$company== comp & tmp$number > 6,'freq'])
 
 sz <- 3
 
 i <- which(comp == unique(tmp$company))
 
 print(i)
 
 p1 <- figures[[i]] +  geom_text(aes(x = -17, y = PAM+6, label = "Pre-announcement \n mean"), size = sz, color="red", check_overlap = TRUE) +
                                   geom_segment(aes(x = -30, y = PAM, xend = 0, yend = PAM), color = "red") +
                                   geom_segment(aes(x = -1, y = PAM, xend = -1, yend = tmp[tmp$company== comp & tmp$number == 0,'freq']), color = "darkgreen", arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
                                   geom_segment(aes(x = -1, y = tmp[tmp$company== comp & tmp$number == 0,'freq'], xend = -1, yend = PAM), color = "Blue", arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
                                   geom_text(aes(x = -10, y = tmp[tmp$company== comp & tmp$number == 0,'freq']/2, label = "Short-term \n Boost"), size = sz, color="Blue", check_overlap = TRUE) +
                                   geom_segment(aes(x = 6, y = LTB, xend = 30, yend = LTB), color = "Darkgreen") +
                                   geom_text(aes(x = 20, y = LTB+5, label = "Long-term \n Boost"), size = sz, color="Darkgreen", check_overlap = TRUE)
 
 p1

 comp <- 'IndonesiaGaruda'
 i <- which(comp == unique(tmp$company))
 print(i)
 figures[[i]]

unique(tmp$company)


# # plot the figures

 p<-ggarrange(p1,
              figures[[33]],
              figures[[51]],
              figures[[76]],
              figures[[7]],
              figures[[16]],
              figures[[17]],
              figures[[38]],
           ncol = 4, nrow = 2)
 options(repr.plot.width = 12, repr.plot.height = 6)  #set canvas size
 print(p)
 ggexport(p, filename = "spikes.pdf", width = 12, height = 6) # write figure to pdf

"""#### plot average number of tweets before vs. after the announcement for all companies"""

# # average number of tweets before and after the announcement
fig2 <- sqldf("select number, avg(freq) as n_d from tmp group by number",method = "name__class")
write.csv(fig2, file="fig2.csv")


 p2<-ggplot(data=fig2, aes(x=number, y=n_d)) +
                       geom_line()+
                       scale_y_continuous(trans='log10')+
                       theme_bw() +
                       labs(x="Days from bankruptcy announcment", y = "Average number of tweets")+
                       ggtitle("Average mention time series") +
                       theme(plot.title = element_text(hjust = 0.5)) +
                       geom_hline(yintercept=mean(fig2[fig2$number!=0,]$n_d), linetype="dashed", color = "red") +
                       geom_point()
 
 print(p2)
 
 ggexport(p2, filename = "figures3.pdf", width = 6, height = 6) # write figure to pdf


 df_four_measures = data.frame(
                               'Pre_announcement_mean' = Pre_announcement_mean$avgfreq,
                               'Short_term_boost' = Short_term_boost$shrt_boost,
                               'Long_term_boost' = Long_term_boost$lng_boost,
                               'Halving' = as.numeric(halving$halving)
                               )
 
 df_four_measures


 mat = as.matrix(df_four_measures)

 res<-NbClust(mat, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "all")


"""## regression

1. Pre-announcement mean mention frequency.
2. Company age (year) at the day of bankruptcy announcement.
3. Bankruptcy strategy (factor with 5 levels: restructuring, reorganization, purchased, sold, closed all stores).
4. Market cup category (factor with 6 levels: specifying the main business of the company: sports, clothes, etc.
5. Market capitalization.
6. Gender of owners (factor with 3 levels: female, male, mixed).



"""
 Pre_announcement_mean$avgfreq<-scale(Pre_announcement_mean$avgfreq)
 Pre_announcement_mean


# # average sentiment analysis of each company
# # load before and after datasets


 before <- read_csv(paste0(PTH1,"/before.csv"),col_types = cols(...1 = col_skip(), id = col_character(),
                                                                        in_reply_to_user_id = col_character(),
                                                                        author_id = col_character(),
                                                                        conversation_id = col_character()))
 
 
 before<-before[!before$company %in% c('DeanAndDeLuca', 'TailorBrands'),]

 tweets_R = before$text

%R -o tweets_R tweets_R

#------ python code ---------
import re
result = [re.sub(r'http\S+', '', x) for x in tweets_R]
from textblob import TextBlob

sentiment = []
subjectivity = []

for text in result:
  x = TextBlob(text)
  sentiment.append( x.sentiment[0] )
  subjectivity.append( x.sentiment[1] ) # The subjectivity is a float within the range [0.0, 1.0] where 0.0 is very objective and 1.0 is very subjective.

#------ end python code ---------
 send back sentiment to R
 %R -i sentiment sentiment
 %R -i subjectivity subjectivity


# # list into vector
sentiment = unlist(sentiment)
 subjectivity = unlist(subjectivity)

 before$sentiment <- sentiment
 before$subjectivity <- subjectivity
 
 mean_sent_by_comp = sqldf("select avg(sentiment), company from before group by company")
 
 mean_subj_by_comp = sqldf("select avg(subjectivity), company from before group by company")
 
 
 mean_sent_by_comp<-mean_sent_by_comp[!mean_sent_by_comp$company %in% c('KikoMilanoUSA', ''),]
 mean_subj_by_comp<-mean_subj_by_comp[!mean_subj_by_comp$company %in% c('KikoMilanoUSA', ''),]

# # number of tweets before
 num_of_tweets_before = sqldf("select company, count(1) as n_tweets from before group by company")
 num_of_tweets_before = num_of_tweets_before[num_of_tweets_before$company != "KikoMilanoUSA",]
 num_of_tweets_before

 unique(before$company)

# # calculate mean inter-tweet time
 inter_tweet_time = data.frame()
 for(company in unique(before$company)){
     v = as.POSIXct( before[before$company==company,]$created_at , format = "%a %b %d %H:%M:%S %z %Y", tz="GMT")
     v = v[order(v , decreasing = TRUE )]
     d = diff(v)
     inter_tweet_time = rbind(inter_tweet_time, c(company, mean(d)))
 }
 
 names(inter_tweet_time) = c('company','avg_inter_time')
 inter_tweet_time = inter_tweet_time[order(num_of_tweets_before$company),]
 inter_tweet_time = sqldf("select company, avg_inter_time/60 as avg_inter_time from inter_tweet_time group by company")  we do this to keep the order of companies
 inter_tweet_time <- inter_tweet_time[inter_tweet_time$company != 'KikoMilanoUSA',]
 inter_tweet_time
