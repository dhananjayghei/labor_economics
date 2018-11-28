# Loading in the required libraries
library(dplyr)

# Reading in the data
bhps <- read.csv("../data/bm_data.csv", header=TRUE)

# Sex Ratio
# Male is coded as 1, Female is coded as 0
sex_ratio <- round(prop.table(table(bhps$sex))*100, digits=2)
names(sex_ratio) <- c("Female", "Male")

# Proportion of people with right censoring
# For initially employed people (e1=1)
Rcens_job <- bhps %>%
    filter(e1==1) %>%
    summarise(total=nrow(.),
              j2jT = sum(j2j[which(j2j==1)]),
              j2uT = sum(j2u[which(j2u==1)])) %>%
    mutate(Rcens_job = 1 - (j2jT+j2uT)/total) %>% data.frame()

# For initially unemployed people (e1=0)
Rcens_unemp <- bhps %>%
    filter(e1==0) %>%
    summarise(total=nrow(.),
              u2jT = sum(u2j[which(u2j==1)])) %>%
    mutate(Rcens_unemp = 1 - (u2jT/total)) %>% data.frame()


# Plotting the Empirical CDF of log wages in 1
Gcdf <- ecdf(bhps$logw1)
# Plotting the kernel density of log wages in 1
Gdens <- density(bhps$logw1, na.rm=TRUE)
# Create a variable categorizing logw1 into 25 bins
logw1_qtls <- quantile(bhps$logw1, probs=seq(0,1,1/25), na.rm=TRUE)
# Calculate the mean spell duration in each of the quantiles
avg_spelldur1 <- do.call(rbind, lapply(logw1_qtls, function(x){
    avg_spelldur1 <- mean(bhps[which(bhps$logw1 <= x), "spelldur1"], na.rm=TRUE)
    return(avg_spelldur1)
}))
# Binding the qunatiles with the mean spell duraation
dur_wages <- data.frame(logw1_qtls=logw1_qtls, avg_spelldur1=avg_spelldur1)
plot(dur_wages$logw1_qtls, dur_wages$avg_spelldur1, type="l")


