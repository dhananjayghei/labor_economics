# Loading in the required libraries
library(dplyr)

# Reading in the data
bhps <- read.csv("../data/bm_data.csv", header=TRUE)

# Sex Ratio
# Male is coded as 1, Female is coded as 0
sex_ratio <- round(prop.table(table(bhps$sex))*100, digits=2)
names(sex_ratio) <- c("Female", "Male")

# Sample unemployment rate
# Initial spell unemployment rate
# NOT sure if this is the correct way to measure unemployment rate
# in the sample.
# This calculates the unemployment rate only in the initial spell. 
unemp_overall <- bhps %>%
    summarise(unemp = length(e1[which(e1==0)]),
              unemp_rate = unemp*100/nrow(.)) %>% data.frame()

# Sample unemployment rate by sex
# Initial spell unemployment rate
unemp_sex <- bhps %>%
    group_by(sex) %>%
    summarise(unemp = length(e1[which(e1==0)]),
              unemp_rate = unemp*100/nrow(.)) %>% data.frame()

# Sample unemployment rate by education
unemp_educ <- bhps %>%
    group_by(educ) %>%
    summarise(unemp = length(e1[which(e1==0)]),
              unemp_rate = unemp*100/nrow(.)) %>% data.frame()

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


# Plotting the Empirical CDF G in spell 1 (Gcdf is also a function)
Gcdf <- ecdf(bhps$logw1)
# Plotting the kernel density of log wages in spell 1
Gdens <- density(bhps$logw1, na.rm=TRUE)
# Constructing the density function using linear interpolation
Gdensity <- approxfun(Gdens)
# Create a variable categorizing logw1 into 25 bins
bhps$logw1_qtls <- cut(bhps$logw1, quantile(bhps$logw1, probs=seq(0,1,1/25), na.rm=TRUE))
#Getting the values of the quantiles
logw1_qtls <- as.numeric(unlist(strsplit(unlist(strsplit(names(table(bhps$logw1_qtls)), split="[,]"))[seq(2,50,by=2)], split="]")))
# Changing the naming to Q's for easy readability
bhps$logw1_qtls <- cut(bhps$logw1, quantile(bhps$logw1, probs=seq(0,1,1/25), na.rm=TRUE), labels=paste0("Q", 1:25))
# Calculate the mean spell duration in each of the quantiles
avg_spelldur1 <- bhps %>%
    group_by(logw1_qtls) %>%
    summarise(avg_spelldur1 = mean(spelldur1, na.rm=TRUE)) %>%
    data.frame() %>%
    filter(!is.na(logw1_qtls))
# Binding the quantiles with the mean spell duraation
dur_wages <- data.frame(wage1_bins=logw1_qtls, avg_spelldur1=avg_spelldur1)

# Construct the CDF F (Fcdf is also a function)
Fcdf <- ecdf(bhps[which(bhps$e1==0 & bhps$u2j==1), "logw2"])
# Constructing the estimate of f
Fdens <- density(bhps[which(bhps$e1==0 & bhps$u2j==1), "logw2"], na.rm=TRUE)
# Constructing the density function using linear interpolation
Fdensity <- approxfun(Fdens)

# Estimating kappa_1 non-paramterically
kappa1 <- function(x){
    kappa1 <- (Fcdf(x)-Gcdf(x))/(Gcdf(x)*(1-Fcdf(x)))
    return(kappa1)
}

wages1 <- unique(bhps$logw1)
kappa_1 <- do.call(rbind, lapply(wages1, kappa1))
kappa_1 <- kappa_1[is.finite(kappa_1)]
