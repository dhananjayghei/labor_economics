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
              total = length(e1),
              unemp_rate = round(unemp*100/total, 3)) %>% data.frame()
unemp_sex$sex <- c("Female", "Male")
colnames(unemp_sex) <- c("Sex", "Unemployed", "Total", "Unemployment Rate")
# Sample unemployment rate by education
unemp_educ <- bhps %>%
    group_by(educ) %>%
    summarise(unemp = length(e1[which(e1==0)]),
              total = length(e1),
              unemp_rate = round(unemp*100/nrow(.),3)) %>% data.frame()
unemp_educ$educ <- c("less than A-levels", "A-levels", "Some Higher Education")
colnames(unemp_educ) <- c("Education", "Unemployed", "Total", "Unemployment Rate")

# Proportion of people with right censoring
# For initially employed people (e1=1)
Rcens_emp <- bhps %>%
    filter(e1==1) %>%
    summarise(total=nrow(.),
              j2jT = sum(j2j[which(j2j==1)]),
              j2uT = sum(j2u[which(j2u==1)])) %>%
    mutate(Rcens_job = 1 - (j2jT+j2uT)/total) %>% data.frame()
# Adding a variable for right censoring for emplyed (will be used for MLE)
bhps$right_censor <- 1
bhps[which(bhps$e1==1 & (bhps$j2j==1 | bhps$j2u==1)), "right_censor"] <- 0

# For initially unemployed people (e1=0)
Rcens_unemp <- bhps %>%
    filter(e1==0) %>%
    summarise(total=nrow(.),
              u2jT = sum(u2j[which(u2j==1)])) %>%
    mutate(Rcens_unemp = 1 - (u2jT/total)) %>% data.frame()
# Adding a variable for right censoring for unemployed (will be used for MLE)
bhps[which(bhps$e1==0 & bhps$u2j==1), "right_censor"] <- 0

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
names(dur_wages) <- c("Wage (Bins)", "Quartiles", "Avg. Spell Duration")

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

## # First transition for employed (v - will be used for MLE)
## bhps$v <- NA
## bhps[which(bhps$e1==1), "v"] <- 0
## bhps[which(bhps$e1==1 & bhps$j2u==1), "v"] <- 1

# Starting the MLE estimation
# Writing the CDF as a function of kappa_1
# Page 327 (Bontemps, Robin, Van Den Berg - IER (2000))
F <- function(w, theta){
    lambda0 <- theta[1]
    lambda1 <- theta[2]
    delta <- theta[3]
    kappa <- lambda1/delta
    F <- (1+kappa)*Gcdf(w)/(1+kappa*Gcdf(w))
    return(F)
}
# Writing the PDF of F as a function of kappa_1
# Page 328 (Bontemps, Robin, Van Den Berg - IER (2000))
f <- function(w, theta){
    lambda0 <- theta[1]
    lambda1 <- theta[2]
    delta <- theta[3]
    kappa <- lambda1/delta
    f <- (1+kappa)*Gdensity(w)/((1+kappa*Gcdf(w))^2)
    return(f)
}

# Likelihood for unemployed people
L0 <- function(theta, cu, du, xu){
    lambda0 <- theta[1]
    lambda1 <- theta[2]                 
    delta <- theta[3]
    scale <- lambda0/(lambda0+delta)
    dens_spell_dur <- (1-cu)*log(lambda0 * exp(-lambda0*du))
    dens_accept_job <- (1-cu)*log(f(xu, theta))
    logLL_unemp <- scale*sum(dens_spell_dur+dens_accept_job, na.rm=TRUE)
    return(-logLL_unemp)
}

cu <- bhps[which(bhps$e1==0), "right_censor"]
du <- bhps[which(bhps$e1==0), "spelldur1"]
xu <- bhps[which(bhps$e1==0), "logw2"]
eu <- bhps[which(bhps$e1==0), "e1"]

L1 <- function(theta, ce, de, j2j, j2u, xe){
    lambda0 <- theta[1]
    lambda1 <- theta[2]
    delta <- theta[3]
    scale <- delta/(lambda0+delta)
    # Density of job value
    dens_job_val <- log(Gdensity(xe))
    # Density of spell duration
    dens_spell_dur <- (1-ce)*log(delta+lambda1*(1-F(xe, theta)))-
        (delta+lambda1*(1-F(xe, theta)))*de
    # Conditional probability of cause of job spell termination
    prob_job_trans <- is.finite(j2u*log(delta/(delta+lambda1*(1-F(xe, theta)))))+ is.finite(j2j*log((lambda1*(1-F(xe, theta)))/(delta+ lambda1*(1-F(xe, theta))))) 
    logLL_emp <- scale*sum(dens_job_val+dens_spell_dur+prob_job_trans,
                     na.rm=TRUE)
    return(-logLL_emp)
}

ce <- bhps[which(bhps$e1==1), "right_censor"]
de <- bhps[which(bhps$e1==1), "spelldur1"]
j2j <- bhps[which(bhps$e1==1), "j2j"]
j2u <- bhps[which(bhps$e1==1), "j2u"]
xe <- bhps[which(bhps$e1==1), "logw1"]
ee <- bhps[which(bhps$e1==1), "e1"]
    
LLfull <- function(theta, cu, du, xu, ce, de, j2j, j2u, xe){
    lambda0 <- theta[1]
    lambda1 <- theta[2]
    delta <- theta[3]
    full_logLL <- L0(theta=theta, cu=cu, du=du,
                                               xu=xu)+
        L1(theta=theta, ce=ce, de=de, j2j=j2j,
                                   j2u=j2u, xe=xe)
    
    return(full_logLL)
}

mle.dat <- optim(par=c(.1,.2,.3), fn=LLfull, cu=cu, du=du, xu=xu,
      ce=ce, de=de, j2j=j2j, j2u=j2u, xe=xe, method="L-BFGS-B",
      lower=c(rep(0.01,3)), upper=1, hessian=TRUE)

# Getting the standard errors
se.estimates <- sqrt(diag(mle.dat$hessian^(-1)))


# Simulated data
sim.dat <- read.csv("../data/bm_data_simulated.csv")
