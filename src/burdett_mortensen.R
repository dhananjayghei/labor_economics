# Including the functions file
source("functions.R")
# Loading in the required libraries
library(dplyr)
library(xtable)
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
genxtable(xtable(unemp_sex, align="llrrr", digits=c(rep(0,4),2)), basename="unemp_sex", include.rownames=FALSE)
# Sample unemployment rate by education
unemp_educ <- bhps %>%
    group_by(educ) %>%
    summarise(unemp = length(e1[which(e1==0)]),
              total = length(e1),
              unemp_rate = round(unemp*100/nrow(.),3)) %>% data.frame()
unemp_educ$educ <- c("less than A-levels", "A-levels", "Some Higher Education")
colnames(unemp_educ) <- c("Education", "Unemployed", "Total", "Unemployment Rate")
genxtable(xtable(unemp_educ, align="llrrr", digits=c(rep(0,4),2)), basename="unemp_educ", include.rownames=FALSE)
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
colnames(dur_wages) <- c("wage", "qtls", "avg_spell")

pdf(file="../doc/pics/duration_wages.pdf", width=5.4, height=3.8)
par(mai=c(.8,.8,.3,.2))
plot(dur_wages$wage, dur_wages$avg_spell, main="", xlab="Log(wages)", ylab="Mean Spell Duration", col="black",
     type="l", lwd=2)
dev.off()

names(dur_wages) <- c("Wage (Bins)", "Quartiles", "Avg. Spell Duration")
dur_wages <- dur_wages[, -1]
genxtable(xtable(dur_wages, align="lrr", digits=c(0,0,3)), basename="duration_wages", include.rownames=FALSE)

# Construct the CDF F (Fcdf is also a function)
Fcdf <- ecdf(bhps[which(bhps$e1==0 & bhps$u2j==1), "logw2"])
# Constructing the estimate of f
Fdens <- density(bhps[which(bhps$e1==0 & bhps$u2j==1), "logw2"], na.rm=TRUE)
# Constructing the density function using linear interpolation
Fdensity <- approxfun(Fdens)

# Plot G vs F
pdf(file="../doc/pics/FG_data.pdf", width=5.4, height=3.8)
par(mai=c(.8,.8,.3,.2))
plot(Gcdf, main="", xlab="Log(wages)", ylab="Cumulative Distribution Function", col="black")
plot.stepfun(Fcdf, do.points=FALSE, add=TRUE, col="gray45")
legend("topleft", legend=c("Empirical G", "Empirical F"),
       lty=rep(1,2), lwd=rep(1,2), col=c("black", "gray45"),
       bty="n")
dev.off()

# Plot g vs f
pdf(file="../doc/pics/FG_density.pdf", width=5.4, height=3.8)
par(mai=c(.8,.8,.3,.2))
plot(Gdens, main="", xlab="Log(wages)", ylab="Density", col="black", xlim=c(min(Fdens$x), max(Fdens$x)))
lines(Fdens, col="gray45")
legend("topleft", legend=c("Empirical g", "Empirical f"), lty=rep(1,2), lwd=rep(1,2), col=c("black", "gray45"),
       bty="n")
dev.off()

# Estimating kappa_1 non-parametrically
kappa1 <- function(x){
    kappa1 <- (Fcdf(x)-Gcdf(x))/(Gcdf(x)*(1-Fcdf(x)))
    return(kappa1)
}

# Adding the Gcdf function
bhps$G <- Gcdf(bhps$logw1)
bhps$G_qtls <- cut(bhps$G, quantile(bhps$G, probs=seq(0,1,1/25), na.rm=TRUE))

G_qtls <- as.numeric(unlist(strsplit(unlist(strsplit(names(table(bhps$G_qtls)), split="[,]"))[seq(2,50,by=2)], split="]")))

# Split by quartiles of G
bhpsG <- split(bhps, bhps$G_qtls)
kappa1_np <- do.call(rbind, lapply(bhpsG, function(x){
    k <- kappa1(x$logw1)
    avgK <- mean(k, na.rm=TRUE)
    return(avgK)
}))
kappa1_np <- data.frame(kappa1_np)
kappa1_np$G_qtls <- G_qtls
kappa1_np <- kappa1_np[is.finite(kappa1_np$kappa1_np), ]

pdf(file="../doc/pics/kappa1_np.pdf", width=5.4, height=3.8)
par(mai=c(.8,.8,.3,.2))
plot(kappa1_np[, 2], kappa1_np[,1], type="l", main="", xlab="G", ylab=expression(kappa[1]), xlim=c(0,1))
abline(h=mean(kappa1_np[,1]), lty=2)
dev.off()

wages1 <- unique(bhps$logw1)
kappa_1 <- do.call(rbind, lapply(wages1, kappa1))
kappa_1 <- kappa_1[is.finite(kappa_1)]

# Starting the MLE estimation
# Storing the data set for MLE
dat <- bhps[, c("e1", "spelldur1", "logw1", "j2u", "j2j",
                "logw2", "right_censor")]

# Maximum Likelihood Estimation
mle.dat <- optim(par=c(1,2,3), full_likelihood, dat=dat, method="L-BFGS-B",
                 lower=c(0.01,.01,.01), upper=c(1,1,1), hessian=TRUE)
# Get the standard errors
# Instead of bootstrapping, I am using the inverse of Hessian (Fisher information matrix)
# to compute the asymptotic standard errors. This gives a lower bound on the variance.
se.estimates <- sqrt(diag(mle.dat$hessian^(-1)))

# Sanity checks for the model - Estimating the coefficients/ distribution function
# Calculate the unemployment rate from the estimates
unemp_est <- mle.dat$par[3]/(mle.dat$par[3]+mle.dat$par[1])

# Estimate F from kappa_1
pdf(file="../doc/pics/estF_data.pdf", width=5.4, height=3.8)
par(mai=c(.8,.8,.3,.2))
plot(sort(dat$logw1), sort(F(dat$logw1, theta=mle.dat$par)), type="l",
     main="", xlab="Log(wages)", ylab="Cumulative Distribution Function", col="black")
plot.stepfun(Fcdf, add=TRUE, do.points=FALSE, col="gray45")
legend("topleft", legend=c("Predicted F (using MLE)", "Empirical F"),
       lty=rep(1,2), lwd=rep(1,2), col=c("black", "gray45"), bty="n")
dev.off()


# Construct the distribution of firm productivity
datPlot <- bhps[, c("e1", "spelldur1", "logw1", "right_censor", "G")]
productivity <- function(w, kappa){
    p <- w + ((1+kappa*Gcdf(w))/(2*kappa*Gdensity(w))) # Productivity
    profit <- (p-w)/p # Monopsony power/ Profit rate/ Profit share
    return(data.frame(prod=p, profit=profit))
}

firm.prod <- data.frame(do.call(rbind, lapply(dat$logw1, function(x) productivity(x, kappa1_sim))))
colnames(firm.prod) <- c("firm_prod", "firm_profit_rate")
dat$firm_prod <- firm.prod$firm_prod
dat$firm_profit_rate <- firm.prod$firm_profit_rate

datPlot <- dat[complete.cases(dat$logw1, dat$firm_prod), ]

pdf(file="../doc/pics/prod_wages.pdf", width=5.4, height=3.8)
par(mai=c(.8,.8,.3,.2))
qqplot(y=datPlot$logw1, x=datPlot$firm_prod, type="l", main="", xlab="Log(productivity)", ylab="Log(wages)",
       lwd=2)
lapply(prod_percentiles, function(x){
    abline(v=x, lty=2)
})
dev.off()

prod_percentiles <- quantile(datPlot$firm_prod, probs=c(.05, .25, .5, .75))
pdf(file="../doc/pics/prod_profit.pdf", width=5.4, height=3.8)
par(mai=c(.8,.8,.3,.2))
qqplot(x=datPlot$firm_prod, y=datPlot$firm_profit_rate, main="", xlab="Log(productivity)",
       ylab="Profit rate", type="l", lwd=2)
lapply(prod_percentiles, function(x){
    abline(v=x, lty=2)
})
dev.off()

# ---------------------------------- Simulated data
sim.dat <- read.csv("../data/bm_data_simulated.csv")
colnames(sim.dat)[1] <- "e1"

# Generating the right censor variable
sim.dat$right_censor <- 1
sim.dat[which(sim.dat$e1==1 & (sim.dat$j2j==1 | sim.dat$j2u==1)), "right_censor"] <- 0
sim.dat[which(sim.dat$e1==0 & sim.dat$u2j==1), "right_censor"] <- 0

# Changing the log wages for unemployed to NA
sim.dat[which(sim.dat$e1==0), "logw1"] <- NA
# Generating the Empirical CDF function
Gsim <- ecdf(sim.dat$logw1)
# Generating the PDF
Gdens <- density(sim.dat$logw1, na.rm=TRUE)
Gdensity <- approxfun(Gdens)
Gcdf <- Gsim
likelihood_unemp_sim <- function(theta, dat){
    x <- dat[which(dat$e1==0), ]
    lambda0 <- theta[1]
    lambda1 <- theta[2]
    delta <- theta[3]
    scale <- log(delta/(lambda0+delta))
    dens_spell_dur <- log(lambda0^(1-x$right_censor) *
                          exp(-lambda0*x$spelldur1))
    # Integrating out the values for unobserved wage 2 (Slide 14 - Topic 2 from Jeremy's slides)
#    dens_accept_job <- (1-x$right_censor)*log(f(x$logw2, theta))
    logLL_unemp <- sum(scale+dens_spell_dur, na.rm=TRUE)
    return(-logLL_unemp)
}

likelihood_sim <- function(theta, dat){
    lambda0 <- theta[1]
    lambda1 <- theta[2]
    delta <- theta[3]
    full_logLL <- likelihood_unemp_sim(theta=theta, dat=dat) +
        likelihood_emp(theta=theta, dat=dat)
    return(full_logLL)    
}

# Maximum Likelihood Estimation on the simulated data
mle.sim <- optim(par=c(.1,.05,.01), likelihood_sim, dat=sim.dat, method="L-BFGS-B",
                 lower=c(0.001,.001,.001), upper=c(1,1,1), hessian=TRUE)
se.estimates.sim <- sqrt(diag(mle.sim$hessian^(-1)))

# Calculate the unemployment rate from the estimates
unemp_est_sim <- mle.sim$par[3]/(mle.sim$par[3]+mle.sim$par[1])
# Unemployment rate in the sample
unemp_sim <- data.frame(prop.table(table(sim.dat$e1)))
# Estimating kappa_1 (The true kappa1 = 5)
kappa1_sim <- mle.sim$par[2]/mle.sim$par[3]


# Plot g vs f for simulated data
pdf(file="../doc/pics/FG_density_sim.pdf", width=5.4, height=3.8)
par(mai=c(.8,.8,.3,.2))
plot(Gdens, main="", xlab="Log(wages)", ylab="Density", col="black", xlim=c(min(Fdens$x), max(Gdens$x)))
lines(Fdens, col="gray45")
legend("topleft", legend=c("Empirical g", "Empirical f"), lty=rep(1,2), lwd=rep(1,2), col=c("black", "gray45"),
       bty="n")
dev.off()

# Construct the distribution of firm productivity
productivity <- function(w, kappa){
    p <- w + ((1+kappa*Gcdf(w))/(2*kappa*Gdensity(w)))
    return(p)
}

firm.prod <- data.frame(do.call(rbind, lapply(sim.dat$logw1, function(x) productivity(x, kappa1_sim))))
colnames(firm.prod) <- "firm_prod"
sim.dat$firm.prod <- firm.prod$firm_prod

sim.datPlot <- sim.dat[complete.cases(sim.dat$logw1, sim.dat$firm.prod), ]

qqplot(y=sim.datPlot$logw1, x=sim.datPlot$firm.prod, type="l")#, main="", xlab="Log(wages)", ylab="Log(productivity)")

