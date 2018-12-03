# Likelihood for unemployed people
likelihood_unemp <- function(theta, dat){
    x <- dat[which(dat$e1==0), ]
    lambda0 <- theta[1]
    lambda1 <- theta[2]
    delta <- theta[3]
    scale <- log(delta/(lambda0+delta))
    dens_spell_dur <- log(lambda0^(1-x$right_censor) *
                                             exp(-lambda0*x$spelldur1))
    dens_accept_job <- (1-x$right_censor)*log(f(x$logw2, theta))
    logLL_unemp <- sum(scale+dens_spell_dur+dens_accept_job, na.rm=TRUE)
    return(-logLL_unemp)
}

# Likelihood for employed people
likelihood_emp <- function(theta, dat){
    x <- dat[which(dat$e1==1), ]
    lambda0 <- theta[1]
    lambda1 <- theta[2]
    delta <- theta[3]
    scale <- log(lambda0/(lambda0+delta))
    # Density of job value
    dens_job_val <- log(Gdensity(x$logw1))
    # Density of spell duration
    dens_spell_dur <- (1-x$right_censor)*log(delta+lambda1*(1-F(x$logw1, theta)))-
        (delta+lambda1*(1-F(x$logw1, theta)))*x$spelldur1
    # Conditional probability of cause of job spell termination
    prob_job_trans <- log((delta/(delta+lambda1*(1-F(x$logw1, theta))))^x$j2u) + log(((lambda1*(1-F(x$logw1, theta)))/(delta+ lambda1*(1-F(x$logw1, theta))))^x$j2j)
    logLL_emp <- sum(scale+dens_job_val+dens_spell_dur+prob_job_trans,
                     na.rm=TRUE)
    return(-logLL_emp)
}

# Full Log Likelihood
full_likelihood <- function(theta, dat){
    lambda0 <- theta[1]
    lambda1 <- theta[2]
    delta <- theta[3]
    full_logLL <- likelihood_unemp(theta=theta, dat=dat) +
        likelihood_emp(theta=theta, dat=dat)
    return(full_logLL)    
}


genxtable <- function(x, basename, include.rownames=FALSE,...) {
  print(x,
        type="latex",
        file=paste("../doc/tables/",basename,".gen", sep=""),
        include.rownames=include.rownames,
        table.placement="tp",
        caption.placement="top",
        sanitize.text=function(x)x,
        latex.environments=c("center","scriptsize"),...)
  print(x,
        type="latex",
        file=paste("../doc/tables/",basename,"_float",".gen", sep=""),
        include.rownames=include.rownames,
        sanitize.text=function(x)x,
        latex.environments=c("center","scriptsize"),
        floating=FALSE)
}


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
