# Including the analysis file (burdett_mortensen.R)
source("burdett_mortensen.R")


# Plot for the empirical CDF
plot(Gcdf, main="", xlab="Log(wages)", ylab="Empirical CDF")


# Plot for the density of the empirical CDF
plot(Gdens, main="", xlab="", ylab="Density")
