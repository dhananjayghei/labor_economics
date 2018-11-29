# Including the analysis file (burdett_mortensen.R)
source("burdett_mortensen.R")


# Plot for the empirical CDF
plot(Gcdf, main="", xlab="Log(wages)", ylab="Empirical CDF")


# Plot for the density of the empirical CDF
plot(Gdens, main="", xlab="", ylab="Density")

# Plot the mean durations against the wage percentiles
plot(dur_wages$logw1_qtls, dur_wages$avg_spelldur1, type="l",
     main="", xlab="Log(wages) in Quantiles",
     ylab="Mean duration of initial spell", lwd=2)

# Plot of estimated F vs estimated G (CDF)
plot.stepfun(Fcdf, do.points=FALSE, main="", xlab="Log(wages)", ylab="Cumulative Distribution Function", col="black", lwd=2)
lines(Gcdf, col="gray50", lwd=2)
legend("topleft", legend=c("Predicted F", "Predicted G"), col=c("black", "gray50"), lty=rep(1,2), lwd=rep(2,2), bty="n")
