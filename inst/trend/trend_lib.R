#########################################################
## Function that perform the Mann-Kendall test
## To evaluate the p-values, it automatically identify if
## there is significant autocorrelation and perform bloc
## bootstrap if needed.
##
## IN
##   x: Vector of data
##   lag.max: Largest lag to verify
## OUT
##   tau: Statistic of the test.
##   pvalue: P-value of the test.
#########################################################
AutoMK <- function(x, lag.max = 5){

	x <- as.numeric(x)
	nx <- length(x)

  ## Compute the PACF and
  ## Find the number of significant lag
  pa.ci <- 1.96 / sqrt(nx)
  pa <- pacf(x, na.action = na.pass, plot = FALSE)$acf[1:lag.max]
  pa.lag <- sum(abs(pa) > pa.ci)

  if(pa.lag == 0){
    mk <- Kendall::MannKendall(x)
    tau <- as.numeric(mk$tau)
    pval <- as.numeric(mk$sl)


  } else {

    ## evaluate p-value using block bootstrap
    MKtau<-function(z) Kendall::MannKendall(z)$tau
    mk.boot <- boot::tsboot(an$value, MKtau, R=2000, l=pa.lag+1, sim="fixed")
    tau <- mk.boot$t0
    pval <- mean(abs(mk.boot$t) > mk.boot$t0)
  }

  return(c(tau = tau, pvalue = pval))

}

#########################################################
## Verify that there is no trend in the number of peaks
## Logistic regression is performed with different type
## of trends and compare to the constant model.
## The models are polynomial of order 1 to 3 and a
## natural spline with 4 df.
## The F-test based on the analysis of deviance.
## The funtion returns the smallest p-value of all trends.
##
## IN
##   x: Vector of covariate. Normally a time variable.
##   peaks: Indices of the peaks in x
## OUT
##   ans: P-value of the test
#########################################################

TrendLogis <- function(x, peaks){

  ## Trend in logistic regression
  xd <- data.frame(y = seq_along(x) %in% peaks, x = x)

  ## Constant model
  fit0 <- glm(y~1, xd, family = quasibinomial())

  ## alternative models
  fit <- vector('list', 4)
  fit[[1]] <- glm(y~x, xd, family = quasibinomial())
  fit[[2]] <- glm(y~poly(x,2), xd, family = quasibinomial())
  fit[[3]] <- glm(y~poly(x,3), xd, family = quasibinomial())
  fit[[4]] <- glm(y~ns(x,4), xd, family = quasibinomial())

  ## Evaluate p-values of the F-test
  fun <- function(z) anova(fit0, z, test = 'F')[2,6]

  ## Return minimal p-value
  return(min(sapply(fit,fun)))
}
