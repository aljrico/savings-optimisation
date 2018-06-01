library(tidyverse)
library(data.table)
library(viridis)
library(MASS)
library(evir)
library(ercv)
library(Rcpp)

source("functions.R")
sourceCpp("cppi.cpp")


# Create Data------------------------------------------------------


alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
a <- 10 # Factor 'a'
years <- 60 # Total time
nsim <- 1e6 # Number of simulations
pi <- 0.1 # Constant proportion for risky investment
K <- 42
A <- 0.5

data <- generate_all_data(alpha = alpha,
						 sigma = sigma,
						 a = a,
						 years = years,
						 nsim = nsim,
						 pi = pi,
						 K = K,
						 A = 0.5,
						 include.mortality = FALSE)


# GPD ---------------------------------------------------------------------

# We first try to guess some threshold in order to define the tail.
threshold <- 20
u <- threshold

# Vector of Losses
x <- -data$X_T

# Tail Definition
y  <- x[x>u]-u
min(y) # If everything is ok, it should be close to 0.

# We compute the frequency of the points, based on breaks.
log_hist <- hist(log(y), freq = FALSE)
freq_tail <- hist(y,
									# breaks=exp(log_hist$breaks),
									freq = FALSE)


# Points plot of that frequency, but with log(y)
plot(x = freq_tail$mids, freq_tail$density, log= 'y')

# We try to feet a General Pareto Distribution (GPD) to those points.
fit.gpd <- gpd(y, 0)$par.ests
grid <- seq(from  = min(freq_tail$mids), to = max(freq_tail$mids), by = 0.1)
lines(grid, dgpd(grid, xi = fit.gpd[["xi"]], beta = fit.gpd[["beta"]]))


# Now we'll try to do the same but using the functions from the 'ercv' library. Which does this automatically
fit.gpd2 <- fitpot(y, threshold = 0)
ccdfplot(y, fit.gpd2)

# As it is obvious, threshold = 0 is not a wise option. In the plot we may notice that the general behaviour utterly deviates from x=27.
# Maybe that ought to be the threshold.
fit.gpd2 <- fitpot(y, threshold=27)
ccdfplot(y, fit.gpd2,log="y")

# The result makes a lot more sense for even more extreme values.
Tm(y,threshold=27,evi=0)
cvplot(y)
threhselect(y, evi = 0)




tail_cppis <- final_wealth %>%
	filter(model == "cppi-simple") %>%
	arrange(ret)

tail_cppis <- head(tail_cppis,nrow(tail_cppis)/10)
data <- 1-tail_cppis$ret

# Exponential
exp.fit <- fitdistr(data, densfun= "exponential")
ks.test(data, "pexp", exp.fit$estimate)
qqPlot(rnorm(10000),"normal")


# Weibull
weibull.fit <- fitdistr(data, densfun= "weibull")
ks.test(data, "pweibull", weibull.fit$estimate)

qqplot(qweibull(ppoints(length(data)), shape = weibull.fit$estimate[1],
								scale = weibull.fit$estimate[2]), data)

