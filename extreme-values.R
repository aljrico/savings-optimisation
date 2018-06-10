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

all_data <- generate_all_data(alpha = alpha,
						 sigma = sigma,
						 a = a,
						 years = years,
						 nsim = nsim,
						 pi = pi,
						 K = K,
						 A = 0.5,
						 include.mortality = TRUE)


# GPD ---------------------------------------------------------------------

data <- all_data %>%
	filter(model == "cppi-mort")
# We first try to guess some threshold in order to define the tail.
threshold <- 0
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

# We try to fit a General Pareto Distribution (GPD) to those points.
fit.gpd <- gpd(y, 0)$par.ests
grid <- seq(from  = min(freq_tail$mids), to = max(freq_tail$mids), by = 0.1)
lines(grid, dgpd(grid, xi = fit.gpd[["xi"]], beta = fit.gpd[["beta"]]))


# Now we'll try to do the same but using the functions from the 'ercv' library. Which does this automatically
fit.gpd2 <- fitpot(y, threshold = 0)
ccdfplot(y, fit.gpd2)
Tm(y,threshold=0,evi=0) # Small p-values state that this is NOT a GPD.

# As it is obvious, threshold = 0 is not a wise option. In the plot we may notice that the general behaviour is well defined at the begginning,
# But it eventually deviates.

# Maybe we ought to change the threshold.
fit.gpd2 <- fitpot(y, threshold=30)
ccdfplot(y, fit.gpd2,log="y")
Tm(y,threshold=30,evi=0)

# The result makes a lot more sense for even more extreme values, with this new threshold.

# Fortunaletly, we have a function that automatically seeks this optimal threshold, instead of having to guess it.
auto.thresh <- thrselect(y, evi = 0)
fit.gpd3 <- fitpot(y, threshold = auto.thresh$solution[["threshold"]])
ccdfplot(y, fit.gpd3)
Tm(y,threshold=auto.thresh$solution[["threshold"]],evi=0)


# Actually, we can notice that the auto selected threshold marks the point from where the distribution enters the rang of the coefficient of variation.
cvplot(y)



new.threshold <- auto.thresh$solution[["threshold"]]



# Exponential -------------------------------------------------------------

tail <- y[y<new.threshold]
# Fit Theorietical Distribution
exp.fit <- fitdistr(tail, densfun= "exponential")
theo <- rexp(n = length(tail), rate = exp.fit$estimate[["rate"]])

# Plot theoretical fit and  empirical data together
data.frame(emp = tail, theo) %>%
	melt() %>%
	ggplot(aes(x = value, fill = variable)) +
	geom_histogram(alpha = 0.75, position = "identity") +
	theme_minimal()

qqplot(y = tail, x = theo)
ks.test(tail, "pexp", exp.fit$estimate)

########################

tail <- y[y>new.threshold]
# Fit Theoretical Distribution
exp.fit <- fitdistr(tail, densfun= "exponential")
theo <- rexp(n = length(tail), rate = exp.fit$estimate[["rate"]])

# Plot theoretical fit and  empirical data together
data.frame(emp = tail, theo) %>%
	melt() %>%
	ggplot(aes(x = value, fill = variable)) +
	geom_histogram(alpha = 0.75, position = "identity") +
	theme_minimal()

qqplot(y = tail, x = theo)
ks.test(tail, "pexp", exp.fit$estimate)

##############################

tail <- y
# Fit Theorietical Distribution
exp.fit <- fitdistr(tail, densfun= "exponential")
theo <- rexp(n = length(tail), rate = exp.fit$estimate[["rate"]])

# Plot theoretical fit and  empirical data together
data.frame(emp = tail, theo) %>%
	melt() %>%
	ggplot(aes(x = value, fill = variable)) +
	geom_histogram(alpha = 0.75, position = "identity") +
	theme_minimal()

qqplot(y = tail, x = theo)
ks.test(tail, "pexp", exp.fit$estimate)



# Weibull -----------------------------------------------------------------

tail <- y[y<new.threshold]
# Fit Theorietical Distribution
weibull.fit <- fitdistr(tail, densfun= "weibull")
theo <- rweibull(n = length(tail), shape = weibull.fit$estimate[["shape"]], scale = weibull.fit$estimate[["scale"]])

# Plot theoretical fit and  empirical data together
data.frame(emp = tail, theo) %>%
	melt() %>%
	ggplot(aes(x = value, fill = variable)) +
	geom_histogram(alpha = 0.75, position = "identity") +
	theme_minimal()

qqplot(y = tail, x = theo)

ks.test(tail, "pweibull", weibull.fit$estimate)

#######################

tail <- y[y>new.threshold]
# Fit Theorietical Distribution
weibull.fit <- fitdistr(tail, densfun= "weibull")
theo <- rweibull(n = length(tail), shape = weibull.fit$estimate[["shape"]], scale = weibull.fit$estimate[["scale"]])

# Plot theoretical fit and  empirical data together
data.frame(emp = tail, theo) %>%
	melt() %>%
	ggplot(aes(x = value, fill = variable)) +
	geom_histogram(alpha = 0.75, position = "identity") +
	theme_minimal()

qqplot(y = tail, x = theo)

ks.test(tail, "pweibull", weibull.fit$estimate)


############################

tail <- y
# Fit Theorietical Distribution
weibull.fit <- fitdistr(tail, densfun= "weibull")
theo <- rweibull(n = length(tail), shape = weibull.fit$estimate[["shape"]], scale = weibull.fit$estimate[["scale"]])

# Plot theoretical fit and  empirical data together
data.frame(emp = tail, theo) %>%
	melt() %>%
	ggplot(aes(x = value, fill = variable)) +
	geom_histogram(alpha = 0.75, position = "identity") +
	theme_minimal()

qqplot(y = tail, x = theo)

ks.test(tail, "pweibull", weibull.fit$estimate)



# Gamma -------------------------------------------------------------------

tail <- y[y<new.threshold]
# Fit Theoretical Distribution
gamma.fit <- 	fitdistr(tail, densfun= "gamma")
theo <- rgamma(n = length(tail), shape = gamma.fit$estimate[["shape"]], rate = gamma.fit$estimate[["rate"]])

# Plot theoretical fit and  empirical data together
data.frame(emp = tail, theo) %>%
	melt() %>%
	ggplot(aes(x = value, fill = variable)) +
	geom_histogram(alpha = 0.75, position = "identity") +
	theme_minimal()

qqplot(y = tail, x = theo)

ks.test(tail, "pgamma", weibull.fit$estimate)


####################################

tail <- y[y>new.threshold]
# Fit Theoretical Distribution
gamma.fit <- 	fitdistr(tail, densfun= "gamma")
theo <- rgamma(n = length(tail), shape = gamma.fit$estimate[["shape"]], rate = gamma.fit$estimate[["rate"]])

# Plot theoretical fit and  empirical data together
data.frame(emp = tail, theo) %>%
	melt() %>%
	ggplot(aes(x = value, fill = variable)) +
	geom_histogram(alpha = 0.75, position = "identity") +
	theme_minimal()

qqplot(y = tail, x = theo)

ks.test(tail, "pgamma", weibull.fit$estimate)

####################################

tail <- y
# Fit Theoretical Distribution
gamma.fit <- 	fitdistr(tail, densfun= "gamma")
theo <- rgamma(n = length(tail), shape = gamma.fit$estimate[["shape"]], rate = gamma.fit$estimate[["rate"]])

# Plot theoretical fit and  empirical data together
data.frame(emp = tail, theo) %>%
	melt() %>%
	ggplot(aes(x = value, fill = variable)) +
	geom_histogram(alpha = 0.75, position = "identity") +
	theme_minimal()

qqplot(y = tail, x = theo)

ks.test(tail, "pgamma", weibull.fit$estimate)


