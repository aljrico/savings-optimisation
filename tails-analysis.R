
# Libraries and Functions -------------------------------------------------

source("functions.R")
library(tidyverse)
library(data.table)
library(viridis)
library(MASS)
library(evir)

library(Rcpp)
sourceCpp("cppi.cpp")



# Initial Parameters ------------------------------------------------------


alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
a <- 10 # Factor 'a'
years <- 60 # Total time
nsim <- 1e6 # Number of simulations
pi <- 0.1 # Constant proportion for risky investment
K <- 70
A <- 0.5

# CPPI simple --------------------------------------------------------------------
X_T <- cppi_c(pi = pi,
						nsim = nsim,
						alpha = alpha,
						sigma = sigma,
						a = a,
						years = years)

final_wealth <- as_tibble(as.data.frame(X_T))
final_wealth$model <- "cppi-simple"
final_wealth$ret <- (1/60)*(-1 + (1 + (8*(X_T))/(a*60))^(1/2))*100


# Alternative simple ------------------------------------------------------
X_T <- alt_c(K = K,
						nsim = nsim,
						alpha = alpha,
						sigma = sigma,
						a = a,
						years = years,
						A_factor = A)

df <- as_tibble(as.data.frame(X_T))
df$model <- "alt-simple"
df$ret <- (1/60)*(-1 + (1 + (8*(X_T))/(a*60))^(1/2))*100
final_wealth <- rbind(final_wealth,df)


# CPPI | Mortality --------------------------------------------------------
X_T <- cppi_mortality(pi = pi,
						nsim = nsim,
						alpha = alpha,
						sigma = sigma,
						a = a,
						years = years)[-c(1,2)]
df <- as_tibble(as.data.frame(X_T))
df$model <- "cppi-mort"
df$ret <- (1/60)*(-1 + (1 + (8*(X_T))/(a*60))^(1/2))*100
final_wealth <- rbind(final_wealth,df)

# Alternative | Mortality ------------------------------------------------------
X_T <- alt_mort(K = K,
							 nsim = nsim,
							 alpha = alpha,
							 sigma = sigma,
							 a = a,
							 years = years)[-c(1,2)]
df <- as_tibble(as.data.frame(X_T))
df$model <- "alt-mort"
df$ret <- (1/60)*(-1 + (1 + (8*(X_T))/(a*60))^(1/2))*100
final_wealth <- rbind(final_wealth,df)



# Plots without mortality ----------------------------------------------------------
#FW CPPI
final_wealth %>%
	filter(model %in% c("cppi-simple")) %>%
	ggplot() +
	geom_histogram(aes(x = X_T, fill = model,y = ..count../sum(..count..)),
							 alpha = 0.75,
							 bins = 70,
							 colour = "black",
							 position = "identity") +
	theme_bw() +
	xlab("Final Wealth") +
	ylab("") +
	scale_fill_viridis(discrete=TRUE, begin = 1) +
	theme(legend.position = "NONE") +
	scale_x_continuous(limits = c(-100,450))

#FW Alternatie
final_wealth %>%
	filter(model %in% c("alt-simple")) %>%
	ggplot() +
	geom_histogram(aes(x = X_T, fill = model,y = ..count../sum(..count..)),
								 alpha = 0.75,
								 bins = 70,
								 colour = "black",
								 position = "identity") +
	theme_bw() +
	xlab("Final Wealth") +
	ylab("") +
	scale_fill_viridis(discrete=TRUE, begin = 0) +
	theme(legend.position = "NONE") +
	scale_x_continuous(limits = c(-100,450))


#FW Both
final_wealth %>%
	filter(model %in% c("cppi-simple","alt-simple")) %>%
	ggplot() +
	geom_density(aes(x = X_T, fill = model),
								 alpha = 0.75,
								 position = "identity") +
	theme_bw() +
	xlab("Final Wealth") +
	ylab("") +
	scale_fill_viridis(discrete=TRUE) +
	# theme(legend.position = "NONE") +
	scale_x_continuous(limits = c(-100,500))

# Loss Both
final_wealth %>%
	filter(model %in% c("cppi-simple","alt-simple")) %>%
	filter(X_T <=0) %>%
ggplot() +
	geom_density(aes(x=X_T, fill = model), alpha = 0.75, position="identity", show.legend = FALSE) +
	xlab("Final Wealth") +
	ylab("Density") +
	theme_bw() +
	scale_fill_viridis(discrete=TRUE) +
	scale_x_continuous(limits = c(-40,0))




# Histogram
final_wealth %>%
	filter(model %in% c("cppi-simple", "alt-simple")) %>%
	filter(X_T <=10) %>%
	mutate(X_T = -X_T + 10) %>%
ggplot() +
	geom_histogram(aes(x = X_T, fill = model, y = (..count../sum(..count..))),
								 bins=200,
								 alpha = 0.75,
								 position = "identity") +
	theme_minimal() +
	scale_fill_viridis(discrete=TRUE)



# Plots with mortality ----------------------------------------------------

#FW CPPI-mort
final_wealth %>%
	filter(model %in% c("cppi-mort")) %>%
	ggplot() +
	geom_histogram(aes(x = X_T, fill = model,y = ..count../sum(..count..)),
								 alpha = 0.75,
								 bins = 70,
								 colour = "black",
								 position = "identity") +
	theme_bw() +
	xlab("Final Wealth") +
	ylab("") +
	scale_fill_viridis(discrete=TRUE, begin = 1) +
	theme(legend.position = "NONE") +
	scale_x_continuous(limits = c(-100,450))

#FW Alternatie-mort
final_wealth %>%
	filter(model %in% c("alt-mort")) %>%
	ggplot() +
	geom_histogram(aes(x = X_T, fill = model,y = ..count../sum(..count..)),
								 alpha = 0.75,
								 bins = 70,
								 colour = "black",
								 position = "identity") +
	theme_bw() +
	xlab("Final Wealth") +
	ylab("") +
	scale_fill_viridis(discrete=TRUE, begin = 0) +
	theme(legend.position = "NONE") +
	scale_x_continuous(limits = c(-100,450))


#FW Both
final_wealth %>%
	filter(model %in% c("cppi-mort","alt-mort")) %>%
	ggplot() +
	geom_density(aes(x = X_T, fill = model),
							 alpha = 0.75,
							 position = "identity") +
	theme_bw() +
	xlab("Final Wealth") +
	ylab("") +
	scale_fill_viridis(discrete=TRUE) +
	theme(legend.position = "NONE") +
	scale_x_continuous(limits = c(-100,500))

# Loss Both
final_wealth %>%
	filter(model %in% c("cppi-mort","alt-mort")) %>%
	filter(X_T <=0) %>%
	ggplot() +
	geom_density(aes(x=X_T, fill = model), alpha = 0.75, position="identity", show.legend = FALSE) +
	xlab("Final Wealth") +
	ylab("Density") +
	theme_bw() +
	scale_fill_viridis(discrete=TRUE) +
	scale_x_continuous(limits = c(-40,0))




# Histogram
final_wealth %>%
	filter(model %in% c("cppi-simple", "alt-simple")) %>%
	filter(X_T <=10) %>%
	mutate(X_T = -X_T + 10) %>%
	ggplot() +
	geom_histogram(aes(x = X_T, fill = model, y = (..count../sum(..count..))),
								 bins=200,
								 alpha = 0.75,
								 position = "identity") +
	theme_minimal() +
	scale_fill_viridis(discrete=TRUE)



# Tails -------------------------------------------------------------------
threshold <- 20


#CPPI simple
df <- final_wealth %>%
	filter(model %in% c("cppi-simple")) %>%
	filter(X_T <=-threshold) %>%
	mutate(X_T = X_T - threshold)

cppi_tail <- hist(log(-df$X_T[df$X_T<(-threshold)-threshold]), freq = FALSE)
cppi_tail <- hist((-df$X_T[df$X_T<(-threshold)-threshold]),breaks=exp(cppi_tail$breaks), freq = FALSE)
plot(cppi_tail$mids,cppi_tail$density,log="xy")
lin.model <- lm(log10(cppi_tail$density)~(cppi_tail$mids))


#Alt simple
df <- final_wealth %>%
	filter(model %in% c("alt-simple")) %>%
	filter(X_T <=-threshold) %>%
	mutate(X_T = X_T - threshold)

z <- hist(log(-df$X_T[df$X_T<(-threshold)-threshold]), freq = FALSE)
alt_tail <- hist((-df$X_T[df$X_T<(-threshold)-threshold]),breaks=exp(z$breaks), freq = FALSE)
plot(alt_tail$mids,alt_tail$density,log="xy")
lin.model <- lm(log10(alt_tail$density)~log10(alt_tail$mids))



tail <- data.frame(dens = alt_tail$density, loss = alt_tail$mids, model = "alt")
tail <- rbind(tail, data.frame(dens = cppi_tail$density, loss = cppi_tail$mids, model = "cppi"))
tail %>%
	ggplot(aes(x = loss, y = dens, colour = model)) +
	geom_point() +
	scale_y_log10() +
	geom_smooth(method='lm',formula=y~x, se = FALSE) +
	# scale_colour_viridis(discrete = TRUE) +
	theme_bw()

summary(lin.model)


# CPPI Mort
df <- final_wealth %>%
	filter(model %in% c("cppi-mort")) %>%
	filter(X_T <=-threshold) %>%
	mutate(X_T = X_T - threshold)

cppi_tail <- hist(log(-df$X_T[df$X_T<(-threshold)-threshold]), freq = FALSE)
cppi_tail <- hist((-df$X_T[df$X_T<(-threshold)-threshold]),breaks=exp(cppi_tail$breaks), freq = FALSE)
plot(cppi_tail$mids,cppi_tail$density,log="xy")
lin.model <- lm(log10(cppi_tail$density)~(cppi_tail$mids))


#Alt mort
df <- final_wealth %>%
	filter(model %in% c("alt-mort")) %>%
	filter(X_T <=-threshold) %>%
	mutate(X_T = X_T - threshold)

z <- hist(log(-df$X_T[df$X_T<(-threshold)-threshold]), freq = FALSE)
alt_tail <- hist((-df$X_T[df$X_T<(-threshold)-threshold]),breaks=exp(z$breaks), freq = FALSE)
plot(alt_tail$mids,alt_tail$density,log="xy")
lin.model <- lm(log10(alt_tail$density)~log10(alt_tail$mids))



tail <- data.frame(dens = alt_tail$density, loss = alt_tail$mids, model = "alt")
tail <- rbind(tail, data.frame(dens = cppi_tail$density, loss = cppi_tail$mids, model = "cppi"))
tail %>%
	ggplot(aes(x = loss, y = dens, colour = model)) +
	geom_jitter() +
	scale_y_log10() +
	geom_smooth(method='lm',formula=y~x, se = FALSE) +
	scale_colour_viridis(discrete = TRUE) +
	theme_bw()

summary(lin.model)



# Distributions -------------------------------------------------------------------

threshold <- 20


#CPPI simple
df <- final_wealth %>%
	filter(model %in% c("cppi-simple")) %>%
	filter(X_T <=-threshold) %>%
	mutate(X_T = X_T - threshold)
cppi_tail <- hist(log(-df$X_T[df$X_T<(-threshold)-threshold]), freq = FALSE)
cppi_tail <- hist((-df$X_T[df$X_T<(-threshold)-threshold]),breaks=exp(cppi_tail$breaks), freq = FALSE)

#Alt simple
df <- final_wealth %>%
	filter(model %in% c("alt-simple")) %>%
	filter(X_T <=-threshold) %>%
	mutate(X_T = X_T - threshold)
z <- hist(log(-df$X_T[df$X_T<(-threshold)-threshold]), freq = FALSE)
alt_tail <- hist((-df$X_T[df$X_T<(-threshold)-threshold]),breaks=exp(z$breaks), freq = FALSE)

# Binding
tail <- data.frame(dens = alt_tail$density, loss = alt_tail$mids, model = "alt")
tail <- rbind(tail, data.frame(dens = cppi_tail$density, loss = cppi_tail$mids, model = "cppi"))

# Testing Gamma
this_tail <- tail %>%
	filter(model == "cppi")

x <- this_tail$dens

gamma.fit <- 	fitdistr(y, densfun= "gamma")
sim_gamma <- rgamma(100000, shape = gamma.fit$estimate[["shape"]], rate = gamma.fit$estimate[["rate"]])

sim_gamma_density <- density(sim_gamma)

gamma_tail <- data.frame(loss = -sim_gamma_density$x, dens = sim_gamma_density$y, model = "gamma") %>%
	# filter(loss <=-threshold) %>%
	mutate(loss = -(loss - threshold))

rbind(gamma_tail, this_tail) %>%
	ggplot(aes(x = loss, y = dens, colour = model)) +
	geom_point() +
	theme_minimal()

# Testing Weibull
this_tail <- tail %>%
	filter(model == "cppi")

x <- this_tail$dens

weibull.fit <- 	fitdistr(x, densfun= "weibull")
sim_weibull <- rweibull(100000, shape = weibull.fit$estimate[["shape"]], scale = weibull.fit$estimate[["scale"]])

sim_weibull_density <- density(sim_weibull)

weibull_tail <- data.frame(loss = -den$x, dens = den$y, model = "weibull") %>%
	filter(loss <=-threshold) %>%
	mutate(loss = -(loss - threshold))

rbind(weibull_tail, this_tail) %>%
	ggplot(aes(x = loss, y = dens, colour = model)) +
	geom_point() +
	theme_minimal()




# GPD ---------------------------------------------------------------------

require(evir)

x<--final_wealth$X_T
u<-threshold
y <-x[x>u]-u
min(y)
plot(x = cppi_tail$mids, cppi_tail$density, log= 'y')
fit.gpd <- gpd(y, 0)$par.ests

lines(grid, dgpd(grid, xi=fit.gpd[["xi"]], beta = fit.gpd[["beta"]]))

require(ercv)
fit.gpd2 <- fitpot(y, threshold = 0)
ccdfplot(y, fit.gpd2)

require(ercv)
fit.gpd2 <- fitpot(y, threshold=27)

ccdfplot(y, fit.gpd2,log="y")
Tm(y,threshold=27,evi=0)
cvplot(y)
threslect(y, evi = 0)




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
