
# Libraries and Functions -------------------------------------------------

source("functions.R")
library(tidyverse)
library(data.table)
library(viridis)
library(MASS)



# Initial Parameters ------------------------------------------------------


alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
a <- 10 # Factor 'a'
years <- 60 # Total time
nsim <- 1e7 # Number of simulations
pi <- 0.1 # Constant proportion for risky investment
K <- 42

# CPPI simple --------------------------------------------------------------------
X_T <- cppi(pi = pi,
						nsim = nsim,
						alpha = alpha,
						sigma = sigma,
						a = a,
						years = years)[-c(1,2)]
final_wealth <- as_tibble(as.data.frame(X_T))
final_wealth$model <- "cppi-simple"
final_wealth$ret <- (1/60)*(-1 + (1 + (8*(X_T))/(a*60))^(1/2))*100


# Alternative simple ------------------------------------------------------
X_T <- montses(K = K,
						nsim = nsim,
						alpha = alpha,
						sigma = sigma,
						a = a,
						years = years)[-c(1,2)]
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


# Histogram
final_wealth %>%
	filter(model %in% c("cppi-simple", "alt-simple")) %>%
	filter(X_T <=10) %>%
	mutate(X_T = -X_T + 10) %>%
ggplot() +
	geom_histogram(aes(x = X_T, fill = model, y = (..count../sum(..count..))),
								 bins=200,
								 alpha = 0.45,
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
	geom_jitter() +
	scale_y_log10() +
	geom_smooth(method='lm',formula=y~x, se = FALSE) +
	scale_colour_viridis(discrete = TRUE) +
	theme_bw()

summary(lin.model)
# Lognormal


# Distributions -------------------------------------------------------------------

tail_cppis <- final_wealth %>%
	filter(model == "cppi-simple") %>%
	arrange(ret)

tail_cppis <- head(tail_cppis,nrow(tail_cppis)/10)
data <- 1-tail_cppis$ret

# Exponential
exp.fit <- fitdistr(data, densfun= "exponential")
ks.test(data, "pexp", exp.fit$estimate)
qqPlot(rnorm(10000),"normal")




# Gamma
gamma.fit <- fitdistr(data, densfun= "gamma")
ks.test(data, "pgamma", gamma.fit$estimate)

# Weibull
weibull.fit <- fitdistr(data, densfun= "weibull")
ks.test(data, "pweibull", weibull.fit$estimate)

qqplot(qweibull(ppoints(length(data)), shape = weibull.fit$estimate[1],
												scale = weibull.fit$estimate[2]), data)
