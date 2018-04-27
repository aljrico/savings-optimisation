
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(data.table)
library(viridis)
library(MASS)
library(rPython)

# Initial Parameters ------------------------------------------------------


alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
a <- 10 # Factor 'a'
years <- 60 # Total time
nsim <- 100 # Number of simulations
pi <- 0.1 # Constant proportion for risky investment
K <- 42

python.load("loop.py")
python.call("cppi_simple", alpha = alpha, sigma = sigma, a = a, years = years, nsim = nsim, pi = pi, K = K)
