
# Table Results -----------------------------------------------------------


library(ggplot2)
library(dplyr)
source("functions.R")

factor <- -3.255
K <- c()
cppi_ret <- c()
pi_b <- c()
es <- c()
cppi_ret <- c()
montses_ret <- c()

for(i in 1:10){
	print(i)
	pi <- 0.1*i
	cppi_res<- cppi(pi)
	es[i] <- cppi_res[1]
	K[i] <- es[i]*factor
	cppi_ret[i] <- cppi_res[2]

	montses_res <- montses(K[i])
	montses_ret[i] <- montses_res[2]
	pi_b[i] <- montses_res[1]
}
