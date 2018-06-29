library(tidyverse)
library(data.table)
library(viridis)
library(Rcpp)
library(progress)

source("functions.R")
sourceCpp("cppi.cpp")

es_record <- c()
k_record <- seq(1, 1000, by = 1)
pi_record <- seq(0.5, 1, by = 0.1)
k_result <- c()
es_init <- c()
es_final <- c()
A <- 2
nsim <- 1e2


for(i in 1:length(pi_record)){
	es_init[i] <- cppi_mort_fasto(pi = pi_record[i], nsim =nsim) %>% na.omit() %>% ES()
	for(j in 1:length(k_record)) {es_record[j] <- alt_mort_fasto(K = k_record[j], nsim = nsim, A = A) %>% na.omit() %>% ES(); cat(paste0("... ",j , "... \n"))}
	k_result[i] <- which.min((es_record - es_init[i])^2)
	es_final[i] <- es_record[k_result[i]]
}

k_result
es_final
es_init


es_to_k <- function(A = 0.5, pi = 0.7, nsim = 1e2, err = 0.01){
	k_result <- c()
	es_init <- c()
	es_final <- c()
	es_record <- c()
	k_record <- seq(1, 1000, by = 1)
	pi_record <- pi

	pb <- progress_bar$new(total = length(k_record))

	for(i in 1:length(pi_record)){
		es_init[i] <- cppi_mort_fasto(pi = pi_record[i], nsim =nsim) %>% na.omit() %>% ES()

		for(j in 1:length(k_record)) {
			pb$tick()
			es_record[j] <- alt_mort_fasto(K = k_record[j], nsim = nsim, A = A) %>% na.omit() %>% ES()
		}

		k_result[i] <- mean(which(abs((es_record - es_init[i])/(es_init[i])) < err))
		es_final[i] <- es_record[k_result[i]]
	}
	return(k_result)
}

es_to_k(nsim = 1e3, err = 0.1)
