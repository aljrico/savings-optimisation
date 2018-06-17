# Libraries and Functions -------------------------------------------------

source("functions.R")
library(tidyverse)
library(data.table)
library(Rcpp)
sourceCpp("cppi.cpp")

equiv_pi <- function(ret
				 , m = 1e2
				 , seed_pi = 0.5
				 , max_pi = 30
				 , min_pi = 0
				 , mortality = FALSE
				 , nsim = 1e4
				 , alpha = 0.0343
				 , sigma = 0.1544
				 , years = 60
				 , a = 10
){
	pi <- seed_pi
	for(i in 1:m){
		if(mortality == FALSE){
			est_ret <- cppi_c(alpha = alpha, sigma = sigma, years = years, pi = pi, a=a, nsim = nsim) %>% na.omit() %>% compute_return()
			while(is.na(est_ret)) est_ret <- cppi_c(alpha = alpha, sigma = sigma, years = years, pi = pi, a=a, nsim = nsim) %>% na.omit() %>% compute_return()
		}else{
			est_ret <- cppi_mort_fasto(pi = pi, nsim = 1e3) %>% na.omit() %>% compute_return()
			while(is.na(est_ret)){est_ret <- cppi_mort_fasto(pi = pi, nsim = 1e3) %>% na.omit() %>% compute_return()}
		}

		if(est_ret < ret){
			min_pi <- pi
			pi <- mean(c(pi, max_pi))
		}

		if(est_ret > ret){
			max_pi <- pi
			pi <- mean(c(pi, min_pi))
		}

		if(est_ret == ret) break

		if(100*abs(est_ret-ret)/abs(ret) < 1) break
	}

	return(pi)
}


equiv_pi2 <- function(ret
											, m = 1e2
											, seed_pi = 0.5
											, max_pi = 1
											, min_pi = 0
											, mortality = FALSE
											, nsim = 1e4
											, alpha = 0.0343
											, sigma = 0.1544
											, years = 60
											, a = 10){

	# Define secant function
	secant <- function(fun, x0, x1, tol=0.1, niter=1e2){
		for ( i in 1:niter ) {
			x2 <- x1-fun(x1)*(x1-x0)/(fun(x1)-fun(x0))
			if (abs(fun(x2)) < tol)
				return(x2)
			x0 <- x1
			x1 <- x2
		}
		return(x2)
	}

	if(mortality == FALSE){
		f <- function(x){cppi_c(alpha = 0.0343, sigma = 0.1544, years = 60, pi = x, a=10, nsim = 1e4) %>% na.omit() %>% compute_return() - ret}
	}else{
		min_pi <- 0.4
		f <- function(x){
			res <- -(cppi_mortality(pi = x, nsim = 1e2) %>% na.omit() %>% compute_return() - ret)
			while(is.na(res)){
				res <- -(cppi_mortality(pi = x, nsim = 1e2) %>% na.omit() %>% compute_return() - ret)
			}
			return(res)
		}
	}
	output <- secant(fun = f, x0 = min_pi, x1 = max_pi, niter = m)
	return(output)
}

