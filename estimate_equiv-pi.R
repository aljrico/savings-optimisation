# Libraries and Functions -------------------------------------------------

source("functions.R")
library(tidyverse)
library(data.table)

equiv_pi <- function(ret
				 , m = 1e2
				 , seed_pi = 0.5
				 , max_pi = 1
				 , min_pi = 0
				 , mortality = FALSE
){
	pi <- seed_pi
	for(i in 1:m){
		if(mortality == FALSE){
			est_ret <- cppi(pi = pi, nsim = 1e3)[[2]]
		}else{
			est_ret <- cppi_mortality(pi = pi, nsim = 1e3)[[2]]
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
