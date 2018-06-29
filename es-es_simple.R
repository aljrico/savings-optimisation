
# Libraries & Functions ---------------------------------------------------

source("functions.R")
source("estimate_equiv-pi.R")
sourceCpp("cppi.cpp")


alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
A <- 0.5
nsim <- 1e5
theta <- 0.95
years <- 60
m <- 1e2
max_A <- 25
a <- 10

es_cppi <- c()
pi_record <- c()
es_alt <- c()
A_record <- c()

as <- seq(from = 0.5, to =2, by = 0.1)
dat <- tibble()

for(j in 1:length(as)){
	pb <- progress_bar$new(total = (100))
	cat(paste0("... ", as[j], " ... \n"))
	for(i in 1:100){
		pb$tick()
		A_record[i] <- as[j]
		pi <- i/100
		pi_record[i] <- pi
		es_cppi[i] <- cppi_c(alpha = alpha, sigma = sigma, years = years, pi = pi, a=a, nsim = nsim) %>%
			ES()
		factor <- 1/(-1 + (1/(1 - theta))*exp(alpha*A*years)*pnorm(qnorm(1-theta)- A*sigma*sqrt(years)))
		K <- es_cppi[i]*factor

		# cat(paste0("... ", i, " ...\n"))

		es_alt[i] <- alt_c(alpha = alpha, sigma = sigma, years = years, a = a, K = K, nsim = nsim, A = A) %>%
			ES()

		while(is.na(es_alt[i])) {es_alt[i] <- alt_c(alpha = alpha, sigma = sigma, years = years, a = a, K = K, nsim = nsim, A = A) %>%
			ES()}

		df <- cbind(A_record, es_cppi, es_alt, pi_record)
	}
	dat <- rbind(dat,df)
}

dat %>%
	as_tibble() %>%
	dplyr::mutate(pi_record = ifelse(pi_record <= 0.1, 0.1, pi_record)) %>%
	dplyr::mutate(pi_record = ifelse(pi_record >= 0.9, 0.9, pi_record)) %>%
	dplyr::mutate(pi_record = (floor(pi_record*10)/10) %>% as.factor()) %>%
	ggplot() +
	geom_point(aes(x = es_cppi, y = (es_alt), colour = pi_record)) +
	xlab("ES (CPPI)") +
	ylab("ES (Alternative)") +
	geom_abline(intercept = 0, slope = 1) +
	scale_colour_viridis(discrete=TRUE, name = "Pi") +
	theme_bw() +
	theme(legend.position = "none")

dat %>%
	as_tibble() %>%
	ggplot() +
	geom_point(aes(x = pi_record, y = (es_alt-es_cppi), colour = (A_record))) +
	xlab("ES (CPPI)") +
	ylab("ES (Alternative)") +
	theme_bw()
