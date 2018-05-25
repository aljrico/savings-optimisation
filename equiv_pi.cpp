// Computing of Equivalent Pi'
// [[Rcpp::export]]
double equiv_pi_c(int m, double ret, int nsim, float alpha, float sigma, float a, int years, float pi)
{
	int i;
	float seed_pi;
	float max_pi;
	float min_pi;
	float est_ret;
	float cppi_median;
	float a, b, b_minus;
	int n;

	n = m;

	NumericVector cppi_res(nsim);

	seed_pi = 0.5;
	max_pi = 1;
	min_pi = 0;

	for(i=0;i<n;i++){
		cppi_res = cppi_c(alpha = alpha, sigma = sigma, a = a, years = years, nsim = nsim, pi = pi);
		std::sort(std::begin(cppi_res), std::end(cppi_res));
		cppi_median = cppi_res[nsim/2];
		est_ret = (1/years)*(-1 + pow(1 + (8*(cppi_median))/(a*years), 0.5)*100);

		if(est_ret < ret){
			min_pi = pi;
			pi = (pi + max_pi)/2;
		}

		if(est_ret > ret){
			max_pi = pi;
			pi = (pi + min_pi)/2;
		}

		if(est_ret == ret){break;}

		b =  pow(est_ret-ret,2);
		b_minus =  pow(ret,2);
		a = (100*b) / b_minus;
		if( a< 1){break;}
	}
	return pi;
}


