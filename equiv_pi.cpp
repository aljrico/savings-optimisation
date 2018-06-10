double return_c(NumericVector vec, float a, int years)
{
	float median;
	float est_ret;

	median = median_c(vec);
	est_ret = (1/years)*(-1 + sqrt(1 + (8*(median))/(a*years)))*100;
	return est_ret;
	}

