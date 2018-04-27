import numpy as np

def cppi_simple(pi,nsim,alpha,sigma,a,years,K):
	f = np.repeat(a,round(years/2)).tolist()
	lg = np.repeat(-a,round(years/2)).tolist()
	f = f+lg

	final_wealth = []

	for j in range(0,nsim-1):
		x = []
		x = np.append(x,0)

		for i in range(0,years-1):
			rndm = np.random.normal(alpha, sigma, 1)
			x = np.append(x,x[i]*(1+rndm)*pi + (1-pi)*x[i] + f[i])

		final_wealth = np.append(final_wealth,x[years-1])
	return final_wealth
