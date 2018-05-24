#include<stdlib.h>
#include<stdio.h>
#include<math.h>
#include<time.h>
#include<stdbool.h>
#include<string.h>
#include <Rcpp.h>

using namespace Rcpp;

// Sum function
double sum(NumericVector x) {
	int n = x.size();
	double total = 0;
	for(int i = 0; i < n; ++i) {
		total += x[i];
	}
	return total;
}

// Computing of 'pi' function
double fpi(double A, double K, double x, NumericVector f, int time, int years)
{
	NumericVector c(years);
	int i;
	float g, xpi;

	for(i=0;i<years;i++){c[i]=0;}
	for(i=time;i<years;i++){c[i]=f[i];}

	g = sum(f);

	xpi = A * (K + x + g);
	return xpi;
}

// [[Rcpp::export]]
double normal(double mu, double sigma)
	{
		double U1, U2, W, mult;
		static double X1, X2;
		static int call = 0;

		if (call == 1)
		{
			call = !call;
			return (mu + sigma * (double) X2);
		}

		do
		{
			U1 = -1 + ((double) rand () / RAND_MAX) * 2;
			U2 = -1 + ((double) rand () / RAND_MAX) * 2;
			W = pow (U1, 2) + pow (U2, 2);
		}
		while (W >= 1 || W == 0);

		mult = sqrt ((-2 * log (W)) / W);
		X1 = U1 * mult;
		X2 = U2 * mult;

		call = !call;

		return (mu + sigma * (double) X1);
	}

// [[Rcpp::export]]
NumericVector cppi_c(int nsim, float alpha, float sigma, float a, int years, float pi){

	NumericVector final_wealth(nsim);
	int i,j;
	float x_next,x_curr,rndm;
	NumericVector f(years);

	for(i=0; i<years/2; i++){f[i] = a;}
	for(i=years/2; i<years; i++){f[i] = -a;}

	for(j=0;j<nsim;j++)
	{
		x_curr=a;

		for(i=0;i<years-1;i++)
		{
			rndm = normal(alpha,sigma);
			x_next = x_curr * (1 + rndm) * pi + (1-pi) * x_curr + f[i+1];
			x_curr = x_next;
		}

		final_wealth[j] = x_next;
	}
	return final_wealth;
}

// [[Rcpp::export]]
NumericVector alt_c(float alpha, float sigma, float a, int years, int nsim, float K, float A_factor){

	NumericVector final_wealth(nsim);
	int i,j,time;
	float x_next,x_curr,rndm;
	NumericVector f(years);
	float pi;

	for(i=0; i<years/2; i++){f[i] = a;}
	for(i=years/2; i<years; i++){f[i] = -a;}

	for(j=0;j<nsim;j++)
	{
		x_curr=a;

		for(i=0;i<years-1;i++)
		{
			rndm = normal(alpha,sigma);
			time = i;
			pi = fpi(A_factor, K, x_curr, f, time, years)/x_curr;
			x_next = x_curr * (1 + rndm) * pi + (1-pi) * x_curr + f[i+1];
			x_curr = x_next;
		}

		final_wealth[j] = x_next;
	}
	return final_wealth;
}

