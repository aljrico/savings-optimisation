library("quantmod")
library("nortest")
library("tseries")
library(tidyverse)

sp500 <- new.env()
getSymbols("^GSPC", env = sp500, src = "yahoo", from = as.Date("1950-01-01"), to = as.Date("2017-12-31"))
GSPC <- sp500$GSPC
plot(GSPC$GSPC.Adjusted, type ="h", main="S&P 500", ylim = c(0, max(GSPC$GSPC.Adjusted)))
lr <- diff(log(GSPC$GSPC.Adjusted))
hlr <- hist(lr, main = "Histogram for S&P daily logreturn", xlab="logreturn", ylab="normalized frequency", border="navy blue")


lr %>% as_tibble() %>%
	ggplot() +
	geom_histogram(aes(x = GSPC.Adjusted,y = ..count../sum(..count..)),
								 alpha = 0.75,
								 bins = 50,
								 fill = "blue",
								 colour = "black",
								 position = "identity") +
	theme_bw() +
	xlab("Daily Logreturns") +
	ylab("") +
	theme(legend.position = "NONE") +
	scale_x_continuous(limits = c(-0.1,0.1))

