# Value-at-Risk

This repository contains solutions to several Value at Risk assignments 
from Boston University's Corporate Risk Management Course with Scott Robertson. 

In the second part of the assignmnet, we were asked to compute the Value at Risk for a Hedged Call Option portfolio. 
A hedged call option is a portfolio consisting of a call option and the Greek delta times the number of shares. 
In the Black Scholes framework, stock returns are log normally distributed so we can simulate the future stock price 
and calculate the new value of the portfolio, assuming the option price and stock price change but the original number of shares does not. 
We do this over a one day time period and a 10 day time period. 

We also back test the Value at Risk of the SP500 using 3 methods. 
We use an empirical method using the 95th quantile of the past 1000 observations. 
We use a GARCH 1,1 model to estimate the variance and, assuming returns are normally distributed, can calculate the 95th quantile of the normal distribution. 
We use a rolling variance where more wight is given to the historic variance and, assuming returns are normally distributed, can calculate the 95th quantile of the normal distribution. 
