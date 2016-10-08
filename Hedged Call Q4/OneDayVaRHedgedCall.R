# HW 2 Q4  Part 1

# Produce 1 day VaR for hedged call option 
library(RQuantLib)

# Option details 
S <- 56.47   # Initial underlying price
delta <- 1/250 # time increment 
maturity0 <- 0.376 # initial maturity 
mu <- .1689 
sigma <- .2066 

n <- 10000 # Number of simulations 

# Pre allocate space 
AllLosses <- rep(0,n)
rand0_1 <- rnorm(n,0,1) # Pre generate numbers from normal(0,1) distribution 

for (i in 1:n){
# Black Scholes Price and Greeks 
Option <- EuropeanOption(type = "call", underlying = S, strike = 55, dividendYield = 0, riskFreeRate = 0.0084, maturity = 0.376, volatility = .2066) 

# Position in stock (Option delta)
ht <- Option$delta

# Value of portfolio at time t
Vt <- S*ht - Option$value

# Stock Return is log normally distributed- use pre generated random number
X_t_delta <- delta* (mu-(sigma^2)/2)+ sigma* sqrt(delta)*rand0_1[i]

# New stock price- found using the stock return and previous return 
S_t_delta <- exp(X_t_delta+log(S))

# New portfolio value 
# New Option Price and Greeks - new stock price and maturity 
New_Option <- EuropeanOption(type = "call", underlying = S_t_delta, strike = 55, dividendYield = 0, riskFreeRate = 0.0084, maturity = (0.376-delta), volatility = .2066) 

# New portfolio value- same position (delta) but new option price and stock price 
Vt_delta <- ht*S_t_delta - New_Option$value

# Loss operator is difference in portfolio values 
L_t_delta <- Vt -Vt_delta

# Store losses 
AllLosses[i] <- L_t_delta
}

#View the distribution of the losses 
hist(AllLosses, breaks = 40)
abline(v = quantile(AllLosses, 0.95), col = "red")

# 95% VaR and 1day VaR * sqrt(10)
quantile(AllLosses, 0.95)
quantile(AllLosses, 0.95)*sqrt(10)
