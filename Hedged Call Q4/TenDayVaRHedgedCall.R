# HW 2 Q4 

# Produce 10 day VaR for hedged call option 
library(RQuantLib)

# Option details 
S <- 56.47   # Initial underlying price
delta <- 1/250 # time increment 
maturity0 <- 0.376 # initial maturity 
mu <- 0.1689 
sigma <- 0.2066 

n <- 10000 # Number of simulations 
nday <- 10

# Pre allocate space and generate random variables 
AllLosses <- rep(0,n)
rand0_1 <- rnorm(nday*n,0,1) # Generate random samples 

for (i in 1:n){ # For each simulaiton 
  
  Temp10DayLosses <- rep(0,nday)
  for(j in 1:nday){ #For each day in the 10 day 
    
  # Black Scholes Price and Greeks 
  Option <- EuropeanOption(type = "call", underlying = S, strike = 55, dividendYield = 0, riskFreeRate = 0.0084, maturity = 0.376-j*(delta), volatility = .2066) 
  
  # Position in stock (Option delta)
  ht <- Option$delta
  
  # Value of portfolio at time t
  Vt <- S*ht - Option$value
  
  # Stock Return (Is the sigma2 divided by 2 or the mu - sigma2)
  X_t_delta <- delta* (mu-(sigma^2)/2)+ sigma* sqrt(delta)*rand0_1[i*nday-j+1]
  
  # New stock price- found using the stock return and previous return 
  S_t_delta <- exp(X_t_delta+log(S))
  
  # New portfolio value 
  # New Option Price and Greeks - new stock price and maturity 
  New_Option <- EuropeanOption(type = "call", underlying = S_t_delta, strike = 55, dividendYield = 0, riskFreeRate = 0.0084, maturity = (0.376-(j+1)*delta), volatility = .2066) 
  
  # New portfolio value- same position (delta) but new option price and stock price 
  Vt_delta <- ht*S_t_delta - New_Option$value
  
  # Loss operator is difference in portfolio values 
  L_t_delta <- Vt -Vt_delta
  
  # Store temp losses 
  Temp10DayLosses[j] <- L_t_delta
  
  # Reset stock price 
  S <- S_t_delta
  } # End the 10 (nday) for loop 
  
  # Store Sum of 10 Day Losses 
  AllLosses[i] <- sum(Temp10DayLosses)
  # Reset Stock price to initial price for new 10 day simulation 
  S <- 56.47
  
}

# 95% VaR 
quantile(AllLosses, 0.95)

# Create histogram of the Losses with the 95% VaR 
hist(AllLosses, breaks = 40)
abline(v = quantile(AllLosses, 0.95), col = "red")

