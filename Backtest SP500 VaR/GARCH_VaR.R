# HW2Q5Pt1

# This file calculates the 95% VaR using a GARCH(1,1) method with EWMA means of the SP 500.
# We use a 4 year rolling window horizon to calibrate the parameters. 
# We then back test the theoretical VaR against the next prediction of the next day to see if it falls outside the 95%
# We are looking the number of times the observed data exceeds the theoretical formulation. 
# We expect the number of violations to be within the confidence interval in file ConfInt.R

# Let's get started....

# Set up the working directory and get the files needed 
library(tseries)
setwd("C:/Users/board/Desktop/Kaggle/Value at Risk/Backtest SP500 VaR")
SPData <- read.csv("S_P_Daily_Log_Return_Data.csv")
head(SPData) 
# check the ordering of the date to make sure we are going forwards through time 

back_obs <- 999  # Number of periods we go back to estimate the GARCH model 
num_pred <- dim(SPData)[1]-back_obs

# Pre allocate space 
coefs <- data.frame(matrix(0,nrow = num_pred, ncol = 3))
names(coefs)[1:3] <- c("a0", "a1", "b1")

for (i in 1:num_pred){ # Rolling window for the GARCH estimates 
# Hypotetical Loss Operator - rolling window for 999 periods 
PortLoss <- SPData[i:(i+back_obs),5]

# Fit Garch to portfolio losses (Trace = suppresses output to console, default is GARCH 1,1)
fit1 <- garch(PortLoss, trace = FALSE)

# Save coefficients of Garch models 
coefs[i,1:3] <- fit1$coef

}

index <- seq(from = 1, to =dim(coefs)[1], by =1)
plot(index,coefs[,2])

# EWMA means 
EWMA_mean <- rep(0,num_pred)
# First value is the mean of the 1st 999 observations
EWMA_mean[1] <- mean(PortLoss[1:back_obs])
# Recursive definition for EWMA  
for (i in 1:(num_pred-1)){
  EWMA_mean[i+1]<- EWMA_mean[i]*0.97 + SPData[(back_obs+i),5]*0.03
}

# Sigma 2 Initilization
sigma2 <- rep(0, num_pred)
sigma2[1] <- var(PortLoss[1:back_obs])

# Simga2 recursive definition using the coefficients from the rolling GARCH 
for(i in 1:(num_pred-1)){
  sigma2[(i+1)] <- coefs[i,1] + coefs[i,2]*(SPData[(back_obs+i),5]-EWMA_mean[i])^2+coefs[i,3]*sigma2[i]
}

# Take a look at the means and volatility over time 
plot(EWMA_mean, main = "EWMA means over time")
plot(sigma2, main = " GARCH Volatility over time")

# Calculate the VaR for the rolling window 
VaR <- EWMA_mean +sqrt(sigma2)*qnorm(0.95)

# Find how many time VaR was violated 
violations <- sum(VaR>SPData[(back_obs+1):dim(SPData)[1],5]) # Some indexing probems are giving me NAs 
num_pred - violations 
# This is a little bit different from his possibly because we calculate the coefficients from garch differently 


