# HW2Q5Pt3

# This file computes the 95% VaR using variance/ covariance method with EWMA means and EMWA volatility 
# We then back test the theoretical VaR against the next prediction of the next day to see if it falls outside the 95%
# We are looking the number of times the observed data exceeds the theoretical formulation. 
# We expect the number of violations to be within the confidence interval in file ConfInt.R

# Let's get started 

# Set up the working directory and get the files needed 
setwd("C:/Users/board/Desktop/Kaggle/Value at Risk/Backtest SP500 VaR")
SPData <- read.csv("S_P_Daily_Log_Return_Data.csv")
head(SPData)

# Some intial parameters 
numb_obs <- dim(SPData)[1]
back_obs <- 999
numb_pred <- numb_obs- back_obs
Returns <- SPData$Log.Return
PortLoss <- SPData$Hypothetical.Portfolio.Loss

# EWMA means 
EWMA_mean <- rep(0,numb_pred)
# First value is the mean of the 1st 999 observations
EWMA_mean[1] <- mean(PortLoss[1:(back_obs)])
# Recursive definition for EWMA  
for (i in 1:(numb_pred-1)){
  EWMA_mean[i+1]<- EWMA_mean[i]*0.97 + Returns[(back_obs+i)]*0.03
}

# Sigma2 
# Pre allocate space and initialize first component 
sigma2 <- rep(0, numb_pred)
sigma2[1] <- var(PortLoss[1:(1+back_obs)])

# Recursive equation for sigma 
for(i in 1:(numb_pred-1)){
  sigma2[i+1] <- 0.97*sigma2[i] + 0.03*(Returns[(i+back_obs)]-EWMA_mean[i])^2
  
}

# Take a peak at means and volatility 
plot(EWMA_mean, main = "EWMA means over time")
plot(sigma2, main = "EWMA Volatility over time")

# Calculate VaR and check for violations 
Var <- -(exp(EWMA_mean[(2:numb_pred)]+sqrt(sigma2[(2:numb_pred)])*qnorm(0.95))-1)
Violations <- PortLoss[(back_obs):(numb_obs-2)] < Var
sum(Violations)

