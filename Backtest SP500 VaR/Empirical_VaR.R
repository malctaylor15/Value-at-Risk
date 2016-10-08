# HW2Q5pt2

# Empirical VaR
# This file calculates the 95% VaR using the 95th quantile of the previous 1000 observations. 
# We then see if the next period's loss is larger than this value 
# We can compare this value to the range in ConfInt.R

# Let's get started 

# Set things up 
setwd("C:/Users/board/Desktop/Kaggle/Value at Risk/Backtest SP500 VaR")
SPData <- read.csv("S_P_Daily_Log_Return_Data.csv")
head(SPData) 
PortLoss <- SPData[[5]]

# Parameters for how many previous losses we use 
numb_obs <- dim(SPData)[1]
back_obs <- 999
numb_pred <- numb_obs- back_obs

# Pre allocate space
Var <- rep(0,numb_pred)
Violation <- rep(0,numb_pred)

# Rolling window to find the quantile and check against observed data
for(i in 1:numb_pred){ 

  Var[i] <- quantile(PortLoss[i:(back_obs+i)],0.95)
  Violation[i] <- PortLoss[(i+back_obs)]>Var[i]
  
} 

# Total number of violations 
sum(Violation)
