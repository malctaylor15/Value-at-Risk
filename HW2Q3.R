library (MASS)

# Market cap weights 
Waapl <- 0.438303316794937
Wmsft <- 1-Waapl

n <- 10000 # Number of simulations 

# Draw samples from APPL and MSFT correlated normal distributions using mean and variance from first part 
Mean <- matrix(c(-0.000240631, 0.001098294), nrow=1, ncol=2)
Covariance <- matrix(c(0.000264812, 0.000137727, 0.000137727, 0.00026202),nrow=2,ncol=2)
# Draw samples from 2 correlated normal distributions
Simulations <- mvrnorm(n ,mu=Mean,Sigma=Covariance)


SimLoss <- Waapl*(exp(Simulations[,1])-1) + Wmsft*(exp(Simulations[,2])-1)
quantile(SimLoss, 0.95)

# With mean and variance from ii)

Mean2 <- matrix(c(0.000911110571900704, 0.00122011524043475), nrow=1, ncol=2)
Covariance2 <- matrix(c(0.000146485928425277, 0.0000317933404617028, 0.0000317933404617028, 0.00012592245224807),nrow=2,ncol=2)
# Draw samples from 2 correlated normal distributions
Simulations2 <- mvrnorm(10000,mu=Mean2,Sigma=Covariance2)

# Find the losses using the full loss operator equation 
FullLossOperator <- Waapl*(exp(Simulations2[,1])-1) + Wmsft*(exp(Simulations2[,2])-1)
# Find the 95% VaR
quantile(FullLossOperator, 0.95)





