# Confidence Interval 

m <- 1516
alpha <- 0.95
beta <- 0.05


N <- m*(1-alpha)
Z_1_b2 <- qnorm(0.975)

CI_upper <- N+Z_1_b2*sqrt(m*alpha*(1-alpha))
CI_lower <- N-Z_1_b2*sqrt(m*alpha*(1-alpha))


N
CI_upper
CI_lower


# Visualize confidence interval estimates 
x<- seq(from = 20, to = 140, by = 0.1)
y <- dnorm(x, mean= N, sd = Z_1_b2*sqrt(m*alpha*(1-alpha)))


plot(x,y, main = "Confidence interval Estimates", type = "l", lwd = 2)
abline(v = c(79,46,50, CI_lower,CI_upper), col = c("blue", "green", "pink", "red", "red"), lwd = "3")
legend("topright",col = c("blue", "green", "pink", "red", "red"), c("EWMA", "Empirical", "Rolling Covariance", "Upper CI", "Lower CI"), lty = 1, lwd = 3)
