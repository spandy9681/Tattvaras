set.seed(123) # Set seed for reproducibility

# Function to generate samples and calculate MLEs
estimate_mles <- function(n, lambda, num_samples) {
  mle_X <- numeric(num_samples)
  mle_Y <- numeric(num_samples)
  
  for (i in 1:num_samples) {
    X <- rexp(n, rate = lambda)
    Y <- ceiling(X)
    
    # MLE using X
    mle_X[i] <- 1 / mean(X)
    
    # MLE using Y
    mean_Y <- mean(Y)
    mle_Y[i] <- log(mean_Y / (mean_Y - 1))
  }
  
  list(mle_X = mle_X, mle_Y = mle_Y)
}

# Parameters
n <- 10^3  # Sample size
lambda <- 2  # Rate parameter for exponential distribution
num_samples <- 100  # Number of samples to generate

# Estimate MLEs
mle_estimates <- estimate_mles(n, lambda, num_samples)

# Calculate variances
var_mle_X <- var(mle_estimates$mle_X)
var_mle_Y <- var(mle_estimates$mle_Y)

# Output the results
cat("Variance of MLE using X:", var_mle_X, "\n")
cat("Variance of MLE using Y:", var_mle_Y, "\n")

# Plot histograms of the MLE estimates
par(mfrow = c(1, 2))  # Set up the plotting area to have 2 plots side by side

hist(mle_estimates$mle_X, probability = TRUE, main = bquote("Histogram of MLE using X" ~ (lambda == .(lambda))))
abline(v = lambda, col = "red", lwd = 2, lty = 2)  # Actual lambda

hist(mle_estimates$mle_Y, probability = TRUE, main = bquote("Histogram of MLE using Y" ~ (lambda == .(lambda))))
abline(v = lambda, col = "red", lwd = 2, lty = 2)  # Actual lambda
