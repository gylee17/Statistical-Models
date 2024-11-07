joint_prob <- data.frame (
  x = c(-1, -1, -1, 0, 0, 0, 2, 2, 2),
  y = c(1, 2, 4, 1, 2, 4, 1, 2, 4),
  xy_prob = c(0.06, 0.10, 0.04, 0.21, 0.35, 0.14, 0.03, 0.05, 0.02)
)

# Question 1
marginal_prob_x <- aggregate(xy_prob ~ x, data = joint_prob, sum)
print("Question 1:")
print(marginal_prob_x)

# Question 2
x_val <- c(-1, 0, 2)
x_prob <- c(0.2, 0.7, 0.1)
x_exp <- sum(x_val * x_prob)
print(paste("Question 2:", x_exp))

# Question 3
x_variance <- sum((x_val - x_exp)^2 *x_prob)
print(paste("Question 3:", x_variance))

# Question 4
x_std <- sqrt(x_variance)
print(paste("Question 4:", x_std))

# Question 5
 # marginal probability mass function of y
y_val <- c(1, 2, 4, 1, 2, 4, 1, 2, 4)
xy_prob = c(0.06, 0.10, 0.04, 0.21, 0.35, 0.14, 0.03, 0.05, 0.02)
marginal_prob_y <- tapply(xy_prob, y_val, sum)
print("Question 5:")
print(marginal_prob_y)

 # expectation of y
y_unique <- unique(y_val)
y_exp <- sum(y_unique * marginal_prob_y)
print(y_exp)

 # variance of y
y_variance <- sum((y_unique - y_exp)^2 *marginal_prob_y)
print(y_variance)

 # std of y
y_std <- sqrt(y_variance)
print(y_std)

# Question 6
xy_cov <- sum((joint_prob$x - x_exp) * (joint_prob$y - y_exp) * joint_prob$xy_prob)
print(paste("Question 6:", xy_cov))

# Question 7
xy_corr <- xy_cov / (x_std * y_std)
print(paste("Question 7:", xy_corr))

# Question 8
print("Question 8: Random variables X and Y are uncorrelated.")

# Question 9
x_prob <- matrix(c(0.2, 0.7, 0.1), ncol = 1) #column
y_prob <- matrix(c(0.3, 0.5, 0.2), nrow = 1) #row

matrix_mult <- x_prob %*% y_prob
print("Question 9:")
print(matrix_mult)

# Question 10
print("Question 10: This matrix matches the one in Question 9.")

# Question 11
print("Question 11: Random variables X and Y are independent.")

# Question 12
print("Question 12: The exponential model can be viewed as a linear model after a logarithmic transformation, taking the logarithm of both sides. The transformation may look like: log_y <- log(B0) + B1 * log(x) + log(Epsilon).")

