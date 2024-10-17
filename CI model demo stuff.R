# Step 1: Initialize the matrix C as before
set.seed(48)
C <- diag(1, 4)
upper_values <- sample(0:1, 6, replace = TRUE)
C[upper.tri(C)] <- upper_values
C[lower.tri(C)] <- t(C)[lower.tri(C)]

# display C
C

# Step 2: Initialize the original vector A
A_original <- rep(1, 4)
A <- A_original  # Set A to be the original values to start the loop

# Step 3: Initialize Delta
Delta <- Inf  # Start with a large value for Delta to enter the loop

# Step 4: Loop until Delta < 0.1
while (Delta >= 0.001) {
  # Multiply C by A to get A_prime
  A_prime <- C %*% A
  
  # Normalize A_prime such that the highest value is 1
  A_prime_normalized <- A_prime / max(A_prime)
  
  # Calculate Delta as the sum of absolute differences
  Delta <- sum(abs(A - A_prime_normalized))
  
  # Print current Delta value for tracking
  print(A)
  print(A_prime_normalized)
  print(paste("Current Delta:", Delta))

  
  # Update A to be the normalized A_prime
  A <- A_prime_normalized

}

# Final output
A
