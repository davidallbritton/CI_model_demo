# Step 1: Initialize a 4x4 matrix with 1s on the diagonal
set.seed(42)  # for reproducibility
C <- diag(1, 4)  # Initialize diagonal matrix with 1s

# Generate random 0s and 1s for the upper triangle (excluding the diagonal)
upper_values <- sample(0:1, 6, replace = TRUE)

# Fill the upper triangle of the matrix
C[upper.tri(C)] <- upper_values

# Mirror the upper triangle to the lower triangle to make it symmetrical
C[lower.tri(C)] <- t(C)[lower.tri(C)]

# Step 2: Initialize vector A with 1s
A <- rep(1, 4)

# Step 3: Multiply matrix C by vector A to produce vector A'
A_prime <- C %*% A

# Print results
C
A
A_prime


# Normalize A_prime by dividing each element by the maximum value of A_prime
A_prime_normalized <- A_prime / max(A_prime)

Delta <- sum(abs(A - A_prime_normalized))

# Assign the normalized values back to A
A <- A_prime_normalized

# Print the new normalized vector A
A
Delta

