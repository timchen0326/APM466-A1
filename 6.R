# Load necessary libraries
library(MASS)

# Define the covariance matrices (Ensure these are the ones from Question 5)
cov_matrix_yield <- matrix(
  c(1.420517e-07, -1.684827e-08, 1.635268e-09, 3.834781e-07, 9.999913e-08,
    -1.684827e-08, 3.620438e-07, 1.614207e-07, 3.251919e-07, 6.590317e-07,
    1.635268e-09, 1.614207e-07, 1.515073e-07, 1.040596e-07, 2.989427e-07,
    3.834781e-07, 3.251919e-07, 1.040596e-07, 1.592843e-06, 1.057839e-06,
    9.999913e-08, 6.590317e-07, 2.989427e-07, 1.057839e-06, 1.482566e-06),
  nrow = 5, byrow = TRUE)

cov_matrix_forward <- matrix(
  c(0.113869726, 0.012213974, -0.079436363, -0.03318858, 0.02247229,
    0.012213974, 0.264731695, 0.004096365, -0.60381017, 0.31924787,
    -0.079436363, 0.004096365, 0.197153851, -0.06044127, 0.03096766,
    -0.033188576, -0.603810169, -0.060441274, 1.39875625, -0.74074577,
    0.022472286, 0.319247866, 0.030967656, -0.74074577, 0.39253647),
  nrow = 5, byrow = TRUE)

# Compute Eigenvalues and Eigenvectors
eigen_yield <- eigen(cov_matrix_yield)
eigen_forward <- eigen(cov_matrix_forward)

# Print Eigenvalues and Eigenvectors
print("Eigenvalues (Yield):")
print(eigen_yield$values)
print("Eigenvectors (Yield):")
print(eigen_yield$vectors)

print("Eigenvalues (Forward):")
print(eigen_forward$values)
print("Eigenvectors (Forward):")
print(eigen_forward$vectors)

# Interpretation
largest_eigenvalue_yield <- eigen_yield$values[1]
largest_eigenvector_yield <- eigen_yield$vectors[,1]

largest_eigenvalue_forward <- eigen_forward$values[1]
largest_eigenvector_forward <- eigen_forward$vectors[,1]

cat("The largest eigenvalue for yield rates is", largest_eigenvalue_yield, "which explains the dominant movement in yields.\n")
cat("The corresponding eigenvector is", largest_eigenvector_yield, "indicating a parallel shift in yields.\n")

cat("The largest eigenvalue for forward rates is", largest_eigenvalue_forward, "which explains the dominant movement in forward rates.\n")
cat("The corresponding eigenvector is", largest_eigenvector_forward, "indicating the most significant factor affecting forward rate changes.\n")