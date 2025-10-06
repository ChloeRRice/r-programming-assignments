#1 Matrix Addition and Subtraction 
# Define Matrices
A <- matrix(c(2, 0, 1, 3), ncol = 2)
B <- matrix(c(5, 2, 4, -1), ncol = 2)

# Compute and Display Results 
A + B
A - B

#2. Create Diagonal Matrix
D <- diag(c(4, 1, 2, 3))
D

#3 Construct a Custom 5×5 Matrix
E <- diag(3, 5)       
E[1, -1] <- 1               
E[2:5, 1] <- 2 
E