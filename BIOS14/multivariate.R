#install.packages("ellipse")
rm(list = ls())
C = matrix(c(0.7, 0.2, -0.3,
             0.2, 1.2, 0.4,
             -0.3, 0.4, 0.6),
           nrow=3)
dim(C)
t(C)
cov2cor(C) #Translate the covariance matrix into a correlation matrix.
#Cor(x, y) = Cov(x, y)/√V ar(x)V ar(y).
library(MASS)
library(ellipse)
set.seed(1)
#mu=a vector giving the means of the variables.
#Sigma=a positive-definite symmetric matrix specifying the covariance matrix of the variables.
X = data.frame(mvrnorm(200, mu=c(0,0,0), Sigma=C)) #var(X$z1)=0.7095716, var(X$z2)=1.018892, var(X$z3)=0.6231182
colnames(X) = c("z1", "z2", "z3")
head(X)
means = c(apply(X[,1:2], 2, mean))#X[,1:2] selects 1:2 columns of the data frame X, 2 means two rows
plot(X$z1, X$z2, las=1)
#this is a covariance matrix, that is why use cov
lines(ellipse(cov(X[,1:2]), centre=means))#var, cov and cor compute the variance of x and the 
#covariance or correlation of x and y if these are vectors.
eigen(C)
plot(X$z1, X$z2, las=1, col="grey")
lines(ellipse(cov(X[,1:2]), centre=means))
arrows(means[1], means[2], # take the stronger eigen value, and eigen vector beneath it
       means[1]+eigen(C)$vectors[1,1],
       means[2]+eigen(C)$vectors[2,1],
       code=2, length=0.1, lwd=2)
arrows(means[1], means[2],
       means[1]+eigen(C)$vectors[1,2],
       means[2]+eigen(C)$vectors[2,2],
       code=2, length=0.1, lwd=2)
# Extract eigenvalues
eigenvalues <- eigen(C)$values
sum(eigen(C)$vectors[1])

# Compute the proportion of variance associated with each eigenvector
eigen(C)$values/sum(eigen(C)$values)

#Length of the eigenvectors
#a = sum of square [1,1], [2,1], [3,1]
#squareroot of a -> 1

# Assuming eig_C contains the eigenvalues and eigenvectors of C
eigenvectors <- eigen(C)$vectors

# Extract the first and second eigenvectors
eigenvector_1 <- eigenvectors[, 1]
eigenvector_2 <- eigenvectors[, 2]

# Compute the dot product of the eigenvectors
dot_product <- sum(eigenvector_1 * eigenvector_2)

# Calculate the lengths of the eigenvectors
length_1 <- sqrt(sum(eigenvector_1^2))
length_2 <- sqrt(sum(eigenvector_2^2))

# Calculate the cosine of the angle between the vectors
cos_angle <- dot_product / (length_1 * length_2)

# Calculate the angle in radians
angle_radians <- acos(cos_angle)

# Convert radians to degrees
angle_degrees <- angle_radians * 180 / pi

# Output the angle between the eigenvectors in degrees
print(paste("Angle between the first and second eigenvectors in degrees:", round(angle_degrees, 2)))

# Given eigenvalues and eigenvectors
eigenvalues <- c(1.4034727, 0.9349665, 0.1615607)
eigenvectors <- matrix(c(0.07739007, 0.8285983, 0.5544688,
                         0.90374270, 0.1765485, -0.3899741,
                         0.42102245, -0.5312773, 0.7351766), nrow = 3)

# Reconstruct the matrix C from the eigenvalues and eigenvector
P <- eigenvectors
Lambda <- diag(eigenvalues)

# Calculate the inverse of P
P_inv <- solve(P)

# Reconstruct the matrix C
reconstructed_C <- P %*% Lambda %*% P_inv

# Print the reconstructed matrix
print(reconstructed_C)
