# Install/load required package
if (!require(triangle)) install.packages("triangle")
library(triangle)

# Parameters
a <- 41   # min
b <- 210  # max
c <- 95   # mode

# Create a sequence of x values
x <- seq(a, b, length.out = 1000)

# Compute the triangular PDF
y <- dtriangle(x, a = a, b = b, c = c)

# Plot the distribution
plot(x, y, type = "l", lwd = 2, col = "blue",
     main = "Triangular Distribution (min=41, mode=95, max=210)",
     xlab = "Surgery Duration (minutes)", ylab = "Density")

# Add vertical lines for min, mode, and max
abline(v = c(a, c, b), col = c("red", "green", "red"), lty = 2)
text(c(a, c, b), max(y)*0.9, labels = c("Min", "Mode", "Max"), col = c("red", "green", "red"), pos = 3)
