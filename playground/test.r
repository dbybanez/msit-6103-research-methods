# Basic R code to test VS Code setup with charts

install.packages("remotes")


# Print a message
print("VS Code R setup is working!")

# Simple calculation
result <- 2 + 2
print(paste("2 + 2 =", result))

# Create a vector and display it
numbers <- c(1, 2, 3, 4, 5)
print("Numbers vector:")
print(numbers)

# Plot a simple chart
plot(numbers, type = "o", col = "blue", main = "Simple Line Chart", xlab = "Index", ylab = "Value")

# Create a barplot
barplot(numbers, col = "green", main = "Barplot of Numbers", xlab = "Index", ylab = "Value")