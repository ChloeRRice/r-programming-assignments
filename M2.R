assignment2 <- c(16, 18, 14, 22, 27, 17, 19, 17, 17, 22, 20, 22)

myMean <- function(x) {
  return(sum(x) / length(x))
}

myMean(assignment2)

# The data was added to assignment2 and the function myMean was tested. 
# It returned the error message "Error in myMean(assignment2) : object 'assignment' not found". 
# This error results from the variable names of the function. Currently, the 
# myMean function is set up less as a function to be used multiple times, 
# and more as a specific operation, and calls for inputs that do not exist in the 
# workspace. In order for the function to work properly, it should be modified so 
# that it uses the input. I replaced the inputs with x, as that is more 
# standardized for custom functions. The working result is shown below. 