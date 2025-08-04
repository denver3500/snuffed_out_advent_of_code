# Import intcode function
source("Day_9/intcode.R")
# Input
input <- scan("Day_9/input_advent.txt", what = numeric(), sep = ",")

# Part A
result <- intcode(input, 1)

# Part B
result <- intcode(input, 2)