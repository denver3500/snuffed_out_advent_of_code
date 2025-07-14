#Input
input <- scan("Day_1/input_snuff.txt", what = numeric())
#Part A
computed <- floor(input / 3) - 2
answer_a <- sum(computed)
print(answer_a)
#Part B
answer_b <- 0
for (i in 1:length(input)) {
  current_number <- input[i]
  while (current_number > 0) {
    current_number <- floor(current_number / 3) - 2
    if (current_number > 0) {
      answer_b <- answer_b + current_number
    }
  }
}

print(answer_b)