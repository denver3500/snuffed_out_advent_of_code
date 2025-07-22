#Part A

count <- 0

is_valid_password <- function(num) {
  # Convert to string
  digits <- as.character(num)
  digit_chars <- strsplit(digits, "")[[1]]
  
  # 6 digits
  if (nchar(digits) != 6) {
    return(FALSE)
  }
  
  # One pair is adjacent digits
  has_adjacent <- FALSE
  for (i in 1:(length(digit_chars) - 1)) {
    if (digit_chars[i] == digit_chars[i + 1]) {
      has_adjacent <- TRUE
      break
    }
  }
  if (!has_adjacent) {
    return(FALSE)
  }
  
  # Digits never increase
  for (i in 1:(length(digit_chars) - 1)) {
    if (as.numeric(digit_chars[i]) > as.numeric(digit_chars[i + 1])) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

for (num in 128392:643281) {
  if (is_valid_password(num)) {
    count <- count + 1
  }
}

print(count)


# Part B

is_valid_password_part_b <- function(num) {
  # Convert to string
  digits <- as.character(num)
  digit_chars <- strsplit(digits, "")[[1]]
  
  # 6 digits
  if (nchar(digits) != 6) {
    return(FALSE)
  }

  # Check if we have one pair of adjacent digits that is not part of a larger group
  has_exact_pair <- FALSE
  i <- 1
  while (i <= length(digit_chars)) {
    current_digit <- digit_chars[i]
    count_consecutive <- 1
    
    while (i + count_consecutive <= length(digit_chars) && 
           digit_chars[i + count_consecutive] == current_digit) {
      count_consecutive <- count_consecutive + 1
    }
    
    # Valid if we have a pair
    if (count_consecutive == 2) {
      has_exact_pair <- TRUE
    }
    
    i <- i + count_consecutive
  }
  
  if (!has_exact_pair) {
    return(FALSE)
  }
  
  # Digits never decrease
  for (i in 1:(length(digit_chars) - 1)) {
    if (as.numeric(digit_chars[i]) > as.numeric(digit_chars[i + 1])) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

# Part B count
count_b <- 0
for (num in 128392:643281) {
  if (is_valid_password_part_b(num)) {
    count_b <- count_b + 1
  }
}

print(count_b)

