# Input
input <- scan("Day_8/input_snuff.txt", what = "")
digits <- unlist(strsplit(input, ""))
digits <- as.integer(digits)

# Part A

width <- 25
height <- 6
layer_size <- width * height
num_layers <- length(digits) / layer_size
layers <- split(digits, rep(1:num_layers, each = layer_size, length.out = length(digits)))
zero_counts <- sapply(layers, function(layer) sum(layer == 0))
min_zero_layer_idx <- which.min(zero_counts)
min_zero_layer <- layers[[min_zero_layer_idx]]
one <- sum(min_zero_layer == 1)
two <- sum(min_zero_layer == 2)
result <- one * two

print(result)

# Part B

final_layer <- rep(2, layer_size)
for (i in 1:layer_size) {
  for (l in 1:num_layers) {
    pixel <- layers[[l]][i]
    if (pixel != 2) {
      final_layer[i] <- pixel
      break
    }
  }
}

final_matrix <- matrix(final_layer, nrow = height, byrow = TRUE)
for (row in 1:height) {
  cat(gsub("0", " ", gsub("1", "#", paste(final_matrix[row,], collapse = ""))), "\n")
}
