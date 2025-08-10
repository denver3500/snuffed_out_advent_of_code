source("Day_11/intcode.R")
# Input
input <- scan("Day_13/input_snuff.txt", what = numeric(), sep = ",")

# Part A
run <- intcode(input)
outputs <- run$outputs

tiles <- data.frame(
    x  = outputs[seq(1, length(outputs), 3)],
    y  = outputs[seq(2, length(outputs), 3)],
    id = outputs[seq(3, length(outputs), 3)]
)

# Count block tiles (id == 2)
block_count <- sum(tiles$id == 2)
print(block_count)

# Part B

input <- scan("Day_13/input_snuff.txt", what = numeric(), sep = ",")
input[1] <- 2

state <- NULL
inputs <- c()
score <- 0
paddle_x <- NA
ball_x <- NA

repeat {
    res <- intcode(input, inputs = inputs, state = state)
    outputs <- res$outputs
    
    for (k in seq(1, length(outputs), by = 3)) {
        x <- outputs[k]
        y <- outputs[k + 1]
        v <- outputs[k + 2]

        if (x == -1 && y == 0) {
            score <- v
        } else {
            if (v == 3) paddle_x <- x
            if (v == 4) ball_x <- x
        }
    }

    if (isTRUE(res$halted)) break
    move <- if (is.na(ball_x) || is.na(paddle_x)) 0 else sign(ball_x - paddle_x)
    inputs <- c(move)
    state <- res$state
}

print(score)
