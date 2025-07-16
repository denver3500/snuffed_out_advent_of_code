# Input
input <- scan("Day_2/input_snuff.txt", what = numeric(), sep = ",")

# Part A

# Replace position
input[2] <- 12
input[3] <- 2

# Process the array
i <- 1
while (i <= length(input)) {
    # Read first 4 positions
    opcode <- input[i]
    # R starts indexing at 1, bruh
    pos1 <- input[i + 1] + 1
    pos2 <- input[i + 2] + 1
    pos3 <- input[i + 3] + 1

    if (opcode == 99) {
        break
    }

    if (opcode == 1) {
        input[pos3] <- input[pos1] + input[pos2]
    } 
    
    else if (opcode == 2) {
        input[pos3] <- input[pos1] * input[pos2]
    }

    # Move to the next 4 numbers
    i <- i + 4
}
print (input[1])

# Part B

found <- FALSE
for (noun in 0:99) {
    for (verb in 0:99) {
        # Reset
        input <- scan("Day_2/input_snuff.txt", what = numeric(), sep = ",")
         # Replace positions
        input[2] <- noun
        input[3] <- verb
        # Process the array
        i <- 1
        while (i <= length(input)) {
            #R starts indexing at 1, bruh
            opcode <- input[i]
            pos1 <- input[i + 1] + 1
            pos2 <- input[i + 2] + 1
            pos3 <- input[i + 3] + 1

            if (opcode == 99) {
                break
            }

            if (opcode == 1) {
                input[pos3] <- input[pos1] + input[pos2]
            } 
            
            else if (opcode == 2) {
                input[pos3] <- input[pos1] * input[pos2]
            }

            # Move to the next 4 numbers
            i <- i + 4
        }

        if (input[1] == 19690720) {
            print(paste(noun, verb))
            print(paste(100 * noun + verb))
            found <- TRUE
            break
        }
    }
    if (found) break
}
