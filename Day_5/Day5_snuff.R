# Input
input <- scan("Day_5/input_snuff.txt", what = numeric(), sep = ",")

# Part A

# Process the array
i <- 1
while (i <= length(input)) {
   # Read first instruction
    opcode_full <- input[i]
    opcode_str <- sprintf("%05d", opcode_full)
    
    # Extract parts
    opcode <- as.numeric(substr(opcode_str, 4, 5))  # Last 2 digits (positions 4-5)
    mode1 <- as.numeric(substr(opcode_str, 3, 3))   # 3rd digit from right
    mode2 <- as.numeric(substr(opcode_str, 2, 2))   # 2nd digit from right
    mode3 <- as.numeric(substr(opcode_str, 1, 1))   # 1st digit from right

    # Get parameter values based on modes
    if (opcode %in% c(1, 2)) {
    val1 <- if (mode1 == 0) input[input[i + 1] + 1] else input[i + 1]
    val2 <- if (mode2 == 0) input[input[i + 2] + 1] else input[i + 2]
    pos3 <- input[i + 3] + 1 # Parameters that an instruction writes to will never be in immediate mode.
    }

    else if (opcode == 3){ 
        pos1 <- input[i + 1] + 1
    } 

    else if (opcode == 4) {
        val1 <- if (mode1 == 0) input[input[i + 1] + 1] else input[i + 1]
    }

    # Process the instruction
    if (opcode == 99) {
    break
    }

    if (opcode == 1) {
    input[pos3] <- val1 + val2
    i <- i + 4
    }

    else if (opcode == 2) {
    input[pos3] <- val1 * val2
    i <- i + 4
    }

    else if (opcode == 3) {
    input[pos1] <- as.numeric(readline(prompt = "Number: "))
    i <- i + 2
    } 
    
    else if (opcode == 4) {
    print(val1)
    i <- i + 2
    }
}

# Part B

input <- scan("Day_5/input_snuff.txt", what = numeric(), sep = ",")
i <- 1
while (i <= length(input)) {
   # Read first instruction
    opcode_full <- input[i]
    opcode_str <- sprintf("%05d", opcode_full)
    
    # Extract parts
    opcode <- as.numeric(substr(opcode_str, 4, 5))  # Last 2 digits (positions 4-5)
    mode1 <- as.numeric(substr(opcode_str, 3, 3))   # 3rd digit from right
    mode2 <- as.numeric(substr(opcode_str, 2, 2))   # 2nd digit from right
    mode3 <- as.numeric(substr(opcode_str, 1, 1))   # 1st digit from right

    # Get parameter values based on modes
    if (opcode %in% c(1, 2, 7, 8)) {
        val1 <- if (mode1 == 0) input[input[i + 1] + 1] else input[i + 1]
        val2 <- if (mode2 == 0) input[input[i + 2] + 1] else input[i + 2]
        pos3 <- input[i + 3] + 1 # Parameters that an instruction writes to will never be in immediate mode.
    }
    else if (opcode == 3){ 
        pos1 <- input[i + 1] + 1
    } 
    else if (opcode %in% c(4, 5, 6)) {
        val1 <- if (mode1 == 0) input[input[i + 1] + 1] else input[i + 1]
        if (opcode %in% c(5, 6)) {
            val2 <- if (mode2 == 0) input[input[i + 2] + 1] else input[i + 2]
        }
    }

    # Process the instruction
    if (opcode == 99) {
        break
    }

    if (opcode == 1) {
        input[pos3] <- val1 + val2
        i <- i + 4
    }
    else if (opcode == 2) {
        input[pos3] <- val1 * val2
        i <- i + 4
    }
    else if (opcode == 3) {
        input[pos1] <- as.numeric(readline(prompt = "Number: "))
        i <- i + 2
    } 
    else if (opcode == 4) {
        print(val1)
        i <- i + 2
    }
    else if (opcode == 5) { 
        if (val1 != 0) {
            i <- val2 + 1  
        } else {
            i <- i + 3
        }
    }
    else if (opcode == 6) {  
        if (val1 == 0) {
            i <- val2 + 1
        } else {
            i <- i + 3
        }
    }
    else if (opcode == 7) {
        input[pos3] <- if (val1 < val2) 1 else 0
        i <- i + 4
    }
    else if (opcode == 8) {
        input[pos3] <- if (val1 == val2) 1 else 0
        i <- i + 4
    }
}