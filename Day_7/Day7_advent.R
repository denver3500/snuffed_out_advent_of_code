library(combinat) # R doesn't have a built-in permutation function lmao

# Input
original_input <- scan("Day_7/input_advent.txt", what = numeric(), sep = ",")

# Part A

run_intcode <- function(program, inputs) {
    input <- program  # Make a copy
    input_index <- 1
    i <- 1
    outputs <- c()
    
    while (i <= length(input)) {
        # Read first instruction
        opcode_full <- input[i]
        opcode_str <- sprintf("%05d", opcode_full)
        
        # Extract parts
        opcode <- as.numeric(substr(opcode_str, 4, 5))
        mode1 <- as.numeric(substr(opcode_str, 3, 3))
        mode2 <- as.numeric(substr(opcode_str, 2, 2))

        # Get parameter values based on modes
        if (opcode %in% c(1, 2, 7, 8)) {
            val1 <- if (mode1 == 0) input[input[i + 1] + 1] else input[i + 1]
            val2 <- if (mode2 == 0) input[input[i + 2] + 1] else input[i + 2]
            pos3 <- input[i + 3] + 1
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
            input[pos1] <- inputs[input_index]
            input_index <- input_index + 1
            i <- i + 2
        } 
        else if (opcode == 4) {
            outputs <- c(outputs, val1)
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
    
    return(tail(outputs, 1))  # Return last output
}

# Generate permutations
phase_settings <- permn(0:4)
max_output <- 0

for (phases in phase_settings) {
    output <- 0
    # Run 5 times
    for (i in 1:5) {
        inputs <- c(phases[i], output)
        output <- run_intcode(original_input, inputs)
    }
    
    if (output > max_output) {
        max_output <- output
    }
}

print(max_output)

# Part B

run_intcode_with_state <- function(program, inputs, state = NULL) {
    # Initialize or restore state
    if (is.null(state)) {
        input <- program  # Make a copy
        input_index <- 1
        i <- 1
        outputs <- c()
    } else {
        input <- state$input
        input_index <- state$input_index
        i <- state$i
        outputs <- state$outputs
    }
    
    # Add new inputs to queue
    input_queue <- c()
    if (!is.null(state) && !is.null(state$input_queue)) {
        input_queue <- state$input_queue
    }
    input_queue <- c(input_queue, inputs)
    current_input_pos <- if (is.null(state) || is.null(state$current_input_pos)) 1 else state$current_input_pos
    
    while (i <= length(input)) {
        # Read first instruction
        opcode_full <- input[i]
        opcode_str <- sprintf("%05d", opcode_full)
        
        # Extract parts
        opcode <- as.numeric(substr(opcode_str, 4, 5))
        mode1 <- as.numeric(substr(opcode_str, 3, 3))
        mode2 <- as.numeric(substr(opcode_str, 2, 2))

        # Get parameter values based on modes
        if (opcode %in% c(1, 2, 7, 8)) {
            val1 <- if (mode1 == 0) input[input[i + 1] + 1] else input[i + 1]
            val2 <- if (mode2 == 0) input[input[i + 2] + 1] else input[i + 2]
            pos3 <- input[i + 3] + 1
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
            return(list(output = if (length(outputs) > 0) tail(outputs, 1) else NULL, 
                       halted = TRUE, state = NULL))
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
            if (current_input_pos <= length(input_queue)) {
                input[pos1] <- input_queue[current_input_pos]
                current_input_pos <- current_input_pos + 1
                i <- i + 2
            } else {
                # Need more input - pause and save state
                state <- list(input = input, input_index = input_index, i = i, 
                             outputs = outputs, input_queue = input_queue, 
                             current_input_pos = current_input_pos)
                return(list(output = if (length(outputs) > 0) tail(outputs, 1) else NULL, 
                           halted = FALSE, state = state))
            }
        } 
        else if (opcode == 4) {
            outputs <- c(outputs, val1)
            i <- i + 2
            # Return output and current state
            state <- list(input = input, input_index = input_index, i = i, 
                         outputs = outputs, input_queue = input_queue, 
                         current_input_pos = current_input_pos)
            return(list(output = val1, halted = FALSE, state = state))
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
    
    return(list(output = if (length(outputs) > 0) tail(outputs, 1) else NULL, 
               halted = TRUE, state = NULL))
}

# Generate permutations for phase settings 5-9
phase_settings_b <- permn(5:9)
max_output_b <- 0

for (phases in phase_settings_b) {

    # Initialize 5 amplifiers' states
    amp_states <- vector("list", 5)
    halted_flags <- rep(FALSE, 5)
    output <- 0

    # First run: initialize each amplifier with its phase setting and first input
    for (i in 1:5) {
        result <- run_intcode_with_state(original_input, c(phases[i], output), NULL)
        amp_states[[i]] <- result$state
        output <- result$output
        halted_flags[i] <- result$halted
    }

    last_output <- output

    # Feedback loop
    while (!all(halted_flags)) {
        for (i in 1:5) {
            if (!halted_flags[i]) {
                result <- run_intcode_with_state(original_input, c(output), amp_states[[i]])
                if (!result$halted) {
                    amp_states[[i]] <- result$state
                    output <- result$output
                    if (i == 5) last_output <- output
                } else {
                    halted_flags[i] <- TRUE
                }
            }
        }
    }

    if (last_output > max_output_b) {
        max_output_b <- last_output
    }
}

print(paste("Part B - Max output:", max_output_b))