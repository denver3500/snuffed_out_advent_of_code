intcode <- function(program, inputs = NULL, state = NULL) {
  # Initialize or restore state
  if (is.null(state)) {
    # New execution
    input <- program
    input_index <- 1
    i <- 1
    outputs <- c()
    relative_base <- 0
  } else {
    # Continue from saved state
    input <- state$input
    i <- state$i
    relative_base <- state$relative_base
    input_index <- 1  # Reset for new inputs
    outputs <- c()
  }
  
  # Handle inputs
  if (!is.null(inputs)) {
    if (length(inputs) == 1) inputs <- c(inputs)
  } else {
    inputs <- c()
  }

  get_param <- function(offset, mode) {
    addr <- switch(
      as.character(mode),
      "0" = input[i + offset] + 1,
      "1" = i + offset,
      "2" = input[i + offset] + relative_base + 1,
      NULL
    )

    if (is.null(addr) || is.na(addr) || addr <= 0) {
      stop(sprintf("Invalid address calculated: %s at position %d", addr, i))
    }
    # Expand memory if needed
    if (addr > length(input)) input <<- c(input, rep(0, addr - length(input)))
    result <- input[addr]
    if (is.na(result)) result <- 0
    result
  }

  get_pos <- function(offset, mode = 0) {
    if (mode == 2) {
      addr <- input[i + offset] + relative_base + 1
    } else {
      addr <- input[i + offset] + 1
    }
    if (is.na(addr) || addr <= 0) {
      stop(sprintf("Invalid position calculated: %s at position %d", addr, i))
    }
    if (addr > length(input)) input <<- c(input, rep(0, addr - length(input)))
    addr
  }

  repeat {
    opcode_full <- input[i]
    opcode_str <- sprintf("%05d", opcode_full)
    opcode <- as.numeric(substr(opcode_str, 4, 5))
    mode1 <- as.numeric(substr(opcode_str, 3, 3))
    mode2 <- as.numeric(substr(opcode_str, 2, 2))
    mode3 <- as.numeric(substr(opcode_str, 1, 1))

    if (opcode == 99) break

    if (opcode == 1) {
      val1 <- get_param(1, mode1)
      val2 <- get_param(2, mode2)
      pos3 <- get_pos(3, mode3)
      input[pos3] <- val1 + val2
      i <- i + 4
    } else if (opcode == 2) {
      val1 <- get_param(1, mode1)
      val2 <- get_param(2, mode2)
      pos3 <- get_pos(3, mode3)
      input[pos3] <- val1 * val2
      i <- i + 4
    } else if (opcode == 3) {
      pos1 <- get_pos(1, mode1)
      if (input_index > length(inputs)) {
        return(list(outputs = outputs, halted = FALSE, input_needed = TRUE, 
                   state = list(input = input, i = i, relative_base = relative_base)))
      }
      input[pos1] <- inputs[input_index]
      input_index <- input_index + 1
      i <- i + 2
    } else if (opcode == 4) {
      val1 <- get_param(1, mode1)
      outputs <- c(outputs, val1)
      i <- i + 2
    } else if (opcode == 5) {
      val1 <- get_param(1, mode1)
      val2 <- get_param(2, mode2)
      if (val1 != 0) {
        i <- val2 + 1
      } else {
        i <- i + 3
      }
    } else if (opcode == 6) {
      val1 <- get_param(1, mode1)
      val2 <- get_param(2, mode2)
      if (val1 == 0) {
        i <- val2 + 1
      } else {
        i <- i + 3
      }
    } else if (opcode == 7) {
      val1 <- get_param(1, mode1)
      val2 <- get_param(2, mode2)
      pos3 <- get_pos(3, mode3)
      if (is.na(val1) || is.na(val2)) {
        stop(sprintf("NA values encountered in comparison at position %d", i))
      }
      input[pos3] <- if (val1 < val2) 1 else 0
      i <- i + 4
    } else if (opcode == 8) {
      val1 <- get_param(1, mode1)
      val2 <- get_param(2, mode2)
      pos3 <- get_pos(3, mode3)
      if (is.na(val1) || is.na(val2)) {
        stop(sprintf("NA values encountered in comparison at position %d", i))
      }
      input[pos3] <- if (val1 == val2) 1 else 0
      i <- i + 4
    } else if (opcode == 9) {
      val1 <- get_param(1, mode1)
      relative_base <- relative_base + val1
      i <- i + 2
    } else {
      stop(sprintf("Unknown opcode: %d at position %d", opcode, i))
    }
  }

  return(list(outputs = outputs, halted = TRUE))
}