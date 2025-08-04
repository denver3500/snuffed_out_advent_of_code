intcode <- function(program, inputs) {
  input <- program
  input_index <- 1
  i <- 1
  outputs <- c()
  relative_base <- 0

  get_param <- function(offset, mode) {
    addr <- switch(
      as.character(mode),
      "0" = input[i + offset] + 1,
      "1" = i + offset,
      "2" = input[i + offset] + relative_base + 1
    )
    # Expand memory if needed
    if (addr > length(input)) input <<- c(input, rep(0, addr - length(input)))
    input[addr]
  }

  get_pos <- function(offset, mode = 0) {
    if (mode == 2) {
      addr <- input[i + offset] + relative_base + 1
    } else {
      addr <- input[i + offset] + 1
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
      input[pos3] <- if (val1 < val2) 1 else 0
      i <- i + 4
    } else if (opcode == 8) {
      val1 <- get_param(1, mode1)
      val2 <- get_param(2, mode2)
      pos3 <- get_pos(3, mode3)
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

  tail(outputs, 1)
  print(outputs)
}
