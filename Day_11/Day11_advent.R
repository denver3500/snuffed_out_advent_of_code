# Import intcode function
source("Day_11/intcode.R")
# Input
input <- scan("Day_11/input_advent.txt", what = numeric(), sep = ",")

# Part A

simulate_robot <- function(program, start_color = 0) {
  # Robot state
  x <- 0
  y <- 0
  direction <- 0
  panels <- list()
  current_state <- NULL
  panels[["0,0"]] <- start_color

  repeat {
    # Get current panel color
    panel_key <- paste(x, y, sep = ",")
    current_color <- ifelse(is.null(panels[[panel_key]]), 0, panels[[panel_key]])

    if (is.null(current_state)) {
      result <- intcode(program, current_color)
    } else {
      result <- intcode(NULL, current_color, current_state)
    }
    if (result$halted) break
    if (result$input_needed && length(result$outputs) >= 2) {
      # Process outputs
      paint_color <- result$outputs[1]
      turn_direction <- result$outputs[2]
      panels[[panel_key]] <- paint_color
      # Turn 
      if (turn_direction == 0) { 
        direction <- (direction - 1) %% 4
      } else { 
        direction <- (direction + 1) %% 4
      }

      # Move
      if (direction == 0) y <- y + 1        # up
      else if (direction == 1) x <- x + 1   # right
      else if (direction == 2) y <- y - 1   # down
      else if (direction == 3) x <- x - 1   # left
      # Save state
      current_state <- result$state
    } else {
      break
    }
  }
  
  return(panels)
}

painted_panels <- simulate_robot(input)
print(length(painted_panels))

# Part B
painted_panels <- simulate_robot(input, start_color = 1)

# Coords
coords <- names(painted_panels)
vals <- unlist(painted_panels)
xy <- do.call(rbind, strsplit(coords, ","))
x <- as.integer(xy[,1])
y <- as.integer(xy[,2])
# Size
min_x <- min(x)
max_x <- max(x)
min_y <- min(y)
max_y <- max(y)
width <- max_x - min_x + 1
height <- max_y - min_y + 1
# Matrix
panel_matrix <- matrix(0, nrow = height, ncol = width)
for (i in seq_along(x)) {
  row <- max_y - y[i] + 1  # invert y for display
  col <- x[i] - min_x + 1
  panel_matrix[row, col] <- vals[i]
}

for (row in 1:height) {
  cat(gsub("0", " ", gsub("1", "#", paste(panel_matrix[row,], collapse = ""))), "\n")
}