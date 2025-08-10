library(combinat)
library(numbers)

# Input
start_time <- Sys.time()
lines <- readLines("Day_12/input_snuff.txt")
coords <- do.call(rbind, lapply(lines, function(line) {
  nums <- as.integer(unlist(regmatches(line, gregexpr("-?\\d+", line))))
  return(nums)
}))
colnames(coords) <- c("x", "y", "z")

# Part A



# Initialize velocity
velocities <- matrix(0, nrow = nrow(coords), ncol = 3)
colnames(velocities) <- c("x", "y", "z")
# Unique permutations
pairs <- combn(nrow(coords), 2)

for (step in 1:1000) {
# Apply gravity
for (k in 1:ncol(pairs)) {
  i <- pairs[1, k]
  j <- pairs[2, k]
  for (axis in 1:3) {
    if (coords[i, axis] < coords[j, axis]) {
      velocities[i, axis] <- velocities[i, axis] + 1
      velocities[j, axis] <- velocities[j, axis] - 1
    } else if (coords[i, axis] > coords[j, axis]) {
      velocities[i, axis] <- velocities[i, axis] - 1
      velocities[j, axis] <- velocities[j, axis] + 1
    }
  }
}

 # Apply velocity
  coords <- coords + velocities
}

# Calculate energies
potential_energy <- apply(abs(coords), 1, sum)
kinetic_energy <- apply(abs(velocities), 1, sum)
total_energy <- potential_energy * kinetic_energy

print(sum(total_energy))
end_time1 <- Sys.time()
print(end_time1 - start_time)
# Part B

coords <- do.call(rbind, lapply(lines, function(line) {
  nums <- as.integer(unlist(regmatches(line, gregexpr("-?\\d+", line))))
  return(nums)
}))
colnames(coords) <- c("x", "y", "z")


find_axis_cycle <- function(coords, axis) {
  initial_coords <- coords[, axis]
  velocities <- rep(0, length(initial_coords))
  step <- 0
  repeat {
    # Apply gravity
    for (i in 1:(length(coords[, axis]) - 1)) {
      for (j in (i + 1):length(coords[, axis])) {
        if (coords[i, axis] < coords[j, axis]) {
          velocities[i] <- velocities[i] + 1
          velocities[j] <- velocities[j] - 1
        } else if (coords[i, axis] > coords[j, axis]) {
          velocities[i] <- velocities[i] - 1
          velocities[j] <- velocities[j] + 1
        }
      }
    }
    # Apply velocity
    coords[, axis] <- coords[, axis] + velocities
    step <- step + 1
    
    if (all(coords[, axis] == initial_coords) && all(velocities == 0)) {
      return(step)
    }
  }
}

x_cycle <- find_axis_cycle(coords, 1)
y_cycle <- find_axis_cycle(coords, 2)
z_cycle <- find_axis_cycle(coords, 3)

system_cycle <- LCM(x_cycle, LCM(y_cycle, z_cycle))
print(system_cycle)

end_time2 <- Sys.time()
print(end_time2 - start_time)