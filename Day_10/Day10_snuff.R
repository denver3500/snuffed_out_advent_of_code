library(gmp)

# Input
lines <- readLines("Day_10/input_snuff.txt")

# Part A

# Parse 

asteroids <- data.frame(x = integer(), y = integer())
for (y in seq_along(lines)) {
  chars <- strsplit(lines[y], "")[[1]]
  for (x in seq_along(chars)) {
    if (chars[x] == "#") {
      asteroids <- rbind(asteroids, data.frame(x = x-1, y = y-1))
    }
  }
}

# GCD
  GCD <- function(x1, y1, x2, y2) {
  dx <- x2 - x1
  dy <- y2 - y1
  if (dx == 0 && dy == 0) return(NA)
  gcd <- abs(gcd(dx, dy))
  c(dx / gcd, dy / gcd)
}

counts <- integer(nrow(asteroids))
for (i in seq_len(nrow(asteroids))) {
  dirs <- list()
  for (j in seq_len(nrow(asteroids))) {
    if (i != j) {
      dir <-  GCD(asteroids$x[i], asteroids$y[i], asteroids$x[j], asteroids$y[j])
      dirs[[paste(dir, collapse = ",")]] <- TRUE
    }
  }
  counts[i] <- length(dirs)
}

best_idx <- which.max(counts)
print(counts[best_idx])

# Part B

station_x <- asteroids$x[best_idx]
station_y <- asteroids$y[best_idx]
other_asteroids <- asteroids[-best_idx, ]

# Angles and distances
asteroid_data <- data.frame(
  x = other_asteroids$x,
  y = other_asteroids$y,
  angle = numeric(nrow(other_asteroids)),
  distance = numeric(nrow(other_asteroids))
)

for (i in seq_len(nrow(other_asteroids))) {
  dx <- other_asteroids$x[i] - station_x
  dy <- other_asteroids$y[i] - station_y

  angle <- atan2(dx, -dy)
  if (angle < 0) angle <- angle + 2 * pi
  asteroid_data$angle[i] <- angle
  asteroid_data$distance[i] <- sqrt(dx^2 + dy^2)
}

# Sort
angles <- unique(asteroid_data$angle)
angles <- sort(angles)  # Sort clockwise from up

vaporized <- c()
vaporized_count <- 0
while (vaporized_count < 200 && length(vaporized) < nrow(asteroid_data)) {
  for (angle in angles) {
    candidates <- which(asteroid_data$angle == angle & !seq_len(nrow(asteroid_data)) %in% vaporized)

    if (length(candidates) > 0) {
      closest_idx <- candidates[which.min(asteroid_data$distance[candidates])]
      vaporized <- c(vaporized, closest_idx)
      vaporized_count <- vaporized_count + 1
      
      if (vaporized_count == 200) {
        answer <- asteroid_data$x[closest_idx] * 100 + asteroid_data$y[closest_idx]
        print(answer)
        break
      }
    }
  }
}