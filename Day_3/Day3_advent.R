# Input
input <- readLines("Day_3/input_advent.txt")
path1 <- strsplit(input[1], ",")[[1]]
path2 <- strsplit(input[2], ",")[[1]]

#Part A

#Function to draw path
draw_path <- function(path) {
  coords <- c(0, 0)
  path_coords <- list()
  
  for (move in path) {
    direction <- substr(move, 1, 1)
    distance <- as.numeric(substr(move, 2, nchar(move)))
    
    for (i in seq_len(distance)) {
      if (direction == "R") {
        coords[1] <- coords[1] + 1
      } else if (direction == "L") {
        coords[1] <- coords[1] - 1
      } else if (direction == "U") {
        coords[2] <- coords[2] + 1
      } else if (direction == "D") {
        coords[2] <- coords[2] - 1
      }
      path_coords[[length(path_coords) + 1]] <- paste(coords, collapse = ",")
    }
  }
  
  return(path_coords)
}

path1_coords <- draw_path(path1)
path2_coords <- draw_path(path2)

matrix_conversion <- function(coords) {
  do.call(rbind, lapply(coords, function(coord_str) {
    as.numeric(strsplit(coord_str, ",")[[1]])
  }))
}

path1_matrix <- matrix_conversion(path1_coords)
path2_matrix <- matrix_conversion(path2_coords)
intersections <- merge(data.frame(path1_matrix), data.frame(path2_matrix))
distance <- abs(intersections[,1]) + abs(intersections[,2])
min <- min(distance)
print(min)

#Part B

calculate_steps <- function(path, intersections) {
  #Convert intestion dataframe to string
  intersection_strings <- paste(intersections[,1], intersections[,2], sep = ",")
  match(intersection_strings, path)
}

steps1 <- calculate_steps(path1_coords, intersections)
steps2 <- calculate_steps(path2_coords, intersections)
total_steps <- steps1 + steps2
min_steps <- min(total_steps)
print(min_steps)