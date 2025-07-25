# Input
input <- readLines("Day_6/input_snuff.txt")

# Part A
data <- do.call(rbind, strsplit(input, ")", fixed = TRUE))
colnames(data) <- c("1", "2")
root <- "COM"

# Wouldn't it be nice to have JSON huh...
build_tree <- function(node, data, depth = 0) {
  children <- data[data[, "1"] == node, "2"]
  if (length(children) == 0) {
    return(list(name = node, depth = depth, children = list()))
  } else {
    child_trees <- lapply(children, function(child) {
      build_tree(child, data, depth + 1) #For explanation of recursion see recursion
    })
    names(child_trees) <- children
    return(list(name = node, depth = depth, children = child_trees))
  }
}
tree <- build_tree(root, data)

count <- function(tree) {
  total <- tree$depth
  for (child in tree$children) { 
    total <- total + count(child)
  }
  return(total)
}
result <- count(tree)
print(result)

# Part B

you <- "YOU"
san <- "SAN"

find_path_to_root <- function(node, tree) {
  if (tree$name == node) return(c(node))
  for (child in tree$children) {
    path <- find_path_to_root(node, child)
    if (!is.null(path)) return(c(tree$name, path))
  }
}

path1 <- find_path_to_root(you, tree)
path2 <- find_path_to_root(san, tree)

# Find the lowest common ancestor
lca <- 0
for (i in seq_along(path1)) {
  if (i <= length(path2) && path1[i] == path2[i]) {
    lca <- path1[i]
  } else {
    break
  }
}

# Calculate the distance
if (lca != 0) {
  distance <- (length(path1) - which(path1 == lca)) + (length(path2) - which(path2 == lca))
  print(distance - 2) # - YOU and SAN themselves
}