# Input
input <- readLines("Day_14/input_snuff.txt")

# Part A

parse_reactions <- function(path) {
  lines <- path
  lines <- lines[nchar(lines) > 0 & !grepl("^\\s*(#|//)", lines)]
  reactions <- list()
  trim <- function(s) gsub("^\\s+|\\s+$", "", s)

  for (ln in lines) {
    parts <- strsplit(ln, "=>", fixed = TRUE)[[1]] #Split In => Out
    lhs <- trim(parts[1])
    rhs <- trim(parts[2])

    # In
    in_specs <- strsplit(lhs, ",\\s*")[[1]]
    in_names <- character(0)
    in_qtys <- numeric(0)

    # Out
    out_parts <- strsplit(rhs, "\\s+")[[1]]
    out_qty <- as.numeric(out_parts[1])
    out_chem <- out_parts[2]

    for (sp in in_specs) {
      p <- strsplit(trim(sp), "\\s+")[[1]]
      in_qtys <- c(in_qtys, as.numeric(p[1]))
      in_names <- c(in_names, p[2])
    }
    reactions[[out_chem]] <- list(qty = out_qty, inputs = setNames(in_qtys, in_names))
  }
  reactions
}

ore_required_for <- function(reactions, target = "FUEL", amount = 1) {
  need <- numeric(0)   #  how much of each chemical we still need
  surplus <- numeric(0) # leftovers
  need[target] <- amount

  take <- function(vec, k) if (k %in% names(vec)) vec[[k]] else 0

  repeat {
    # Pick a chemical (not ORE) that we still need
    pending <- names(need)[need > 0 & names(need) != "ORE"]
    if (length(pending) == 0) break
    chem <- pending[1]
    req <- need[[chem]]

    # Use surplus if available
    s <- take(surplus, chem)
    use <- min(s, req)
    req <- req - use
    surplus[chem] <- s - use

    if (req == 0) {
      need[chem] <- 0
      next
    }

    rxn <- reactions[[chem]]
    batches <- ceiling(req / rxn$qty)
    produced <- batches * rxn$qty
    surplus[chem] <- take(surplus, chem) + (produced - req)

    # Add inputs needed
    for (iname in names(rxn$inputs)) {
      qty <- rxn$inputs[[iname]] * batches
      need[iname] <- take(need, iname) + qty
    }

    need[chem] <- 0
  }

  take(need, "ORE")
}

# Load and solve
reactions <- parse_reactions(input)
ore_for_one_fuel <- ore_required_for(reactions, "FUEL", 1)
print(ore_for_one_fuel)

# Part B
max_fuel_for_ore <- function(reactions, ore_budget = 1e12) {
  ore1 <- ore_required_for(reactions, "FUEL", 1)
  if (ore1 <= 0) stop("Invalid ORE per FUEL")

  lo <- 0
  hi <- (ore_budget %/% ore1) * 2 + 1  # safe upper bound

  while (lo < hi) {
    mid <- (lo + hi + 1) %/% 2
    ore <- ore_required_for(reactions, "FUEL", mid)
    if (ore <= ore_budget) {
      lo <- mid
    } else {
      hi <- mid - 1
    }
  }
  lo
}

ore_budget <- 1000000000000
max_fuel <- max_fuel_for_ore(reactions, ore_budget)
print(max_fuel)