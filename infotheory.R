# file containing the information theory functions for the cluedo solver

# entropy equation
# accepts a numeric probability vector as input,
# or another vector but then a uniform distribution is used
computeH <- function(oc) {
  p <- if (is.numeric(oc)) oc else rep(1 / length(oc), length(oc))
  # only use elements that are not 0 to avoid nan
  # those elements add or subtract nothing from entropy anyway
  p <- p[p > 0]
  H <-  -1 * sum(p * log(p, base=2))
  return(H)
}

# entropy
computeEntropy <- function(outcomes, ...) {
  # listing all additional outcome vectors and putting them into a vector
  all.outcomes <- c(list(outcomes), list(...))
  # return the sum of the entropies of the different outcome vectors
  # in the case of cluedo this is ok because characters, rooms, weapons are independent
  entropy <- sum(sapply(all.outcomes, computeH))
  return(entropy)
}
