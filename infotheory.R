# file containing the information theory functions for the cluedo solver

# entropy
computeEntropy <- function(outcomes, ...) {
  # listing all additional outcome vectors and putting them into a vector
  all_outcomes <- c(list(outcomes), list(...))
  # entropy equation
  # TODO: let it accept probability vector
  computeH <- function(oc) {
    p <- rep(1 / length(oc), length(oc))
    H <-  -1 * sum(p * log(p, base=2))
    return(H)
  }
  # return the sum of the entropies of the different outcome vectors
  return(sum(sapply(all_outcomes, computeH)))
}