# cluedo card information matrices

# plays from the perspective of one of the players
priorMatrix <- function(state, solver.name) {
  player.names <- names(state$players)
  # also create a column for the envelope
  col.names <- c(player.names, "envelope")
  
  mat <- as.data.frame(
    matrix(NA, nrow = length(all_cards), ncol = length(col.names),
           dimnames = list(all_cards, col.names))
  )
  hand <- state$players[[solver.name]]$hand
  # all cards that the solver has in their hand get probability 1
  mat[hand, solver.name] <- 1
  # all cards that the solver has in their hand get probability 0 for the other players
  mat[hand, solver.name != col.names] <- 0
  
  # prior probabilities of envelope: uniform over unknown cards
  for (category in list(characters, weapons, rooms)) {
    unknown <- setdiff(category, hand)
    mat[unknown, "envelope"] <- 1 / length(unknown)
  }
  mat
}

game <- createGame(c("Alice", "Bob", "Charlie", "Carol", "Ed"))
priorMatrix(game, "Bob")
testmat <- matrix(letters[1:25], nrow = 5)
testmat[1,1]
