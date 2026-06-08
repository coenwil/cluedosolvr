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
  # all cards that the solver does not have in their hand get probability 0
  mat[setdiff(all_cards, hand), solver.name] <- 0
  # all cards that the solver has in their hand get probability 0 for the other players
  mat[hand, solver.name != col.names] <- 0
  
  # prior probabilities of envelope: uniform over unknown cards
  for (category in list(characters, weapons, rooms)) {
    unknown <- setdiff(category, hand)
    mat[unknown, "envelope"] <- 1 / length(unknown)
    # do the same for the cards in each opponents hand
    # more complicated here since envelope is constrained to have 1 card of each category, but players can have multiple
    # so the equation for uniform probabilities over the players but allowing them to have multiple is 
    # (1 / nr of players - 1 to account for each player knowing their own hand) * ((nr of unknown cards - 1 thats in the envelope) / (total number of unknown cards))
    mat[unknown, !colnames(mat) %in% c("envelope", solver.name)] <- (1 / (length(player.names) - 1)) * ((length(unknown) - 1) / length(unknown))
    }
  mat
}
