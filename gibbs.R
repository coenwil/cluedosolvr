# file that handles likelihood of the data and gibbs sampling for cluedosolvr

# computing the likelihood of a given game action
# the event was the observed data, the envelope is current simulated conditional solution
computeLH <- function(event, envelope, prior) {
  
  data <- c(event$suspect, event$weapon, event$room)
  
  # cards in the suggestion that are not in the envelope
  C <- setdiff(data, envelope)
  
  if (event$type == "accusation") {
    # would the accusation have been wrong given the simulated envelope?
    wrong <- !all(data == envelope)
    
    # if yes, the likelihood of observing a wrong accusation is 1, else 0
    return(as.numeric(wrong))
  }
  
  # if there was no refutation
  # can only happen if none of the players hold any card in the suggestion set
  if (is.null(event$refuter)) {
    # if the observed suggestion set is exactly the simulated set, 
    # the likelihood of nonrefutation is 1
    if (length(C) == 0) return(1)
    # initialize likelihood value
    lh <- 1
    # cycle through players in refutation order
    for (player in event$non.refuters) {
      # vector of probabilities of player holding the simulated solution set in the observed data 
      prob.C <- prior[C, player]
      # probability a player don't hold any of them is a multiproduct
      # valid because this is a solution set so must be all different categories
      prob.nonrefute <- prod(1 - prob.C)
      # probability all players nonrefuting is also assumed to be a product
      lh <- lh * prob.nonrefute
    }
    return(lh)
    # if there was a refuter in the observed data
  } else {
    # if the observed suggestion set is exactly the simulated set, 
    # the likelihood of refutation is 0
    if (length(C) == 0) return(0)
    # probability of refuter holding simulated solution set
    prob.C <- prior[C, event$refuter]
    # the probability of refuting if the refuter only has 1 card in common with the
    # simulated envelope set, is the probability of them having that card
    if (length(C) == 1) return(prob.C[1])
    # if they have 2 cards in common, it's the union of probabilities
    if (length(C) == 2) return(prob.C[1] + prob.C[2] - prob.C[1]*prob.C[2])
    # if they have 3 in common, triple union
    if (length(C) == 3) return(prob.C[1] + prob.C[2] + prob.C[3] - 
                                 prob.C[1]*prob.C[2] - prob.C[1]*prob.C[3] - prob.C[2]* prob.C[3])
                                 + prob.C[1]*prob.C[2]*prob.C[3]
  }
  return(lh)
}