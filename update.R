# file that handles the updating of priors using bayes factors for cluedosolvr

updatePrior <- function(state, event, solver.name, opponents) {
  # event is passed by makeSuggestion which calls this function
  # opponents is also defined by makeSuggestion
  
  # do nothing if the type of event is not a suggestion
  # only suggestions carry potential (non)refutation information
  if (event$type != "suggestion") return(state)
  
  suggestion <- c(event$suspect, event$weapon, event$room)
  # current player's prior matrix
  prior <- state$players[[solver.name]]$prior
  
  # case 1: no refutation. this means they must not have any of the cards in the suggestion
  # looping over the next players according to the turns
  for (opponent in opponents) {
    # if somebody becomes a refuter, break the loop and go to refutation loop below
    # next players do not get considered
    if (opponent == event$refuter) break
    for (card in suggestion) {
      prior[card, opponent] <- 0
    }
  }
  
  # if no one refuted, suggested cards must be in envelope
  if (is.null(event$refuter)) {
    for (card in suggestion) {
      player.probs <- prior[card, names(state$players)]
      # check if the previous loop set all player probabilities to 0
      if (all(player.probs == 0)) {
        prior[card, "envelope"] <- 1
      }
    }
  }
  
  # case 2: refutation. somebody has at least one of the cards in the suggestion
  # check if the solver is also the suggester, which means they get to see the card
  if (!is.null(event$refuter)) {
    if (solver.name == event$player) {
      # then you know for sure which card the refuter has in their hand
      prior[event$card.shown, event$refuter] <- 1
      # and so it cannot be in the envelope
      prior[event$card.shown, "envelope"] <- 0
    } else {
      # if the solver can only observe the suggestion, they do not see the card
      # check if you already know that a suggested card cannot be in the envelope
      # if yes, skip. 
      for (card in suggestion) {
        prob.envelope <- prior[card, "envelope"]
        # do nothing if probability in envelope already 0
        if (prob.envelope == 0) next
        
        # look up the probabilities of the other 2 cards in the suggestion being in the refuters hand
        other.cards <- setdiff(suggestion, card)
        # is a vector
        prob.others <- prior[other.cards, event$refuter]
        
        # likelihood of refuter refuting given that the current card in the loop is in the envelope
        # prob of them holding at least one of the other 2
        lh.in <- prob.others[1] + prob.others[2] - (prob.others[1] * prob.others[2])
        
        # likelihood of refuter refuting given that current card is NOT in the envelope
        # prob of them holding at least one of the 3
        prob.card <- prior[card, event$refuter]
        lh.out <- prob.card + prob.others[1] + prob.others[2] - 
          (prob.card * prob.others[1]) - (prob.card * prob.others[2]) - (prob.others[1] * prob.others[2]) +
          (prob.card * prob.others[1] * prob.others[2])
        
        # computing bayes factors from the likelihoods for the envelope column
        bf <- lh.in / lh.out
        prior.odds <- prob.envelope / (1 - prob.envelope)
        posterior.odds <- bf * prior.odds
        
        # normalize back to posterior probability for evidence matrix
        prior[card, "envelope"] <- posterior.odds / (1 + posterior.odds)
        
        # updating column of refuter
        prob.refuter <- prior[card, event$refuter]
        # skip if current card is already known anyway
        if (prob.refuter == 0) next
        bf.refuter <- 1 / lh.in
        prior.odds.refuter <- prob.refuter / (1 - prob.refuter)
        # posterior odds
        posterior.odds.refuter <- bf.refuter * prior.odds.refuter
        prior[card, event$refuter] <- posterior.odds.refuter / (1 + posterior.odds.refuter)
      }
    }
  }
  # update the prior with the posterior probabilities, will be the prior for the next event
  state$players[[solver.name]]$prior <- prior
  state
}
