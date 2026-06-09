# file that handles the updating of priors using bayes factors for cluedosolvr

updatePrior <- function(state, event, solver.name, opponents) {
  # event is passed by makeSuggestion which calls this function
  # opponents is also defined by makeSuggestion
  
  # handle accusations
  # do nothing if correct, because the game is then over
  if (event$type == "accusation") {
    prior <- state$players[[solver.name]]$prior
    
    # if incorrect, update probabilities of cards being in envelope
    if (!event$correct) {
    suggestion <- c(event$suspect, event$weapon, event$room)
    
    for (card in accusation) {
      prob.envelope <- prior(card, "envelope")
      # skip if probability already 0
      if (prob.envelope == 0) next
      other.cards <- setdiff(accusation, card)
      bf <- 1 - (prior[other.cards[1], "envelope"] * prior[other.cards[2], "envelope"])
      # convert prior probability to odds for multiplication
      prior.odds <- prob.envelope / (1 - prob.envelope)
      posterior.odds <- bf * prior.odds
      # and back to probability for matrix
      prior[card, "envelope"] <- posterior.odds / (posterior.odds + 1)
    }
    # assign updated matrix to prior matrix
    state$players[[solver.name]]$prior <- prior  
    }
  # exit function
  return(state)
  }
  
  suggestion <- c(event$suspect, event$weapon, event$room)
  # current player's prior matrix
  prior <- state$players[[solver.name]]$prior
  
  # case 1: no refutation. this means they must not have any of the cards in the suggestion
  # looping over the next players according to the turns
  for (opponent in opponents) {
    # if somebody becomes a refuter, break the loop and go to refutation loop below
    # next players do not get considered
    if (!is.null(event$refuter) && opponent == event$refuter) break
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
      # and so it cannot be in the envelope or in the other player's hands
      prior[event$card.shown, setdiff(colnames(prior), event$refuter)] <- 0
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
  
  # check if now there is a card certainly in the envelope from a category, if yes make all others 0
  for (category in list(characters, weapons, rooms)) {
    envelope.probs <- prior[category, "envelope"]
    
    # use .999 to avoid floating point arithmetic bugs
    if (any(envelope.probs > .999)) {
      known <- category[envelope.probs == 1]
      # the unknown cards in the category get 0
      prior[setdiff(category, known), "envelope"] <- 0
    }
  }
  # update the prior with the posterior probabilities, will be the prior for the next event
  state$players[[solver.name]]$prior <- prior
  state
}
