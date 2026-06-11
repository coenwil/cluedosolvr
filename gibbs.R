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
}

# gibbs sampler
gibbs <- function(state, solver.name, n.iter = 1000, burn = 200) {
  prior <- state$players[[solver.name]]$prior
  history <- state$history
  
  # using log likelihoods so i can sum
  logLH <- function(envelope) {
    # init value
    llh <- 0
    # compute likelihood for every event
    for (event in history) {
      lh <- computeLH(event, envelope, prior)
      llh <- llh + log(lh)
    }
    llh
  }
  # initial cards are sampled from marginal posteriors
  samp.s <- sample(characters, 1, prob = prior[characters, "envelope"])
  samp.w <- sample(weapons, 1, prob = prior[weapons, "envelope"])
  samp.r <- sample(rooms, 1, prob = prior[rooms, "envelope"])
  
  # creating an outcome vector of lists of most likely solution sets
  samples <- vector("list", n.iter) 
  
  # gibbs sampling loop
  for (h in 1:n.iter) {
    
    # sample suspect conditional on weapon and room
    cond.envelope <- c(samp.w, samp.r)
    # log conditional posterior of suspect
    log.condpost <- sapply(characters, function(s) {
      # creating the simulated solution set, loop over all 6
      envelope <- c(s, cond.envelope)
      # addition is logged multiplication
      logLH(envelope) + log(prior[s, "envelope"])
    })
    # sample new suspect with probability equal to conditional posterior
    samp.s <- sample(characters, 1, prob = exp(log.condpost))
    
    # sample weapon given suspect and room
    cond.envelope <- c(samp.s, samp.r)
    log.condpost <- sapply(weapons, function(w) {
      envelope <- c(samp.s, w, samp.r)
      logLH(envelope) + log(prior[s, "envelope"])
    })
    # sample new weapon for next iteration
    samp.w <- sample(weapons, 1, prob = exp(log.condpost))
    
    # sample room given suspect and weapon
    cond.envelope <- c(samp.s, samp.w)
    log.condpost <- sapply(rooms, function(r) {
      envelope <- c(samp.s, samp.w, r)
      logLH(envelope) + log(prior[r, "envelope"])
    })
    # sample new room
    samp.r <- sample(rooms, 1, prob = exp(log.condpost))
    
    # one sample of a full solution set goes into the outcome vector
    samples[[t]] <- c(samp.s, samp.w, samp.r)
  }
  
  # removing burn in and outputting samples from joint
  samples[(burn + 1):n.iter]
}









