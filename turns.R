# file containing functions for turn logic in cluedo

# get the name of the player whose turn it is to make a suggestion
currentPlayer <- function(state) {
  # state will be the output of createGame
  names(state$players)[state$turn.index]
}

# validate card to prevent spelling mistakes
# return an error message that says that there is no such card among the weapons/rooms/characters
validateCard <- function(card, category, valid.set) {
  if (!card %in% all_cards)
    stop(sprintf("'%s' is not a valid %s.\n Choose from: %s",
                 card, category, paste(valid.set, collapse = ", ")))
}

# function to advance turns
advanceTurn <- function(state) {
  n <- length(state$players)
  # increment turn number
  state$turn.number <- state$turn.number + 1L
  
  for (i in seq_len(n)) {
    # next player id is this modulo equation
    next.idx <- (state$turn.index %% n) + 1L
    # increment turn index 
    state$turn.index <- next.idx
    # stop looking for the next id if the next player is not eliminated
    # else increment id by 1 to skip the eliminated player
    if (!state$players[[next.idx]]$eliminated) break
  }
  state
}

# a suggestion is a non-final rumor that is done every turn
# TODO: make sure input is character or convert it
makeSuggestion <- function(state, suspect, weapon, room) {
  # if game is over already, error message
  if (state$game.over) {message("Game is over! Cannot make another suggestion.");
    return(state)}
  
  # validate if the suggestion is possible
  validateCard(card = suspect, category = "suspect", valid.set = characters)
  validateCard(card = weapon, category = "weapon", valid.set = weapons)
  validateCard(card = room, category = "room", valid.set = rooms)
  
  suggester <- currentPlayer(state)
  suggestion <- c(suspect, weapon, room)
  n <- length(state$players)
  player.names <- names(state$players)
  
  correct <- setequal(state$envelope, suggestion)
  
  cat(sprintf(
    "[Turn %d]\n %s suggests: %s, %s, in the %s\n",
    state$turn.number, suggester, suspect, weapon, room
  ))
  
  # other players refute in turn order
  refuter <- NULL
  card.shown <- NULL
  
  # turn order: everyone after the suggester, clockwise
  
  # how many people need to refute after the suggestion
  n.after <- seq_len(n-1)
  
  # getting a vector of the indices that need to go after the suggester
  # wraps clockwise
  idxs <- setdiff(c(state$turn.index:n, 1:n), state$turn.index)
  
  opponents <- player.names[idxs]
  # remove players who have been eliminated
  opponents <- opponents[!sapply(opponents, function(i) state$players[[i]]$eliminated)]
  
  # initialize a vector of nonrefuters for later use
  non.refuters <- c()
  
  # round of refusing
  for (opponent in opponents) {
    matching <-  intersect(state$players[[opponent]]$hand, suggestion)
    
    if (length(matching) > 0) {
      # show a random matching card
      # TODO: allow for choice of what to show
      card.shown <- sample(matching, 1)
      refuter <- opponent
      # add card shown to evidence matrix of suggester
      state$players[[suggester]]$evidence[card.shown] <- "X"
      cat(sprintf(" %s refutes by showing a card to %s.\n", refuter, suggester))
      break
    } else { 
      cat(sprintf(" %s cannot refute.\n", opponent)) 
      non.refuters <- append(non.refuters, opponent)
    }
    
  }
  
  # refuter will remain NULL if nobody has a matching card
  if (is.null(refuter)) {
    cat("Nobody could refuse the suggestion!\n")
    
    # mark the cards as unrefuted in evidence matrix of suggester
    for (card in suggestion) {
      if (is.na(state$players[[suggester]]$evidence[card])) {
        state$players[[suggester]]$evidence[card] <- "?"
      }
    }
  }
  # log the event
  event <- list(
    turn = state$turn.index,
    type = "suggestion",
    player = suggester,
    suspect = suspect,
    weapon = weapon,
    room = room,
    refuter = refuter,
    non.refuters = non.refuters,
    card.shown = card.shown
  )
  state$history <- append(state$history, list(event))
  
  # update priors for all players based on the suggestion event
  for (name in names(state$players)) {
    state <- updatePrior(state, event, solver.name = name, opponents)
  }
  
  # next turn
  state <- advanceTurn(state)
  state
}

# function to make a final accusation
makeAccusation <- function(state, suspect, weapon, room) {
  if (state$game.over) { message("Game is already over."); return(state) }
  
  validateCard(card = suspect, category = "suspect", valid.set = characters)
  validateCard(card = weapon, category = "weapon", valid.set = weapons)
  validateCard(card = room, category = "room", valid.set = rooms) 
  
  accuser <- currentPlayer(state) 
  
  suggestion <- c(suspect, weapon, room)
  correct <- setequal(state$envelope, suggestion)
  
  cat(sprintf("[Turn %d]\n %s ACCUSES: %s, %s, in the %s.\n %s!",
              state$turn.index, accuser, suspect, weapon, room,
              if (correct) "CORRECT" else "WRONG")) 
  
  event <- list(
    turn = state$turn.index,
    type = "accusation",
    player = accuser,
    suspect = suspect,
    weapon = weapon,
    room = room,
    correct = correct
  )
  state$history <- append(state$history, list(event))
  
  if (correct) {
    state$game.over <- TRUE
    state$winner <- accuser
    cat(sprintf(" %s wins the game!", accuser))
  } else {
    # eliminate player
    state$players[[accuser]]$eliminated <- TRUE
    # filter out only the players who are not elimiated
    # Filter() applies function to a list and keeps elements where TRUE
    # (so using ! for the inverse here)
    active <- Filter(function(pl) !pl$eliminated, state$players)
    
    if (length(active) == 0) {
      state$game.over <- TRUE
      cat(" All players have been eliminated. Nobody wins.")
      cat(sprintf(" The answer was: %s, %s, in the %s",
                  state$envelope$suspect, state$envelope$weapon, state$envelope$room))
    }
    else {
      state <- advanceTurn(state)
    }
  }
  state
}
