# helper files for the cluedo game
# functions that print states, for example

# print a summary of the game
printState <- function(state) {
  cat("--- GAME STATE ---\n")
  cat(sprintf("turn: %d | current player: %s\n",
              state$turn.number, currentPlayer(state)))
  cat(sprintf("game over: %s", state$game.over))
  if (!is.null(state$winner)) cat(sprintf(" | winner: %s", state$winner))
  
  cat("\nplayers:\n")
  
  for (i in state$players) {
    cat(sprintf("%-20s %s\n",
                i$name, if (i$eliminated) " [ELIMINATED]" else " [ACTIVE]"))
  }
}

# print the hand of all or a singular player
printHands <- function(state, player = "all") {
  cat("--- PLAYER HANDS ---\n")
  
  if (player == "all") {
    for (i in state$players) {
      cat(sprintf("\n%-20s \n[%s]\n",
                  i$name, paste(i$hand, collapse = ", ")))
    }
  } else {
    cat(sprintf("\n%s \n[%s]\n",
                player, paste(state$players[[player]]$hand, collapse = ", ")))
  }
}

# print a player's evidence matrix
printEvidence <- function(state, player.name) {
  ev <- state$players[[player.name]]$evidence
  cat(sprintf("%s's evidence:\n", player.name))
  cat("X = card is confirmed not in envelope\n")
  cat("? = card has been suggested but not refuted\n")
  cat("- = card has not been investigated\n")
  
  # internal function to print each of the categories of cards separately
  printSection <- function(cards, category) {
    cat(category, ":\n", sep = "")
    for (card in cards) {
      # make sure there is a - for NAs in the evidence
      symbol <- if (is.na(ev[card])) "-" else ev[card]
      
      cat(sprintf("[%s] %s\n", symbol, card))
    }
  }
  printSection(characters, "Characters")
  printSection(weapons, "Weapons")
  printsection(rooms, "Rooms")
}

# print full suggestion/accusation history
printHistory <- function(state) {
  cat("--- GAME HISTORY ---\n")
  # print all events in history
  for (event in state$history) {
    if (event$type == "suggestion") {
      refute.answ <- if (!is.null(event$refuter)) {
        sprintf("refuted by %s", event$refuter) } else "nobody refuted"
      
      cat(sprintf("T%02d [suggestion] %-20s: %s, %s, %s (%s)\n",
                  event$turn, event$player, 
                  event$suspect, event$weapon, event$room,
                  refute.answ))
    } else {
      cat(sprintf("T%02d [accusation] %-20s: %s, %s, %s [%s]\n",
                  event$turn, event$player,
                  event$suspect, event$weapon, event$room,
                  if (event$correct) "CORRECT" else "WRONG"))
    }
  }
}
