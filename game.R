# file containing the functions to initialize a cluedo game in R

# lists of cards
characters <- c("scarlett", "mustard", "white", "green", "peacock", "plum")
weapons <- c("candlestick", "dagger", "pipe", "revolver", "rope", "wrench")

rooms <- c("hall", "lounge", "dining", "kitchen", "ballroom", "conservatory",
           "billiard", "library", "study")

all_cards <- c(characters, weapons, rooms)

### creating players and game ###

# function to deal cards
dealCards <- function(nr.players, board) {
  # return a nr.players amount of vectors of cards with as even an amount as possible
  # uses lapply to return a list of vectors of cards for each player
  lapply(1:nr.players, function(i) board[seq(i, length(board), by = nr.players)])
} 

# function to create a new player object that has an 'evidence' attribute
# their knowledge about the game
newPlayer <- function(name, hand) {
  evidence <- setNames(rep(NA_character_, length(all_cards)), all_cards)
  # they know their own dealt hand is not in the envelope; mark that with X
  evidence[hand] <- "X"
  
  # return a list of their attributes
  list(
    name = name,
    hand = hand,
    evidence = evidence,
    # will be true only if a wrong final accusation is made
    eliminated = FALSE
  )
}

# function to create players from a vector of player names, using the newPlayer function
createPlayers <- function(player.names, dealt) {
  lapply(seq_along(player.names), function(i) {
    newPlayer(player.names[i], dealt[[i]])
  })
}

# function to create a game
# input: character vector of player names
createGame <- function(player.names) {
  n <- length(player.names)
  # pick 3 random cards to be the solution
  envelope <- list(
    suspect = sample(characters, 1),
    weapon  = sample(weapons, 1),
    room    = sample(rooms, 1)
  )
  # let the rest be the remaining board and shuffle them
  board <- setdiff(all_cards, envelope) |> sample()
  dealt <- dealCards(nr.players = n, board = board)
  
  # create list of players and give them their names
  players <- createPlayers(player.names, dealt)
  names(players) <- player.names
  
  # return game state
  list(
    players = players,
    envelope = envelope,
    # integer index for whose turn it is (player number)
    turn.index = 1L, 
    # integer counter for number of turns
    turn.number = 1L,
    # keep track of game
    game.over = FALSE,
    winner = NULL,
    # log of all suggestion/accusations
    history = list()
  )
}