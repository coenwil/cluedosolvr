# file to play a game
# get all files in the github repo in the same directory to get it to work

# helper code to get all functions sourced
# list all files in this directory
helper_files <- list.files(
  pattern = "\\.R$", 
  full.names = TRUE
)
# except itself
helper_files <- helper_files[!grepl("testing\\.R$", helper_files)]
# source all of them
sapply(helper_files, source)


set.seed(37)

# creating a game state called test
test <- createGame(c("Bob", "Alice", "Eve"))
# adding the event of a suggestion to the game state.
# note that bob was the first player in createGame so this would be their suggestion
test <- makeSuggestion(test, "Miss Scarlett", "candlestick", "hall")
# next turn; alice
test <- makeSuggestion(test, "Miss Scarlett", "dagger", "hall")

# eve gets it correct first try!
test <- makeAccusation(test, "Prof. Plum", "dagger", "dining room")
# suggestion of a solution (create another game state first to get a playable round)
test <- makeSuggestion(test, "Prof. Plum", "dagger", "dining room")

# printing out prior/updated marginal probabilities
test$players$Bob$prior
test$players$Alice$prior
test$players$Eve$prior

# code to make a bunch of suggestions
suggestions <- list(
  c("Miss Scarlett", "candlestick", "hall"),
  c("Col. Mustard", "rope", "lounge"),
  c("Mrs. White", "revolver", "kitchen"),
  c("Reverend Green", "pipe", "ballroom"),
  c("Mrs. Peacock", "wrench", "conservatory"),
  c("Prof. Plum", "dagger", "dining room"), # this one is the solution!
  c("Prof. Plum", "candlestick", "library"),
  c("Miss Scarlett", "dagger", "study"),
  c("Col. Mustard", "dagger", "dining room"),
  c("Prof. Plum", "rope", "dining room")
)

# sample a suggestion from the list
suggsamp <- sample(suggestions, 1)
# and use it in a suggestion
test <- makeSuggestion(test, suspect = suggsamp[[1]][1], weapon = suggsamp[[1]][2], room = suggsamp[[1]][3])


# run a gibbs sample on the current game state
samples <- gibbs(test, solver.name = "Bob", n.iter = 1000, burn = 200)

# make a dataframe
sample.df <- do.call(rbind, lapply(samples, function(s) {
  data.frame(suspect = s[[1]], weapon = s[[2]], room = s[[3]], 
             stringsAsFactors = F)
}))

# finding some likelihoods
true.envelope <- c("Prof. Plum", "dagger", "dining room")
wrong.envelope <- c("Miss Scarlett", "revolver", "ballroom")

ll.true <- sum(sapply(test$history, function(e) log(computeLH(e, true.envelope, test$players$Otis$prior))))
ll.wrong <- sum(sapply(test$history, function(e) log(computeLH(e, wrong.envelope, test$players$Otis$prior))))

# and comparing them
cat("true:", ll.true, "\nwrong:", ll.wrong, "\n")

# traceplot
plot(samples$trace, type = "l", xlab = "iteration", ylab = "log likelihood",
     main = "Gibbs sampler trace")
abline(v = 200, col = "red", lty = 2)


