# file to choose an action in cluedo

# creating all possible actions
getActions <- function(characters, weapons, rooms) {
  
  # enumerate all 324 possible suggestions
  all.suggestions <- expand.grid(
    suspect = characters, 
    weapon = weapons, 
    room = rooms)
  
  # make them into 324 lists 
  split(all.suggestions, 1:nrow(all.suggestions))
}

# scoring them on how much overlap with solution set as gotten through posterior joint
# max 3 min 0
scoreAction <- function(action, samples) {
  suggestion.set <- c(action$suspect, action$weapon, action$room)
  
  # for each sample this will be a triplet of booleans
  overlap <- sapply(samples, function(E) {
    sum(suggestion.set == E)
  })
  mean(overlap)
}

bestAction <- function(actions, samples) {
  stats <- t(sapply(actions, function(a) {
    current.set <- c(a$suspect, a$weapon, a$room)
    
    # the best sets have more overlap. for each sample this is a triplet of booleans
    overlap <- sapply(samples, function(E) {
      sum(suggestion.set == E)
    })
    
    c(mean = mean(overlap),
      var = var(overlap))
    
  }))
  
  # first finding the suggestion sets with the best means
  best.mean <- max(stats[, "mean"])
  best.sets <- which(stats, "mean" == best.mean)
  
  # there might be ties to decide that based on overlap variance
  # higher variance means more variation across posterior joint samples, so more uncertainty
  final.set <- best.sets[
    which.max(stats[, "var"][best.sets])
    ]
  
  # output best suggestion set. if still a tie, randomly sample
  actions[[sample(final.set, 1)]]
}
