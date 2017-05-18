# hidden markov model

#confusionMatrix   przejscia z hidden states na observable states


observableStates <- c("Dry", "Dryish", "Damp", "Soggy") 
hiddenStates <- c("Sunny", "Cloudy", "Rainy")

confMatr<-rbind( c(0.6, 0.2, 0.15, 0.05), c(0.25, 0.25, 0.25, 0.25), c(0.05, 0.1, 0.35, 0.50) )
rownames(confMatr) <- hiddenStates
colnames(confMatr) <- observableStates

confMatr


#State transition matrix - przejscie miedzy stanami (ukrytymi)

stateTransMatr<-rbind( c(0.5, 0.375, 0.125), c(0.25, 0.125, 0.625), c(0.25, 0.375, 0.375) )
rownames(stateTransMatr) <- hiddenStates
colnames(stateTransMatr) <- hiddenStates

stateTransMatr

#TDMC - time discrete markov chain

library("markovchain")

mc1 <- new("markovchain", states = hiddenStates, transitionMatrix = stateTransMatr)

markovchainSequence(n=20, markovchain=mc1, t0 = "Rainy",  include.t0 = T)  #t0 - initial state;  include -> if include initial state

