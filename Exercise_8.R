### Exercise 8

## Using the score-by-score information from this game summarized in 
## "UWvMSU_1-22-13.txt" generate a graph similar to the one shown above.
## Don't worry about how pretty the graph is. Focus on the control structures
## required in the script to generate the plot.

UWvsMSU <-  read.csv("UWvMSU_1-22-13.txt", header = TRUE, sep = "\t", quote = "") # Read in CSV file

UW <- UWvsMSU[which(UWvsMSU[,2] == "UW"),] # Separate the data for UW
MSU <-  UWvsMSU[which(UWvsMSU[,2] == "MSU"),] # Separate the data for MSU

timeUW <- UW[,1]      # Create vector for the times that UW scored
scoreUW <- UW[,3]    # Create vector for scores of UW

timeMSU <- MSU[,1]    # Create a vector for the times that MSU scored
scoreMSU <- MSU[,3]   # Create a vetor for the scores of MSU

# Want to generate a matrix or dataframe with a cumulative score for each team 
# whenever the team scores.

# Cumulative Score for UW

totScoreUW <- numeric(length(timeUW))   # Empty vector to sum the total score in
for (x in 1:length(timeUW)){
  totScoreUW[x] <- sum(scoreUW[1:x])    # Creates vector that contains the cumulative score of the game up until that point
}

# Cumulative Score for MSU

totScoreMSU <- numeric(length(timeMSU)) # Empty vector to sum the total score in
for (y in 1:length(timeMSU)){
  totScoreMSU[y] <- sum(scoreMSU[1:y])   # Creates vector that contains the cumulative score of the game up until that point
}

# For plotting, use plot(x,y,type='1') in base package. You can add a second 
# line to the graph with lines(x,y). 

plot(timeMSU,totScoreMSU,type="l",col="green", ann = FALSE)
lines(timeUW,totScoreUW,col="red", xlab = "Time (min)", ylab = "Total Points")
title(main="Score of MSU vs UW game over time", xlab= "Time (min)", ylab="Total Points")
legend("topleft",legend=c("MSU","UW"),col = c("green","red"), lty=1:1)




## Write a game called "guess my number". This computer game will generate a 
## randow number between 1 and 100. The used types in a number and the computer
## replies with "lower", "higher", or "correct!". The player can continue   
## guessing up to 10 times. (sample()= random number generator)


## TO PLAY THE GAME, CALL THE FUNCTION "PlayGuessMyNumber()" ##

PlayGuessMyNumber <- function(){      # Main game function, call it to play the game
readInteger <- function()             # Function to prompt the user for a guess, called within the while loop
{
  n <- readline(prompt="Guess: ")     # Prompts the user to guess a number
  if(!grep("[0-9]+$",n))
  {
    return(readInteger())
  }
  return(as.integer(n))
}

num <- sample(1:100,1)                # Generates a random number between 1 and 100
guess <- -1                           # Initializes the users guess
numGuess <- 1                         # Initializes the counter variable

print("I'm thinking of a number 1-100...")   # Initial prompt for the user

while(numGuess < 11)                  # Gives the user 10 guesses, while loop ends after 10 guesses
{
  print(paste0("---- Attempt#",numGuess)) # Tells the user what attempt they are on
  guess <- readInteger()              # Reads in the user's guess as input
  
  if (guess == num)                   # Conditional statements
  {
    print("Correct!")
    numGuess <- 11                    # Exit the loop if the user picks the right number  
  }
  else if (guess < num)               # Guess is less than the right answer
  {
    print("Higher")
  }
  else if (guess > num)               # Guess is lower than the right answer
  {
    print("Lower")
  }
  numGuess = numGuess + 1             # Increments counter variable
}

 if (guess != num){
print(paste0("You're out of guesses! The answer was ",num)) # If the user did not guess the answer within 10 tries, the
 }                                                          # answer is revealed
}
