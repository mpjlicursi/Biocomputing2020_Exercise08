#Biocomputing Exercise 08, Matthew Licursi

#Question 1

#Produce a plot summarizing the cumulative score by each basketball team as a
#function of time in the game

#Read the file "UWvMSU_1-22-13.txt"
game<-read.table(file="UWvMSU_1-22-13.txt", header=TRUE)
game

#Organize the time and score data to be separated by team
#For Score
UWScoreData<-cumsum(game[,3][game[,2] == "UW"])
MSUScoreData<-cumsum(game[,3][game[,2] == "MSU"])

#For Time
UWTimeData<-game[,1][game[,2] == "UW"]
MSUTimeData<-game[,1][game[,2] == "MSU"]

#Create Dataframes for each team
UWdataframe<-data.frame(x<-UWTimeData[], y<-UWScoreData[])
MSUdataframe<-data.frame(x<-MSUTimeData[], y<-MSUScoreData[])

#Creates Plot
plot(UWdataframe$x, UWdataframe$y, type = "l", col = "red", ylab = "Score", xlab = "Time")
#Adds another line to the plot for the other team
lines(MSUdataframe$x, MSUdataframe$y, col = "blue")



#Question 2

#A "Guess my number" game. The computer generates a number between 1 and 100.
#The user types a number and the computer replies "lower" if the answer is lower, "higher" if the answer
#is higher, and "correct" if the answer is correct. The player has 10 attempts.

GuessMyNumberGame<-function(x){
  MaxGuesses<-10 #10 Max guesses. The "10" can be altered if one desires a game with more attempts
  Count<-1 #Start count at 1
  IntegerSet<-c(1:100) #Set of 100 Integers
  RandomInteger<-sample(IntegerSet, 1) #Picks a random integer in the set
  guess<-as.numeric(readline(prompt = "I'm thinking of a number 1-100...Guess?"))

#The While loop below delivers certain outputs ("Lower", "Higher", or "Correct") based on
#what the guess is relative to the randomly generated number

#To play the game, execute the following code and then run the "GuessMyNumberGame" in the console
  
    while(Count<MaxGuesses) {
    if(guess>RandomInteger){     #statement for if the guess is greater than the random integer
      print("Lower")
      a<-as.numeric(readline("Try again:"))
      guess<-as.numeric(a)
      Count<-Count+1             #counts the number of guesses
    }else if(guess<RandomInteger){
        print("Higher")
        a<-as.numeric(readline("Try again:"))
        guess<-as.numeric(a)
        Count<-Count+1          #counts the number of guesses
    }else if(guess==RandomInteger){
        print("Correct!")
        opt<-options(show.error.messages = FALSE)
        stop()
    }
}
  while(Count==MaxGuesses){ #while loop runs if the user is out of guesses
    if(guess!=RandomInteger){
    readline("Out of Guesses!")
    }else if(guess==RandomInteger){
      readline("Correct!")
    }
  }
}

