#**********************************
#Montecarlo simulation
#**********************************
#rep() creates an 'urn' from which we pick beads
beads <- rep(c("red","blue"), times = c(2,3))
beads

#sample() picks something at random WITHOUT replacement!
sample(beads, 1)

#replicate() repeats a task a number of times
B <- 10000
events <- replicate(B, sample(beads, 1))

#table() to see the distribution
tab <- table(events)

#prop.table() to see the proportions of the ditribution
prop.table(tab)

#***Alternatively***
#We can use sample WITH replacement and not use the replciate function
events2 <- sample(beads, B, replace = TRUE)
tab2 <- table(events2)
prop.table(tab2)


#**********************************
#Independence
#**********************************

#Pr(A and B) = Pr(A)*Pr(B|A)  B|A means probability of B given that A has hapened



#**********************************
#Combinations - Creating a 'deck of cards'
#**********************************
#paste() concatenates strings
paste(letters[1:5], as.character(1:5))

#expand.grid() gives us the combination of two lists
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

#Now generate a desk of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck

#Concat the columns
card_deck <- paste(deck$number, deck$suit)
card_deck


#Probability of drawing a king from the deck
kings <- paste("King", suits)
kings
mean(card_deck %in% kings)


#permutations(), in gtools package, computes for a list of n items, all the different ways of picking r items (without replacement)
#Order matters!
library(gtools)
permutations(5,2)

#Permutations of two-card hand from card_deck
hands <- permutations(52, 2, v = card_deck)
hands

first_card <- hands[,1] #first column
second_card <- hands[,2] #second column

#Probability of getting a king in the first card and a king in the second card
# should equate to 3 in 51
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)
#also
mean(first_card %in% kings & second_card %in% kings) / mean(first_card %in% kings)




#Combinations - order does not matter
combinations(3,2)

#To compute the probability of getting a natural 21 in blackjack:
aces <- paste("Ace", suits)

facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)
facecard

#generate all posible two card hands
hands <- combinations(52, 2, v = card_deck)

#What is the probability of drawing 21 out of these hands?
mean(hands[,1] %in% aces & hands[,2] %in% facecard)




#What are the chances of sharing the same birthday in a room of 50 people?
#Birthdays can be represented as numbers between 1 and 365

#50 random birthdays
n <- 50
bdays <- sample(1:365, n, replace = TRUE)

#Are there any duplicated birthdays in our dataset?
any(duplicated(bdays))

#Monte carlo simulation
results <- replicate(B, {
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})

#now the probability
mean(results)

#Turn this into a function
compute_prob <- function(n, B=1000){
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

#Work out what the probability for sharing a birthday is for the first 60 days
n <- seq(1:60)

#sapply allows us to apply element-wise operation on any function
prob <- sapply(n, compute_prob)
plot(n,prob)

#Now to calculate the exact probaility
#The first person will have a probability of NOT sharing a birthday of 1
#The second person will have a probability of NOT sharing a birthday of 364/365
#The third person will have a probability of NOT sharing a birthday of 363/365 etc...
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365
  1 - prob_unique
}

eprob <- sapply(n, exact_prob)

plot(n, eprob)




#**********************************
#Addition rule
#**********************************

#Pr(A or B) = Pr(A) + Pr(B) - Pr(A and B)

#Probaility of Ace followed by face card
1/13 * 16/51

#Probability of Face card followed by Ace
16/52 * 4/51



#**********************************
#Monty Hall Three Door problem
#**********************************
#Three doors, one is removed. Choice between staying with original choice, or switching.

#Stay with original choice
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat" , "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  stick <- my_pick
  stick == prize_door
})

mean(stick)


#Switch to other door
B <- 10000
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat" , "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  stick <- my_pick
  switch <- doors[!doors %in% c(my_pick, show)]
  switch == prize_door
})

mean(switch)




#**********************************
#Continuous probability
#**********************************
#It is useful to define a distribution that operates on intervals instead of individual values
#This is known as a "Cumulitve Distibution function (CDF)" or "Empirical Cumultive Distribution Function (eCDF)

library(tidyverse)
library(dslabs)
data(heights)

x <- heights %>% filter(sex == "Male") %>% .$height

#CDF
F <- function(a) mean(x <= a)

#What is the proportion (probability) of males with height GREATER than 70 inches
1 - F(70)


#CDF for normal distribution
#F(a) = pnorm(a, avg, std_dev)
1 - pnorm(70.5, mean(x), sd(x))


#dnorm() is the probability density function for the normal distribution (the line that the bell curve follows)

#rnorm() generates a random normally distributed data
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

ds_theme_set()
data.frame(simulated_heights=simulated_heights) %>% ggplot(aes(simulated_heights)) +
  geom_histogram(color = "black", binwidth = 2)


#Monte Carlo to work out prob of how rare seven-foot tall men are in a pool of 800
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)
  max(simulated_data)
})

mean(tallest >= 7*12)




#**********************************
#Sampling
#**********************************
#A roulette wheel has 18 red, 18 black and 2 green - 0 and 00 (in American roulette).
#Create an 'urn' to represent a roulette wheel
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2))

#1000 independent draws. Gambler wins $1 on red, loses $1 on not-red.
n <- 1000
X <- sample(ifelse( color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

#Or in 1 line of code
X <- sample(c(-1, 1), n, replace = TRUE, prob = c(18/38, 20/38))
S <- sum(X)
S

#Now we repeat the above in a monte carlo simulationn to work out how many times the casino lost money (S < 0)
n <- 1000 #1000 players
B <- 10000 #Iterations of the simulation
S <- replicate(B, {
	X <- sample(c(-1, 1), n, replace = TRUE, prob = c(18/38, 20/38))
	sum(X)
})
#Drum roll
a <- 0 #Our target value
mean(S <= a)

#What is the average amount of money the casino wins? This is the 'Expected Value'
mean(S)

#and standard deviation? This is the 'Standard Error'
sd(S)




#**********************************
# The Big Short
#**********************************
# Setting bank interest rates
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02

defaults <- sample(c(0,1), n, prob = c(1-p, p), replace = TRUE)

sum(defaults * loss_per_foreclosure)



# Now estimate as a Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample(c(0,1), n, prob = c(1-p, p), replace = TRUE)
  sum(defaults * loss_per_foreclosure)
})

#Plot the distribution
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>% ggplot(aes(losses_in_millions)) + geom_histogram(binwidth = 0.6, col="Black")


# Using the Central Limit Theorem
expected_value <- n * (p * loss_per_foreclosure + (1-p) * 0)
standard_deviation <- sqrt(n) * abs(loss_per_foreclosure) * sqrt(p * (1-p))

expected_value
standard_deviation

# The breakeven point is where the expected value is zero
# i.e. l*p + x*(1-p) = 0
# where l = loss per forclosure and x = our target value

x <- -loss_per_foreclosure * p/(1-p)
x

#This is the interest rate to set to break even
x / -loss_per_foreclosure * 100

# Say we want a probablity of loss = 0.01
# Pr(S < 0) = 0.01
# Trick is to subract expected value, then divide by standard error on both sides of the equation
# Pr((S - expected_value) / standard_deviation < (0 - expected_value) / standard_deviation)
# This becomes a 'Z' score
# Pr(Z < (0 - expected_value) / standard_deviation)
# Z score has expected value 0 and standard error of 1. i.e. qnorm(0.01)

l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*(n*p - z*sqrt(n*p*(1-p))) / (n*(1-p) + z*sqrt(n*p*(1-p)))
x

# New interest rate
x / -loss_per_foreclosure * 100

# Expected profit per loan
loss_per_foreclosure*p + x*(1-p)

# Total expected profit
n * (loss_per_foreclosure*p + x*(1-p))