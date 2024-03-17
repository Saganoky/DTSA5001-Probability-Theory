#define the vectors for size, topping and order
size = c("S", "M", "L")
topping = c("pepperoni", "sausage", "meatball", "extra cheese")
order = c("deliver", "pick-up")

#keep track of the pizzas
pizzas = character(0)

#iterate over each value for each variable
for(i in 1:length(size)){
  for(j in 1:length(topping)){
    for(k in 1:length(order)){
      
      #create a pizza
      pizzas = rbind(pizzas, c(size[i], topping[j], order[k]))
    }
  }
}

#print out the pizzas; should have 24
pizzas

#count total number of large sausages; should get 2
#   we divide by 3 because rows are length 3, and we want
#   to convert back to number of rows (i.e., number of pizzas)
Number_Of_Large_Sausage = length(pizzas[pizzas[ ,1] == "L" & pizzas[ ,2] == "sausage"])/3

# install combinat package so the following code works
install.packages("combinat")

#generate all of the possible permutations
perms = combinat::permn(c("A", "B", "C", "D", "E", "F", "G"))

#look at the first few permutations
head(perms)

#should get factorial(7) = 5040
length(perms)

# install gtools package
install.packages("gtools")

#generate the committees (people labeled 1 to 5)
committees = gtools::combinations(n = 5, r = 3)

#should get choose(5, 3) = factorial(5)/(factorial(3)*factorial(2)) = 10 committees
committees


### part a. ###
#should get 6^4 = 1296
dim(gtools::permutations(n = 6, r = 4, repeats.allowed = TRUE))[1]

## [1] 1296


### part b. ###
#should get factorial(6)/factorial(2) = 360
dim(gtools::permutations(n = 6, r = 4, repeats.allowed = FALSE))[1]

## [1] 360


## part c. ##
#find the unique passcodes
uniques = gtools::permutations(n = 6, r = 4, repeats.allowed = FALSE)

#create unique and ascending passcodes
uniques.ascending = apply(uniques, 1, function(x) sort(x))

#orient the matrix (the function t() just transposes a matrix; flips it on it's side)
uniques.ascending = t(uniques.ascending)

#create a data frame
uniques.ascending = data.frame(uniques.ascending)

#remove duplicates; should get choose(6, 4) = 15
dim(unique(uniques.ascending))[1]

## [1] 15


## part d. ##
#find all passcodess
passcodes = gtools::permutations(n = 6, r = 4, repeats.allowed = TRUE)

#create ascending passcodes
ascending = apply(passcodes, 1, function(x) sort(x))

#orient the matrix (the function t() just transposes a matrix; flips it on it's side)
ascending = t(ascending)

#create a data frame
ascending = data.frame(ascending)

#remove duplicates; should get choose(6 + 4 - 1, 4) = 126
dim(unique(ascending))[1]

## [1] 126


#generate the committees (people labeled 1 to 5)
committees2 = gtools::combinations(n = 3, r = 2)
committees1 = gtools::combinations(n = 3, r = 1)

#print the sets of the two committees
committees2; committees1

##      [,1] [,2]
## [1,]    1    2
## [2,]    1    3
## [3,]    2    3

##      [,1]
## [1,]    1
## [2,]    2
## [3,]    3


#label people 1 to 4
people = 1:4

#first, we do the president-first approach
#initialize the committees
committees1 = character(0)

#iterate through potential presidents
for(i in 1:length(people)){
  
  #generate the new committes, with i as the president
  new.committees = cbind(i, gtools::combinations(n = length(people) - 1, r = 2, v = people[-i]))
  
  #add on to existing committees
  committees1 = rbind(committees1, new.committees)
}

  
#remove column names
colnames(committees1) = c("", "", "")



#second, we do the committee-first approach; initialize here
committees2 = character(0)

#generate all of the committees, without presidents
new.committees = gtools::combinations(n = 4, r = 3)

#iterate through the committees and add presidents
for(i in 1:dim(new.committees)[1]){
  
  #pick each person as president
  for(j in 1:3){
    
    #add the committee
    committees2 = rbind(committees2, c(new.committees[i, j], new.committees[i, -j]))
  
  }
}

#remove column names
colnames(committees2) = c("", "", "")

#print the two groups of committees; should be the same size
#   the first person is president in each case
#we should have k*choose(n, k) = 3*choose(4, 3) = 12 total
committees1

committees2



#replicate
set.seed(110)
sims = 1000

#create a deck; define spades as 1, everything else as 0
deck = c(rep(1, 13), rep(0, 52 - 13))

#keep track of the number of spades we sample
spades = rep(NA, sims)

#run the loop
for(i in 1:sims){
  
  
  #deal the four card hand
  hand = sample(deck, 4, replace = FALSE)
  
  #see how many spades we got
  spades[i] = sum(hand)
}

#should get 1
mean(spades)

## [1] 1.012