
library(rvest)


file <- sotu.address.all.data

set.seed(111989)

seed <- sample(seq(1, length(file)), size = 1)


word1       <- file[seed]
word2       <- file[seed + 1]
word3       <- file[seed + 2]
word4       <- file[seed + 3]
sentence    <- paste(word1, word2, word3, word4, sep = " ")
lengths     <- NULL


#Matching words 1, 2, 3 & 4

for(i in 1:200){
  
  locations  <- which(file == word1)
  locations  <- locations[which(file[locations + 1] == word2)]
  locations  <- locations[which(file[locations + 2] == word3)]
  locations  <- locations[which(file[locations + 3] == word4)]

  next.possible <- locations + 4
  if(length(next.possible) == 1){
    rand.draw <- next.possible
  
  } else{
    rand.draw <- sample(next.possible, size = 1)
    
  }
  
  if(rand.draw > length(file)){
    
    rand.draw <- 1989
  }
  next.word     <- file[rand.draw]
  
  sentence <- paste(sentence, next.word, sep = " ")
  lengths  <- c(lengths, length(next.possible))
  word1 <- word2
  word2 <- word3
  word3 <- word4
  word4 <- next.word
  
}


g <- regexpr("\\.[^\\.]*$", x)


test <-
  str_split(sentence, pattern = "\ . ")
  

