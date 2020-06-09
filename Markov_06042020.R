library(markovchain)
library(tidyverse)
library(dplyr)


setwd("/Users/bmr225/Documents/CodinginR/States_sequence_data")


dataFiles <- lapply(Sys.glob("*.csv"), read_csv)  ### very good  # make a list # dataFiles[[1]].....
myFiles <- list.files(pattern="*.csv")


for (i in 1:length(dataFiles)){
  
    state_seq <-as.data.frame(dataFiles[[i]])
    
    m = matrix(0, nrow = 3, ncol = 3)
    matrix_loop = matrix(0, nrow = 3, ncol = 3)
    
    for (j in 1:length(state_seq)){
      
      list_seq <- as.list(state_seq[j])
      sleepwakeFittedMLE <- markovchainFit(data = list_seq, method = 'mle', name = 'sleepwakeMLE')
      a <- sleepwakeFittedMLE$estimate # how to deal with S4 class in a more automatic class
      
      m[1,1] <- a[1,1]
      m[2,2] <- a[2,2]
      m[3,3] <- a[3,3]
      m[2,1] <- a[2,1]
      m[1,2] <- a[1,2]
      m[1,3] <- a[1,3]
      m[2,3] <- a[2,3]
      m[3,1] <- a[3,1]
      m[3,2] <- a[3,2]
      
      matrix_loop <- matrix_loop +  m
      
    }
    
    matrix_loop <- matrix_loop/length(state_seq)
    
    filename <- unlist(strsplit(myFiles[i], split='.', fixed=TRUE))[1]
    
    write.csv(matrix_loop, file = paste(filename,'_transition_matrix.csv',sep=""))
  
}





