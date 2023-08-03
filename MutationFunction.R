#Putting the entire Mutation Function in a function that can be called
#*** means edit
mutationfunction <- function(gen){ 
knitr::opts_chunk$set(echo = TRUE)
rm(list=ls())

#install.packages("openxlsx")
#install.packages("rJava")
#install.packages("tidyr")

library("gamesGA")
library("tidyr")
library("dplyr")
library("stringr")
library("readxl")
library("openxlsx")

# calling the file based on the generation running at the moment 
#***
fname1 = paste0("C://Users//Sara Benson//Documents//Masters Project//Grant//Autotest//Gen",gen, "data.xlsx")
data = read_excel(fname1)
mut_rate = 75

aa2 = c("A","C","D","E","F","G","H","I","K","L","M","N","P","Q","R","S","T","V","W","Y","")


seq = data[,1]

dtcopy <- data.frame(seq[rep(1:nrow(seq), each = 10), ])

colnames(dtcopy) <- c("Sequence")

loading = data.frame(matrix(nrow = nrow(dtcopy),ncol=nchar(dtcopy[1,1])))
substr(dtcopy[1,1],1,1)

for(k in 1:nrow(dtcopy)){
  for(i in 1:nchar(dtcopy[1,1])){
    loading[k,i] = substr(dtcopy[k,1],i,i)
  }
}

p = crossover(loading,prob = .050)

pclean = data.frame(do.call(paste, as.data.frame(p, stringsAsFactors=FALSE)))
dtcopy = pclean

dtcopy = data.frame(apply(dtcopy,2,str_remove_all, " "))

ran.x  = sample(1:ncol(p),mut_rate,replace=T)
ran.y  = sample(1:nrow(p),mut_rate,replace=T)
ran.n = sample(aa2,mut_rate,replace=T)

for (j in 1:mut_rate){
  p[ran.y[j],ran.x[j]] = ran.n[j]
  
}
pclean = data.frame(do.call(paste, as.data.frame(p, stringsAsFactors=FALSE)))
dtcopy = pclean

dtcopy = data.frame(apply(dtcopy,2,str_remove_all, " "))

mut = data.frame(dtcopy)
dt2 <- data.frame(mut[rep(1:nrow(mut), each = 2), ])
dt2[1:nrow(dt2) %% 2 == 1, ] <- NA


dt2[is.na(dt2)] <- ">"

# creating files based on the generation running at the moment
#***
fname2 = paste0("C://Users//Sara Benson//Documents//Masters Project//Grant//Autotest//Gen",gen+1, "data.txt")
write.table(dt2, file = fname2, sep = "\t",
            row.names = FALSE,col.names = FALSE,quote=FALSE)
colnames(mut) <- c("Sequence")

#***
fname3 = paste0("C://Users//Sara Benson//Documents//Masters Project//Grant//Autotest//Gen",gen+1, ".xlsx")
write.xlsx(mut, fname3)
}