#Putting the entire Modelling code in a function that can be called
modellingfunction <- function(gen){
knitr::opts_chunk$set(echo = TRUE)
rm(list=ls())

###########################################TRAINING THE MODEL###############################
#set.seed()
#install.packages("tidyverse")
#install.packages("openxlsx")
#install.packages("neuralnet")
library("readxl")
library("openxlsx")
library("neuralnet")

# *** needs to be edited
Comp_data = read_excel("C://Users//Sara Benson//Documents//Masters Project//PeptideMaster.xlsx")
Comp_data <- Comp_data[!duplicated(Comp_data), ]
library(randomForest)
require(caTools)

Comp_dataMIC1 = Comp_data[,2:15]

dft = Comp_dataMIC1
dft$MIC1 = (1/dft$MIC1)
sample_size = round(nrow(dft)*1) 
index <- sample(seq_len(nrow(dft)), size = sample_size)

train <- dft[index, ]
test <- dft[-index, ]

NN <- neuralnet(
  MIC1 ~ .,
  data=data.frame(train),
  hidden=10,
  rep=4,
  hreshold=0.01,
  err.fct="sse",
  linear.output=FALSE
)


#rf <- randomForest(
  #MIC~.,
  #data=data.frame(train)
#)

#############################################################################################

library(stringr)

# calling the file based on the generation running at the moment *** needs to be edited
fname7 = paste0("C://Users//Sara Benson//Documents//Masters Project//Grant//NNPCA1.2//Gen",gen+1, "automated.xlsx")
newdata = read_excel(fname7)
newdata = read_excel("C://Users//Sara Benson//Documents//Masters Project//Grant//NNPCA1.2//Gen1automated.xlsx")
gen.S = data.frame(newdata$Sequence)

newdata = newdata[,2:14]

predRFnew = predict(rf, newdata=data.frame(newdata))

gen.M = data.frame(predRFnew)
library(writexl)

choose = 10 #choose the top 10 
gen.total = data.frame(c(gen.S,gen.M))
gen.total = gen.total[order(-gen.total$predRFnew),]
gen.total = gen.total[1:choose,]

# *** needs to be edited
fname8 = paste0("C://Users//Sara Benson//Documents//Masters Project//Grant//NNPCA1.2//Gen",gen+1, "data.xlsx")
write.xlsx(gen.total, fname8)
write.xlsx(gen.total, "C://Users//Sara Benson//Documents//Masters Project//Grant//NNPCA1.2//Gen1data.xlsx")
}