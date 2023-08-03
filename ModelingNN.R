#Putting the entire Modelling code in a function that can be called
#*** means edit
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

#***
Comp_data = read_excel("C://Users//Sara Benson//Documents//Masters Project//PeptideMaster.xlsx")

#Removing any duplicated data  
Comp_data <- Comp_data[!duplicated(Comp_data), ]
require(caTools)

#Calling all properties of peptides other than sequences
Comp_dataMIC1 = Comp_data[,2:15]

dft = Comp_dataMIC1

#Putting MIC values in MIC^-1  
dft$MIC1 = (1/dft$MIC1)

#Generating train and test sets
sample_size = round(nrow(dft)*1) 
index <- sample(seq_len(nrow(dft)), size = sample_size)

train <- dft[index, ]
test <- dft[-index, ]

#Neural network function
NN <- neuralnet(
  MIC1 ~ .,
  data=data.frame(train),
  hidden=10, #number of hidden neurons (vertices) in each layer
  rep=4, #the number of repetitions for the neural network's training
  threshold=0.01, #numeric value specifying the threshold for the partial derivatives of the error function as stopping criteria
  err.fct="sse", #error function used, sum of squared errors
  linear.output=FALSE #logical, ff act.fct should not be applied to the output neurons set linear output to TRUE, otherwise to FALSE
)

#############################################################################################

library(stringr)

# calling the file based on the generation running at the moment 
#***
fname7 = paste0("C://Users//Sara Benson//Documents//Masters Project//Grant//Autotest//Gen",gen+1, "automated.xlsx")
newdata = read_excel(fname7)
#***
newdata = read_excel("C://Users//Sara Benson//Documents//Masters Project//Grant//Autotest//Gen1automated.xlsx")

#Creating dataframe of new sequences
gen.S = data.frame(newdata$Sequence)

#New biophysical property data  
newdata = newdata[,2:14]

#Predicting new values   
predNN = predict(NN, newdata=data.frame(newdata))

gen.M = data.frame(predNNnew)
library(writexl)

 #Choosing top 10 sequences  
choose = 10 
gen.total = data.frame(c(gen.S,gen.M))
gen.total = gen.total[order(-gen.total$predNNnew),]
gen.total = gen.total[1:choose,]

#Creating new excel file  
#***
fname8 = paste0("C://Users//Sara Benson//Documents//Masters Project//Grant//Autotest//Gen",gen+1, "data.xlsx")
write.xlsx(gen.total, fname8)
#***
write.xlsx(gen.total, "C://Users//Sara Benson//Documents//Masters Project//Grant//Autotest//Gen1data.xlsx")
}
