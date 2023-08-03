# This program run the other 3 programs in a loop for as many generations
# as you want.
#*** means edit

#Calling the other 3 programs 
#***
source("C://Users//Sara Benson//Documents//Masters Project//Automated Program//MutationFunction.R")
source("C://Users//Sara Benson//Documents//Masters Project//Automated Program//Automation.R")
source("C://Users//Sara Benson//Documents//Masters Project//Automated Program//ModelingNN.R") #***This line needs changed for either RF or NN

gen=0
totalgen=50 #***enter the total number of generations you want to run here 

#Loop to run it for multiple generations
while(gen<totalgen){
mutationfunction(gen)
Sys.sleep(10)
automationfunction(gen)
Sys.sleep(10)
modellingfunction(gen)
Sys.sleep(10)
gen=gen + 1
}