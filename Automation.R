# function to automate the process of uploading the peptide sequences to 
# the website and getting the data from it stored in an excel sheet
#*** means edit
automationfunction <- function(gen){
#installing the packages required
#install.packages("RSelenium")
#install.packages("netstat")
#install.packages("tidyverse")
#install.packages("rvest")
#install.packages("openxlsx")

#load packages
library(tidyverse)
library(RSelenium)
library(netstat)
library(rvest)
library("readxl")
library("openxlsx")
library("stringr")
  
#genauto=0
#Start server
rs_driver_object <- rsDriver(browser = 'firefox', chromever = NULL, verbose = FALSE, port = free_port(), iedrver=NULL)

#create a client object
remDr <- rs_driver_object$client

#open a browser 
remDr$open()
remDr$maxWindowSize()
remDr$navigate('https://dbaasp.org/tools?page=property-calculation')

#finding the input text area element
box1 <- remDr$findElement(using = 'tag name', 'TEXTAREA')

#reading the sequences and inputing them in the correct format 
#***
fname4 = paste0("C://Users//Sara Benson//Documents//Masters Project//Grant//Autotest//Gen",gen+1, "data.txt") 
act <- read.delim(file = fname4, sep = "\n")

listOfNames = as.list(unlist(act))
seq1=list('>')
for(p in listOfNames){
  seq1=append(seq1, '\n')
  seq1=append(seq1, p)
}

box1$sendKeysToElement(seq1)

#finding the submit button and clicking on it
button1 <- remDr$findElement(using = 'tag name', 'BUTTON')
button1$clickElement()
Sys.sleep(20)

#finding the results table and writing it to a file
getthetable <- remDr$findElement(using = 'xpath', '//table[@class="pept_char_result"]') #@ symbol in front of class and class=result

getthetable_html <- getthetable$getPageSource()
page1 <- read_html(getthetable_html %>% unlist())
dh <- html_nodes(page1, "table th")
dh2 <- html_text(dh)

df <- html_table(page1)
dftibbl<- as.data.frame(df)
colnames(dftibbl) = dh2
dftibbl<-dftibbl[,-1]

# Naming and writing the files based on the generation running at the moment *** needs to be edited
#***
fname5 = paste0("C://Users//Sara Benson//Documents//Masters Project//Grant//Autotest//Gen",gen+1, ".xlsx") 
iniData = read_excel(fname5)
#***
iniData = read_excel("C://Users//Sara Benson//Documents//Masters Project//Grant//Autotest//Gen1.xlsx")
medData = iniData[,1]
medData$Length=str_length(medData$Sequence)
finalData <- cbind(medData, dftibbl)

#***
fname6 = paste0("C://Users//Sara Benson//Documents//Masters Project//Grant//Autotest//Gen",gen+1, "automated.xlsx")
write.xlsx(finalData,file = fname6 , rowNames = FALSE)
#***
write.xlsx(finalData,"C://Users//Sara Benson//Documents//Masters Project//Grant//Autotest//Gen1automated.xlsx" , rowNames = FALSE)
}