#PCA biplot Generation of PCA1 and PCA2
#*** means edit 

#install.packages("factoextra")
#install.packages("readxl")
library("factoextra")
library("readxl")

#Need to install file to run PCA
#***
PCAtot <- read_excel("C://Users//Sara Benson//Documents//Masters Project//Grant//PCAtot.xlsx")
Data <- subset(PCAtot, select=-c(Sequence,MIC1)) #subsetting data to only incorporate biphysical properties

#Running PCA of dataset
pca <- prcomp(Data,
              scale=TRUE)
pca$rotation #Rotating the pca, maximizing the separation within the variance obtained by components

#Biplot generation of PCA
fviz_pca_biplot(pca,
                label='var',
                col.ind="cos2", 
                col.var = "black",
                gradient.cols='YlOrRd') 