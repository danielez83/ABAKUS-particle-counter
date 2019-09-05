#----------------------------------------------------------------------------------------
# Program name: normalization of distributions
# Author: Azzurra Spagnesi e Daniele Zannoni
# Date: 05 September 2019
# Objective: Normalize each distribution obtained with the abakus
# Description:Dopo aver selezionato il file di interesse e averlo convertito in formato tabulare,
#             presa la corretta matrice di valori dai quali si ottiene il conteggio/ml per ciascun canale,
#             si divide quest'ultimo per la sommatoria dei conteggi/ml, normalizzando così la distribuzione.


setwd("~/GitHub/ABAKUS-particle-counter-6a14d31af1ab2bd38185d695de445ef3c7c54ea8")
# file_name <- "test_data.txt" # Select ABACUS file name manually
# Select file using Windows Prompt
file_name <- choose.files(default = "", caption = "Select file",
                          multi = FALSE, filters = Filters,
                          index = nrow(Filters))
skip_lines = 6; #number of lines to skip

# Import text file and convert to a data frame
abakus_data <- read.delim2(file = file_name,
                           sep = ";",
                           skip = 6,
                           header = TRUE)
rm(file_name) # remove variables
rm(skip_lines) # remove variables

#seleziona righe e colonne di interesse nella matrice di valori
startrow<-3
endrow<-length(abakus_data$Index)
startcolumn<-3
endcolumn<-length(abakus_data)-1

#dichiarazione del flusso
flow_rate <- 2/60 #ml/s
volume<-flow_rate* (endrow-startrow) #vol impostato sulla pompa (ml)

#dichiarazione nuova matrice
matr<-abakus_data[startrow:endrow, startcolumn:endcolumn]

#normalizzazione valori output
x0.8<-c(sum(matr$X0.8)/volume)/sum(matr/volume)
x0.9<-c(sum(matr$X0.9)/volume)/sum(matr/volume)
x1.0<-c(sum(matr$X1.0)/volume)/sum(matr/volume)
x1.1<-c(sum(matr$X1.1)/volume)/sum(matr/volume) 
x1.2<-c(sum(matr$X1.2)/volume)/sum(matr/volume)
x1.3<-c(sum(matr$X1.3)/volume)/sum(matr/volume)
x1.4<-c(sum(matr$X1.4)/volume)/sum(matr/volume)
x1.6<-c(sum(matr$X1.6)/volume)/sum(matr/volume)
x1.8<-c(sum(matr$X1.8)/volume)/sum(matr/volume)
x2.0<-c(sum(matr$X2.0)/volume)/sum(matr/volume) 
x2.2<-c(sum(matr$X2.2)/volume)/sum(matr/volume) 
x2.4<-c(sum(matr$X2.4)/volume)/sum(matr/volume) 
x2.6<-c(sum(matr$X2.6)/volume)/sum(matr/volume)
x2.9<-c(sum(matr$X2.9)/volume)/sum(matr/volume)
x3.2<-c(sum(matr$X3.2)/volume)/sum(matr/volume)
x3.5<-c(sum(matr$X3.5)/volume)/sum(matr/volume) 
x3.9<-c(sum(matr$X3.9)/volume)/sum(matr/volume) 
x4.3<-c(sum(matr$X4.3)/volume)/sum(matr/volume)
x4.8<-c(sum(matr$X4.8)/volume)/sum(matr/volume)  
x5.3<-c(sum(matr$X5.3)/volume)/sum(matr/volume)  
x5.8<-c(sum(matr$X5.8)/volume)/sum(matr/volume)  
x6.4<-c(sum(matr$X6.4)/volume)/sum(matr/volume) 
x7.1<-c(sum(matr$X7.1)/volume)/sum(matr/volume)
x7.8<-c(sum(matr$X7.8)/volume)/sum(matr/volume) 
x8.6<-c(sum(matr$X8.6)/volume)/sum(matr/volume)
x9.5<-c(sum(matr$X9.5)/volume)/sum(matr/volume) 
x10.5<-c(sum(matr$X10.5)/volume)/sum(matr/volume) 
x11.6<-c(sum(matr$X11.6)/volume)/sum(matr/volume)
x12.8<-c(sum(matr$X12.8)/volume)/sum(matr/volume) 
x14.1<-c(sum(matr$X14.1)/volume)/sum(matr/volume)  
x15.5<-c(sum(matr$X15.5)/volume)/sum(matr/volume)  

#vector of normalized values
counts<-c(x0.8,x0.9,x1.0,x1.1,x1.2,x1.3,x1.4,x1.6,x1.8,x2.0,x2.2,x2.4,x2.6,x2.9,x3.2,x3.5,x3.9,x4.3,x4.8,x5.3,x5.8,x6.4,x7.1,x7.8,x8.6,x9.5,x10.5,x11.6,x12.8,x14.1, x15.5)

# Define size classes
size_classes <- c(0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.9,3.2,3.5,3.9,4.3,4.8,5.3,5.8,6.4,7.1,7.8,8.6,9.5,10.5,11.6,12.8,14.1,15.5)
# -----------------------------------------------------------------
# Display cumulative distribution as line (log scale)
plot(x = size_classes, y = counts, 
     type = 'l',
     log = "xy",
     ylab = "Normalized Counts",
     xlab = expression(paste("Diameter (", mu, "m)")),
     lwd=2,
     col="green",
)
