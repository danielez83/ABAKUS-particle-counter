#----------------------------------------------------------------------------------------
# Program name: normalization of distributions
# Author: Azzurra Spagnesi e Daniele Zannoni
# Date: 05 September 2019
# Objective: Normalize each distribution obtained with the abakus
# Description:Dopo aver selezionato il file di interesse e averlo convertito in formato tabulare,
#             presa la corretta matrice di valori dai quali si ottiene il conteggio/ml per ciascun canale,
#             si divide quest'ultimo per la sommatoria dei conteggi/ml, normalizzando così la distribuzione.


# file_name <- "test_data.txt" # Select ABACUS file name manually
# Select file using Windows Prompt
file_name <- choose.files(default = "", caption = "Select file",
                          multi = TRUE, filters = Filters,
                          index = nrow(Filters))
# Import text files and convert to data frames

abakus_data <- read.table(file_name[1], skip=6, sep="\t", dec=",", header=TRUE)
abakus_data2<- read.table(file_name[2], skip=6, sep="\t", dec=",", header=TRUE)
rm(file_name) # remove variables

#SETTINGS_seleziona righe e colonne di interesse nella matrice di valori
startrow<-3
endrow<-length(abakus_data$Index)
startcolumn<-3
endcolumn<-length(abakus_data)-1

#dichiarazione nuove matrici -confrontabili-
matr<-abakus_data[startrow:endrow, startcolumn:endcolumn]
matrix2<-abakus_data2[startrow:endrow, startcolumn:endcolumn]

#dichiarazione del flusso
flow_rate <- 2/60 #ml/s
volume<-flow_rate*(endrow-startrow) #vol impostato sulla pompa (ml)
rm(startrow,endrow,startcolumn,endcolumn,abakus_data,abakus_data2)

#normalizzazione valori output_matr
col.sums <- apply(matr,2,sum) #sum of values for each column of the matrix
first.step<-col.sums/volume #pt/ml
second.step<-sum(matr)/volume
normalization<-first.step/second.step
rm(col.sums, first.step,second.step)
#---------------------------------------------------------------
#matr2 #repete for n matrices
col.sums_2 <- apply(matrix2,2,sum) #sum of values for each column of the matrix
first.step_2<-col.sums_2/volume #pt/ml
second.step_2<-sum(matrix2)/volume
normalization_2<-first.step_2/second.step_2
rm(col.sums_2,first.step_2,second.step_2)

# Define size classes
size_classes <- c(0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.9,3.2,3.5,3.9,4.3,4.8,5.3,5.8,6.4,7.1,7.8,8.6,9.5,10.5,11.6,12.8,14.1,15.5)
# -----------------------------------------------------------------
# Display cumulative distribution as line (log scale)

x  <- size_classes
y1 <- normalization
y2 <- normalization_2
plot(x,y1,type="l",col="firebrick2",log = "xy",
     main="Cumulative distribution",
     ylab = "Normalized Counts",
     xlab = expression(paste("Diameter (", mu, "m)")),
     lwd=2)
lines(x,y2,col="blue", lty=5, lwd=1)
legend("topright", legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty=1:5, cex=0.8)