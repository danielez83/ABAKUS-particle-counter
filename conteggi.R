#----------------------------------------------------------------------------------------
# Program name: abakus_particles/ml 
# Author: Azzurra Spagnesi e Daniele Zannoni
# Date: 03 September 2019
# Objective: Conteggio particelle/ml per ciascun canale dell'abakus
# Description:L'operatore deve selezionare il file di output di interesse (abakus),
#             del quale lo script consente di saltare le prime sei righe di intestazione. 
#             Acquisito il file e convertito in formato tabulare, si procede nella selezione 
#             di colonne e righe di interesse (negligible col.: index, duration, s).
#             Per ciascun canale dimensionale, la somma dei conteggi delle particelle nel tempo
#             viene divisa per il flow rate (ml/s) per ottenere pt/ml lette dall'abakus.



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

#CONTEGGI/ml
# Prompt total particle concentration
paste("Total particles concentration: ", sum(matr/volume), "pt/ml") #tot particelle/ml
# Prompt particles concentration by size
paste("Particles concentration size",variable.names(matr), "(mm):", colSums(matr/flow_rate), "pt/ml")

# sum(matr$X0.8)/volume #pt/ml per canale
# conteggi_ml<-matr/flow_rate