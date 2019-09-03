#conteggi/ml 
file_name <- "test_data.txt" # ABACUS file name
skip_lines = 6; #number of lines to skip

# Import text file and convert to a data frame
abakus_data <- read.delim2(file = file_name,
                           sep = ";",
                           skip = 6,
                           header = TRUE)
rm(file_name) # remove variables
rm(skip_lines) # remove variables

#CONTEGGI/ml
flow_rate <- 2/60 #ml/s

startrow<-3
endrow<-length(abakus_data$Index)
Startcolumn<-3
endcolumn<-length(abakus_data)-1

volume<-flow_rate* (endrow-startrow) #ml

matr<-abakus_data[startrow:endrow, Startcolumn:endcolumn]
sum(matr$X0.8)/flow

sum(matr)/volume
matr/flow_rate
sum(matr$X0.8)/volume
conteggi_ml<-matr/flow_rate
paste("particle concentration: ", sum(matr/volume), "pt/ml")
