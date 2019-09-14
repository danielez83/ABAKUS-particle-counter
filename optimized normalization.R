#----------------------------------------------------------------------------------------
# Program name: normalization of distributions
# Authors: Azzurra Spagnesi, Daniele Zannoni, Federico Dallo 
# Date: 12 September 2019
# Objective: Distributions obtained with the abakus are normalized and compared in a unique plot
# Description:Choose and import ABACUS files, convert them to data frames with comparable variables,
#             then normalize the outputs and plot the results. 

rm(list=ls())
setwd(file.path("/Users/azzur/Documents/GitHub/ABAKUS-particle-counter-6a14d31af1ab2bd38185d695de445ef3c7c54ea8/measures"))

#DATASET CONSTRUCTION_ with several files uploaded as a unique vector.
#This avoid to manually handle the selection.   
files <- Sys.glob("*_**prova*")

#EXTRACT FILES from the vector "files" and create a list of matrices. 
#Lapply allows us to create a list of matrices starting from those files stored in the initial vector.
#Indeed, each file has a different length so it should be managed singularly.
filelist <- lapply(files, read.table, skip=6, sep="\t", dec=",", header=TRUE,row.names=NULL)

#Create a vector of names taken from the files path
#Apply those names to the filelist
filenames<-list.files(path = getwd(), pattern = NULL, all.files = FALSE,
             full.names = FALSE, recursive = FALSE,
             ignore.case = TRUE, include.dirs = TRUE, no.. = TRUE)
names(filelist)<-cbind(filenames)

#SETTINGS for the list of matrices
#Delete matrices with less than 300 rows (5 min acquisition) and make the remanent comparable. 
filelist<-filelist[sapply(filelist,nrow)>300]
filelist <- lapply(filelist, function(x)x[3:309,3:33]) 
filenames<-(names(filelist))
rm(files)

#just to be sure
is.na(filelist)  

#Declaration of the flux
flow_rate <- 2/60 #ml/s
volume<-flow_rate*nrow(data.frame(filelist)) 

#Normalize output values
col.sums <- apply(data.frame(filelist),2,sum) #sum of values for each column of each data.frame
first.step<-col.sums/volume #pt/ml
second.step<-sum(data.frame(filelist))/volume
normalization<-data.frame(first.step/second.step)
#Split normalization df by groups (different files)
n <- 31
nr <- nrow(normalization)
normalization<-split(normalization, rep(1:ceiling(nr/n), each=n, length.out=nr))
normalization<-data.frame(normalization)

#Rename col and rows of the normalization matrix
names(normalization) <- cbind(filenames)
rownames(normalization) <- 1:nrow(normalization)
# Define size classes and add it as new column to the front of df
size_classes <- c(0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.9,3.2,3.5,3.9,4.3,4.8,5.3,5.8,6.4,7.1,7.8,8.6,9.5,10.5,11.6,12.8,14.1,15.5)
normalization<-data.frame(size_classes, normalization)

rm(col.sums,first.step,second.step,n,nr,filenames)  

#Create different dataframes for each kind of measurement (to plot them separately)
install.packages("tidyverse")
library("tidyverse", lib.loc="~/R/win-library/3.5")

df_STD<-select(normalization,contains("STD"))
df_STD<-data.frame(size_classes, df_STD)

df_milliQ<-select(normalization,contains("milliQ_prova"))
df_milliQ<-data.frame(size_classes, df_milliQ)

df_milliQ_triton<-select(normalization,contains("milliQ_triton"))
df_milliQ_triton<-data.frame(size_classes, df_milliQ_triton)

#MILLIQ_PLOT
plot(df_milliQ$size_classes,df_milliQ$milliQ_prova_1.txt,type="l",col="blue",
     main="Cumulative distribution_milliQ", 
     ylab = "Normalized Counts",
     xlab = expression(paste("Diameter (", mu, "m)")),
     lwd=1, lty=1)
lines(df_milliQ$size_classes,df_milliQ$milliQ_prova_2.txt,col="cornflowerblue", lty=5, lwd=2)
lines(df_milliQ$size_classes,df_milliQ$milliQ_prova_3_10mlvolume.txt,col="aquamarine", lty=5, lwd=4)
legend("topright", legend=c("milliQ_pr1", "milliQ_pr2","milliQ_pr3"),
       col=c("blue", "cornflowerblue", "aquamarine"), lty=1:5, cex=0.8)

#MILLIQ+TRITON_PLOT
plot(df_milliQ_triton$size_classes,df_milliQ_triton$milliQ_triton_1ml_prova2.txt,type="l",col="chartreuse",
     main="Cumulative distribution_milliQ+triton", 
     ylab = "Normalized Counts",
     xlab = expression(paste("Diameter (", mu, "m)")),
     lwd=2)
lines(df_milliQ_triton$size_classes,df_milliQ_triton$milliQ_triton_1ml_prova4.txt,col="chartreuse4", lty=5, lwd=1)
legend("topright", legend=c("milliQ+triton_1", "milliQ+triton_2"),
       col=c("chartreuse", "chartreuse4"), lty=1:5, cex=0.8)

#STD_PLOT
plot(df_STD$size_classes,df_STD$STD_FD066_20kpt_1ml_10mlvolume_prova1.txt,type="l",col="firebrick2",
     main="Cumulative distribution_Standard", 
     ylab = "Normalized Counts",
     xlab = expression(paste("Diameter (", mu, "m)")),
     lty=5,lwd=2)
lines(df_STD$size_classes,df_STD$STD_FD066_20kpt_1ml_triton_prova1.txt,col="coral3", lty=5, lwd=2)
lines(df_STD$size_classes,df_STD$STD_FD066_20pt_1ml_10mlvolume_prova1.txt,col="brown1", lty=5, lwd=2)
lines(df_STD$size_classes,df_STD$STD_FD066_82pt_1ml_10mlvolume_prova1.txt,col="orange", lty=5, lwd=2)
lines(df_STD$size_classes,df_STD$STD_NIST_1mltriton_10mlmin_prova1.txt,col="darkred", lty=1, lwd=2)
legend("bottomleft", legend=c("FD066_20kpt_pr1", "FD066_82kpt_pr1","FD066_20kpt_pr2","FD066_20kpt_triton_pr3","NIST_triton"),
       col=c("firebrick2", "coral3", "brown1", "orange","darkred"), lty=1:5, cex=0.8)
