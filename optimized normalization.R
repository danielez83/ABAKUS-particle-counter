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
files <- Sys.glob("*_*prova*")

#EXTRACT FILES from the vector "files" and create a list of matrices. 
#Lapply allows us to create a list of matrices starting from those files stored in the initial vector.
#Indeed, each file has a different length so it should be managed singularly.
filelist <- lapply(files, read.table, skip=6, sep="\t", dec=",", header=TRUE,row.names=NULL)
filenames <- lapply(files, print)
names(filelist) <- filenames

rm(filenames)

#Create a vector of names taken from the files path
#Apply those names to the filelist
#PAY ATTENTION: in this way the order of the names is alphabetical
# # filenames<-list.files(path = getwd(), pattern = NULL, all.files = FALSE,
#              full.names = FALSE, recursive = FALSE,
#              ignore.case = TRUE, include.dirs = TRUE, no.. = TRUE)
# names(filelist)<-cbind(filenames)

#CHECK NA values in the filelist, their position, and remove the correspondant data.frames 
na_FLAG <- 0
na_vec_NAindex<-vector(mode="numeric", length=0) #to create an empty numeric vector
na_vec_bol<-logical(length=0) #to create an empty logical vector
for (i in 1:length((filelist))) {
  if (sum(colSums(is.na(data.frame((filelist[[i]]))))) > 0){
    na_FLAG <- na_FLAG + 1
    print('NA in file...')
    print(names(filelist[i]))
    na_vec_NAindex<-c(i,na_vec_NAindex)
    na_vec_bol <- c(FALSE, na_vec_bol)
  } else {
    na_vec_bol <- c(TRUE, na_vec_bol)
  }
} 
filelist <- filelist[na_vec_bol]

if (na_FLAG > 0){
  stop("Script interrupted", call. = FALSE)
}

#SETTINGS for the list of matrices
#Delete matrices with less than 300 rows (5 min acquisition) and make the remanent comparable. 
filelist<-filelist[sapply(filelist,nrow)>300]
filelist <- lapply(filelist, function(x)x[1:309,3:33]) 
rm(files, i, na_FLAG,na_vec_bol, na_vec_NAindex)

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
new_filenames<-names(filelist)
names(normalization) <- cbind(new_filenames)
rownames(normalization) <- 1:nrow(normalization)
# Define size classes and add it as new column to the front of df
size_classes <- c(0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.9,3.2,3.5,3.9,4.3,4.8,5.3,5.8,6.4,7.1,7.8,8.6,9.5,10.5,11.6,12.8,14.1,15.5)
normalization<-data.frame(size_classes, normalization)

rm(col.sums,first.step,second.step,n,nr,new_filenames)  

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
matplot(df_milliQ[,1],df_milliQ[,2:3],type='l', col=topo.colors(2), log = "xy",
        main="Cumulative distribution_milliQ", 
        ylab = "Normalized Counts",
        xlab = expression(paste("Diameter (", mu, "m)")),
        lwd=1, lty=1) 
legend("topright", colnames(df_milliQ[,2:3]),col=topo.colors(2),cex=0.8,fill=topo.colors(2))

#MILLIQ+TRITON_PLOT
matplot(df_milliQ_triton[,1],df_milliQ_triton[,2:4],type='l',col=heat.colors(3), log = "xy",  
        main="Cumulative distribution_milliQ+triton", 
        ylab = "Normalized Counts",
        xlab = expression(paste("Diameter (", mu, "m)")),
        lwd=1, lty=1) 
legend("topright",colnames(df_milliQ_triton[,2:4]),col=heat.colors(3),cex=0.8,fill=heat.colors(3))

#STD_PLOT 

matplot(df_STD[,1],df_STD[,2:4],type='l', col = rainbow(3), log = "xy", 
        main="Cumulative distribution_STD", 
        ylab = "Normalized Counts",
        xlab = expression(paste("Diameter (", mu, "m)")),
        lwd=1, lty=1) 
legend("topright",colnames(df_STD[,2:4]),col=rainbow(3),cex=0.6,fill=rainbow(3))
-----------------------------------------------------------------------------------------
#SECOND UPLOAD_new set of particle size (FD069)  
  
rm(filelist, normalization,volume)

setwd(file.path("C:/Users/azzur/Documents/GitHub/ABAKUS-particle-counter-6a14d31af1ab2bd38185d695de445ef3c7c54ea8/FD069 pt size"))
files <- Sys.glob("*_*prova*")
filelist <- lapply(files, read.table, skip=6, sep="\t", dec=",", header=TRUE,row.names=NULL)
filenames <- lapply(files, print)
names(filelist) <- filenames

#CHECK NA values in the filelist, their position, and remove the correspondant data.frames 
na_FLAG <- 0
na_vec_NAindex<-vector(mode="numeric", length=0) #to create an empty numeric vector
na_vec_bol<-logical(length=0) #to create an empty logical vector
for (i in 1:length((filelist))) {
  if (sum(colSums(is.na(data.frame((filelist[[i]]))))) > 0){
    na_FLAG <- na_FLAG + 1
    print('NA in file...')
    print(names(filelist[i]))
    na_vec_NAindex<-c(i,na_vec_NAindex)
    na_vec_bol <- c(FALSE, na_vec_bol)
  } else {
    na_vec_bol <- c(TRUE, na_vec_bol)
  }
} 
filelist <- filelist[na_vec_bol]

if (na_FLAG > 0){
  stop("Script interrupted", call. = FALSE)
}
#SETTINGS for the list of matrices
filelist <- lapply(filelist, function(x)x[1:279,3:33]) 
rm(files, i, na_FLAG,na_vec_bol, na_vec_NAindex)

#list2env(filelist,envir=.GlobalEnv) #convert the list of data frames into individual df. 

volume<-flow_rate*nrow(data.frame(filelist)) 
new_size_classes <- c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93)

col.sums <- apply(data.frame(filelist),2,sum) #sum(pt)per ch. 
col.sums<-data.frame(col.sums)/volume #sum(pt/ml)per ch. 
n <- 31
nr <- nrow(col.sums)
col.sums<-split(col.sums, rep(1:ceiling(nr/n), each=n, length.out=nr))
names(col.sums) <- names(filelist) #to rename each df of the list

#RENAME COLUMNS of each df with the respective names in the output list
for (i in seq_along(filenames)) { 
  colnames(col.sums[[i]]) <- filenames[i] 
} 

#per ciascun df della ls(col.sums), somma i valori e ottieni una cumulata pt/ml
cumulata <- data.frame(apply(data.frame(col.sums),2,sum))
rownames(cumulata) <- names(col.sums)

#Create a unique df from the list col.sums and add a new col at the beginning for the size_classes
col.sums<-data.frame(new_size_classes, col.sums)

#Create a new df with "names","cumulata","standard mass (mg)"

STD_mass<-c(210,36.5,73,0)
R2_df<-data.frame(STD_mass,cumulata)
names(R2_df)[2] <- "pt/ml cumulata"

x<-R2_df$STD_mass
y<-R2_df$`pt/ml cumulata`
plot(y~x,pch=20, col="blue", main="Cumulative distributions", 
        ylab = "pt/ml_cumulata",
        xlab = "FD069 mass(mg)")
fit <- lm(y ~ x, data = R2_df)
abline(fit, col="darkturquoise", lwd=2, lty=3)
legend("right", bty="n", legend=paste("adj R2 is", format(summary(fit)$adj.r.squared, digits=4)))
summary_fit<-summary(fit)

#fit third degree polynomial equation:
fit3 <- lm(y~poly(x,3,raw=TRUE))
#generate range of 50 numbers starting from 0 and ending at 210
xx <- seq(0,210, length=50)
lines(xx, predict(fit3, data.frame(x=xx)), col="blue") #ADD BEST LINE FIT 

matplot(col.sums[,1],col.sums[,2:5],type='l', col = rainbow(5), log="xy",
        xlab = expression(paste("Diameter (", mu, "m)")),
        ylab= "Test (pt/ml)",
        lwd=1, lty=1)
axis(1, at=seq(3, 47, by=3), labels = FALSE)
legend("topright",colnames(col.sums[,2:5]),col=rainbow(5),cex=0.6,fill=rainbow(5))

# #PLOT pt/ml per channel 3.0 
# x<-STD_mass[1:3]
# y<-t(col.sums[1,2:4])
# plot(y~x, pch=20, col="green", main="pt/ml per channel", 
#      ylab = "pt/ml",
#      xlab = "FD069 mass(mg)")
# abline(lm(t(col.sums[1,2:4])~STD_mass[1:3]), col="darkgreen")
# modelX<-lm(y~x)
# summary(modelX)$r.square
# text(paste("R2= ",summary(modelX)$r.square,sep = " "), x = 150, y = 5000)

matrix<-data.frame(t(col.sums[,2:4])) 
colnames(matrix) <- new_size_classes
matrix<-data.frame(STD_mass[1:3], matrix)

#Create an R2 vector
R_square<-sapply(matrix[-ncol(matrix)],function(x){
  mylm<-lm(x~matrix$STD_mass.1.3.)
  theR2<-summary(mylm)$r.squared
  return(theR2)
})

#omit na values from the R2 vector
R_square <- na.omit(R_square)
R_square<-R_square[2:16] #ch. 3.0: ch. 45

y1<-R_square
x1<-new_size_classes[1:15] #keep only the first 15ch. (from 3.0 to 45)
df<-data.frame(x1,y1)
# plot(y1~x1,pch=20, col="deeppink3", 
#      ylab = "R2",
#      xlab = "channel") 
# title("Scatterplot R2 per channel", outer=TRUE, line=-1, cex.main=1.5)
# axis(3, at=seq(3, 45, by=3), labels = T)
library(ggplot2)
qplot(x1, y1) + stat_smooth()+
labs(x = "channel")+
labs(y = "R2")+ 
coord_cartesian(ylim = (0:1))+
labs(title = "R2 per channel")+ 
theme(plot.title = element_text(hjust = 0.5))
