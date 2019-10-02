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

# if (na_FLAG > 0){
#   stop("Script interrupted", call. = FALSE)
# }

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

#TESTs PLOT (WITHOUT milliQ)
matplot(col.sums[,1],col.sums[,2:4], type="l",col = rainbow(4), log="xy",
        xlab = expression(paste("Diameter (", mu, "m)")),
        ylab= "Test (pt/ml)",
        main="Tests (FD069) with new set of diameters (3 um<d<96 um)",
        lwd=1, lty=1)
axis(1, at=seq(3, 47, by=3), labels = FALSE)
legend("topright",colnames(col.sums[,2:4]),col=rainbow(4),cex=0.6,fill=rainbow(4))

matrix<-data.frame(t(col.sums[,2:4])) 
colnames(matrix) <- new_size_classes
matrix<-data.frame(STD_mass[1:3], matrix)

#Create an R2 vector 
R_square<-sapply(matrix[-ncol(matrix)],function(x){
  mylm<-lm(x~matrix$STD_mass.1.3.)
  theR2<-summary(mylm)$r.squared
  return(theR2)
})

#create slope array
slope<-sapply(matrix[-ncol(matrix)],function(x){
  mylm<-lm(x~matrix$STD_mass.1.3.)
  slope_val<-mylm$coefficients[2]
  return(slope_val)
})

#control slope
R_square[slope<=0]=0

#omit na values from the R2 vector
R_square <- na.omit(R_square)
R_square<-R_square[2:16] #ch. 3.0: ch. 45

#PLOT diameters vs R2
x1<-new_size_classes[1:15] #keep only the first 15ch. (from 3.0 to 45)
y1<-R_square
df<-data.frame(x1,y1) 
plot(y1~x1,pch=20, col="deeppink3", 
      ylab = "R2",
      xlab = expression(paste("Diameter (", mu, "m)")),
 main="Scatterplot R2 per channel" )
 axis(1, at=seq(3, 50, by=3), labels = T)

#Put down the remanent outliers in order to fit the best polyf. to the scatterplot 
 R_square<-data.frame(R_square)
 for(i in 1:14){
   if((R_square[i,]==0.0000000))   #valutare se usare una soglia meno stringente
     R_square[i+1,]<- 0.0000000
 } 
 
 #NEW SCATTERPLOT WITH THE BEST POLYLINE FITTED BY HAND
 y<-R_square$R_square
 x<-new_size_classes[1:15]
 scatterplot_R2_diameters<-data.frame(x,y)
 plot(y~x,pch=20, col="deeppink3", 
      ylab = "R2",
      xlab = expression(paste("Diameter (", mu, "m)")),
      main="Scatterplot R2 per channel" )
 axis(1, at=seq(3, 50, by=3), labels = T)

 #fit third degree polynomial equation:
 fit3 <- lm(y~poly(x,3,raw=TRUE))
 #generate range of 45 numbers starting from 0 and ending at 200
 xy <- seq(0,200, length=45)
 lines(xy, predict(fit3, data.frame(x=xy)), col="blue") #ADD BEST LINE FIT 
 #fit eighth degree polynomial equation:
 fit8 <- lm(y~poly(x,8,raw=TRUE))
 #generate range of 45 numbers starting from 0 and ending at 200
 xx <- seq(0,200, length=45)
 lines(xx, predict(fit8, data.frame(x=xx)), col="deeppink3") #ADD BEST LINE FIT 

#0.9105 R2 fit3, 0.987 R2 fit8
 
#GGPLOT WITH POLYLN and equation 
library("ggpmisc", lib.loc="~/R/win-library/3.5")
 y<-R_square$R_square
 x<-new_size_classes[1:15]
scatterplot_R2_diameters<-data.frame(x1,y) 
 
colnames(scatterplot_R2_diameters)<-c("Diameters (um)","R2")
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(scatterplot_R2_diameters, aes(x, y)) +  
  labs(x = "Diameter (um)")+
  labs(y = "R2")+ coord_cartesian(ylim = (0:1))+
  geom_point() +
  labs(title = "Correction")+ theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label =  paste(stat(eq.label), stat(rr.label), sep = "~~~~")),
               formula = formula, rr.digits = 3, coef.digits = 2, parse = TRUE)


#New df with tests normalized for the graphs' underneathed area 
col.sums_subset <- data.frame(col.sums[,2:4])
col.sums_subset <-apply(data.frame(col.sums_subset),2,sum)
normalized_tests <-col.sums[,2:4]/col.sums_subset


# PLOT TESTS (20K, 3.5k, 7k pt/ml) NORMALIZED for the area
new_diam<-c(3:33)
normalized_tests<-data.frame(new_diam, normalized_tests)

x<-normalized_tests[,1]
y<-normalized_tests[,2]
formula1 <- y ~ poly(x, 3, raw = TRUE)
plot1<-ggplot(normalized_tests, aes(x, y)) +  
  labs(x = "Diameter (um)")+
  labs(y = "STD_FD069 (pt/ml)")+
  geom_point() +
  labs(title = "FD069 20k pt/ml normalized for the area")+ theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = "lm", formula = formula1) +
  stat_poly_eq(aes(label =  paste(stat(eq.label), stat(rr.label), sep = "~~~~")),
               formula = formula1, rr.digits = 3, coef.digits = 2, parse = TRUE)

x<-normalized_tests[,1]
y1<-normalized_tests[,3]
formula2 <- y1 ~ poly(x, 3, raw = TRUE)
plot2<-ggplot(normalized_tests, aes(x, y1)) +  
  labs(x = "Diameter (um)")+
  labs(y = "STD_FD069 (pt/ml)")+
  geom_point() +
  labs(title = "FD069 3.5k pt/ml normalized for the area")+ theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = "lm", formula = formula2) +
  stat_poly_eq(aes(label =  paste(stat(eq.label), stat(rr.label), sep = "~~~~")),
               formula = formula2, rr.digits = 3, coef.digits = 2, parse = TRUE)

x<-normalized_tests[,1]
y2<-normalized_tests[,4]
formula3 <- y2 ~ poly(x, 3, raw = TRUE)
plot3<-ggplot(normalized_tests, aes(x, y2)) +  
  labs(x = "Diameter (um)")+
  labs(y = "STD_FD069 (pt/ml)")+
  geom_point() +
  labs(title = "FD069 7k pt/ml normalized for the area")+ theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = "lm", formula = formula3) +
  stat_poly_eq(aes(label =  paste(stat(eq.label), stat(rr.label), sep = "~~~~")),
               formula = formula3, rr.digits = 3, coef.digits = 2, parse = TRUE)


library("gridExtra", lib.loc="~/R/win-library/3.5")
grid.arrange(plot1, plot2, plot3, ncol=2)

# coefficients of the cubic functions
a <- c(-0.00019,0.012,-0.23,1.3)
b <- c(-0.00012,0.0071,-0.13,0.71)
c <- c(-0.00012,0.0071,-0.13,0.72)
corr<-c(4.4*10^-5,-0.0022,-0.014,1.2)

#Three arrays with corrected coefficients for each cubic function
y1c<-a-corr
y2c<-b-corr
y3c<-c-corr
#Write cubic functions corrected
library(polynom) 
p1 <- polynomial(c(y1c[4],y1c[3],y1c[2],y1c[1]))
p2 <- polynomial(c(y2c[4],y2c[3],y2c[2],y2c[1]))
p3 <- polynomial(c(y3c[4],y3c[3],y3c[2],y3c[1]))

#Plot polynomial functions corrected
par(mfrow=c(2,2)) 
plot(p1,pch=20, col="red", 
     ylab = "FD069_20kpt corrected",
     xlab = expression(paste("Diameter (", mu, "m)")),
     main="FD069_20kpt corrected")
plot(p2,pch=20, col="green", 
     ylab = "FD069_3.5kpt corrected",
     xlab = expression(paste("Diameter (", mu, "m)")),
     main="FD069_3.5kpt corrected")
plot(p3,pch=20, col="blue", 
     ylab = "FD069_7kpt corrected",
     xlab = expression(paste("Diameter (", mu, "m)")),
     main="FD069_7kpt corrected")

#dev.off() 

#ARRAY CORRECTION
#Extraction of the first 15 diameters (3 to 45 um) from the matrix df
matrix_3to45um<-subset(matrix, select = X3:X45)
R_square<-array(R_square$R_square)
#Create a df of correction, multiplying R2 values for pt/ml per channel. 
correction<-data.frame(mapply(`*`,matrix_3to45um,R_square))
#Subtract the correction from the intial values (pt/ml per channel)
matrix_corrected<-data.frame(mapply(`-`,matrix_3to45um,correction))
matrix_corrected<-data.frame(t(matrix_corrected[,1:15])) 
matrix_corrected<-data.frame(x1, matrix_corrected)
colnames(matrix_corrected)<-c("Diameters (um)","FD069_20kpt/ml","FD069_3,5kpt/ml","FD069_7kpt/ml")
#plot new results

matplot(matrix_corrected[,1],matrix_corrected[,2:4],type='l', col = rainbow(3), 
        main="Tests corrected",  
        xlab = expression(paste("Diameter (", mu, "m)")),
        ylab= "corrected tests (pt/ml)",
        lwd=1, lty=1)
axis(1, at=seq(3, 47, by=3), labels = FALSE)
legend("topright",colnames(matrix_corrected[,2:4]),col=rainbow(3),cex=0.6,fill=rainbow(3))

#import milliQ.txt file
milliQ<-read.table(file.choose(),skip=6, sep="\t", dec=",", header=TRUE,row.names=NULL) 
milliQ<-milliQ[,3:34]

flow_rate <- 2/60 #ml/s
diameters <- c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,96)

#1st df for milliQ (600s of acquisition)
milliQ_600s<-milliQ[1:600,]

volume_milliQ_600s<-flow_rate*nrow(data.frame(milliQ_600s)) #ml

milliQ_600s <- apply(data.frame(milliQ_600s),2,sum) #sum(pt)per ch. (600s)
milliQ_600s <-data.frame(milliQ_600s)/volume_milliQ_600s #sum(pt/ml)per ch. 
milliQ_600s<-data.frame(diameters, milliQ_600s)

#2nd df for milliQ (1800s of acquisition)
milliQ_1800s<-milliQ[1:1800,]

volume_milliQ_1800s<-flow_rate*nrow(data.frame(milliQ_1800s)) #ml

milliQ_1800s <- apply(data.frame(milliQ_1800s),2,sum) #sum(pt)per ch. (1800s)
milliQ_1800s <-data.frame(milliQ_1800s)/volume_milliQ_1800s #sum(pt/ml)per ch. 
milliQ_1800s <-data.frame(diameters, milliQ_1800s)

#3rd df for milliQ (3600s of acquisition)
milliQ_3600s<-milliQ[1:3600,]

volume<-flow_rate*nrow(data.frame(milliQ_3600s)) #ml

milliQ_3600s <- apply(data.frame(milliQ_3600s),2,sum) #sum(pt)per ch. (1h)
milliQ_3600s <-data.frame(milliQ_3600s)/volume #sum(pt/ml)per ch. 
milliQ_3600s <-data.frame(diameters, milliQ_3600s)

# 3 figures arranged in 2 rows and 2 columns
par(mfrow=c(2,2)) 
plot(milliQ_600s$milliQ_600s~milliQ_600s$diameters,pch=20, col="goldenrod3", 
     ylab = "milliQ pt/ml",
     xlab = expression(paste("Diameter (", mu, "m)")),
     main="milliQ pt/ml 10min")
plot(milliQ_1800s$milliQ_1800s~milliQ_1800s$diameters,pch=20, col="orange", 
     ylab = "milliQ pt/ml",
     xlab = expression(paste("Diameter (", mu, "m)")),
     main="milliQ pt/ml 30min")
plot(milliQ_3600s$milliQ_3600s~milliQ_3600s$diameters,pch=20, col="green", 
     ylab = "milliQ pt/ml",
     xlab = expression(paste("Diameter (", mu, "m)")),
     main="milliQ pt/ml 1h")

#Create an R2 vector for milliQ tests (linear regression: raw counts vs time)

time<-c(0:3603) #s
milliQ<-data.frame(time,milliQ)

R2_vec_milliQ<-sapply(milliQ[-ncol(milliQ)],function(x){
  mylm<-lm(x~milliQ$time)
  theR2<-summary(mylm)$r.squared
  return(theR2)
})

R2_vec_milliQ <- na.omit(R2_vec_milliQ)




