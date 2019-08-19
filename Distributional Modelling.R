data.actual <- read.csv("Data_GP2.csv")
# Separate the dataset into BRK and IBM
BRK <- data.actual[-1,1:7]
IBM <- data.actual[-1,9:15]
# Update column names
colnames(BRK) <- c("Date","Open","High","Low","Close","Vol","Close.Adj")
colnames(IBM) <- c("Date","Open","High","Low","Close","Vol","Close.Adj")
# Remove blank cells in BRK
BRK[BRK ==""] <- NA
BRK <- na.omit(BRK)
# Convert prices and volume to numeric
for (i in 2:7){
  BRK[,i] <- as.numeric(as.character(BRK[,i]))
  IBM[,i] <- as.numeric(as.character(IBM[,i]))
}
# Change date from 17/2/17 to 2017-02-17 in this form %y-%m-%d
BRK$Date <- as.Date(as.character(BRK$Date), "%d/%m/%y")
IBM$Date <- as.Date(as.character(IBM$Date), "%d/%m/%y")
# Change the date format
BRK$Date <- as.Date(format(BRK$Date))
# IBM$Date <- as.Date(format(IBM$Date)) does not work for years 1968 and before
IBM$Date <- as.Date(ifelse(IBM$Date > Sys.Date(), format(IBM$Date, "19%y-%m-%d"), format(IBM$Date)))
# Look at summary of the data
summary(BRK)
summary(IBM)
# Order the data by date and subset for the same date points of both stocks
data_berk <- BRK[(order(as.Date(BRK$Date))),]
data_ibm <- IBM[(order(as.Date(IBM$Date))),]
# Make into xts, combine data, remove NAs, separate into the 2 stocks
library('quantmod')
data_berk <- xts(data_berk[,-1], order.by=data_berk[,1])
data_ibm <- xts(data_ibm[,-1], order.by=data_ibm[,1])
test1<-cbind(data_berk,data_ibm)
test1<-na.omit(test1)
data_berk<-test1[,1:6]
data_ibm <-test1[,7:12]
data_berk1 <- data_berk[1:4657,]
data_ibm1 <- data_ibm[1:4657,]
## Starting to calculate the returns for different horizons
# daily return
data_berk_daily <- dailyReturn(data_berk1[,6], subset=NULL, type='arithmetic', leading=TRUE) #-20/290
data_ibm_daily <- dailyReturn(data_ibm1[,6], subset=NULL, type='arithmetic', leading=TRUE) 
# Checking plots of returns
plot(data_berk_daily)
plot(data_ibm_daily)
# Put both stocks' returns in a dataframe and plot the 2-dimensional returns
data_berk_daily.df <-as.data.frame(data_berk_daily)
data_ibm_daily.df <- as.data.frame(data_ibm_daily)
plot(data_berk_daily.df$daily.returns, data_ibm_daily.df$daily.returns,xlab='berk daily return',
     ylab='ibm daily return',main='Bivariate Daily Return Data')
# Combine both stocks returns and make into a matrix
combined_daily <- cbind(data_berk_daily,data_ibm_daily)
combined_daily <- as.matrix(combined_daily)
##  Fitting various distributions
# install.packages("ghyp")
library('ghyp')
# Fitting normal distribution
normal_daily <-fit.gaussmv(combined_daily, na.rm = T, save.data = T)
normal_daily
# Fitting t distribution
t_daily <- fit.tmv(combined_daily, nu = 3.5,
               opt.pars = c(lambda = T, mu = T, sigma = T, gamma = F,symmetric = T))
t_daily
# Fitting variance gamma distribution
vg_daily <-fit.VGmv(combined_daily, lambda = 1, opt.pars = c(lambda = T, mu = T, sigma = T, gamma = F))
vg_daily
# Create function to take 5, 10 or 30 rows away as the next datapoint
get_data <- function(x,days){
  len <- dim(x)
  len <- len[1]
  index_subset <- seq(1,len,days)
  x <- x[index_subset,]
  return (x)
}
# 5-day return
data_berk1_5 <- get_data(data_berk1,5) # taking 5 rows away to calculate next return
data_ibm1_5 <- get_data(data_ibm1,5) # taking 5 rows away to calculate next return
data_berk_5_returns <- dailyReturn(data_berk1_5[,6], subset=NULL, type='arithmetic', leading=TRUE)
data_ibm_5_returns <- dailyReturn(data_ibm1_5[,6], subset=NULL, type='arithmetic', leading=TRUE)
plot(data_berk_5_returns)
plot(data_ibm_5_returns)
data_berk_5.df <-as.data.frame(data_berk_5_returns)
data_ibm_5.df <- as.data.frame(data_ibm_5_returns)
plot(data_berk_5.df$daily.returns, data_ibm_5.df$daily.returns,xlab='berk 5-day return',
     ylab='ibm 5-day return',main='Bivariate 5-day Return Data')
combined_5 <- cbind(data_berk_5_returns,data_ibm_5_returns)
combined_5 <- as.matrix(combined_5)
normal_5 <-fit.gaussmv(combined_5, na.rm = T, save.data = T)
normal_5
t_5 <- fit.tmv(combined_5, nu = 3.5,
                 opt.pars = c(lambda = T, mu = T, sigma = T, gamma = F,symmetric = T))
t_5
vg_5 <-fit.VGmv(combined_5, lambda = 1, opt.pars = c(lambda = T, mu = T, sigma = T, gamma = F))
vg_5
# 10-day return
data_berk1_10 <- get_data(data_berk1,10) # taking 10 rows away to calculate next return
data_ibm1_10 <- get_data(data_ibm1,10) # taking 10 rows away to calculate next return
data_berk_10_returns <- dailyReturn(data_berk1_10[,6], subset=NULL, type='arithmetic', leading=TRUE)
data_ibm_10_returns <- dailyReturn(data_ibm1_10[,6], subset=NULL, type='arithmetic', leading=TRUE)
plot(data_berk_10_returns)
plot(data_ibm_10_returns)
data_berk_10.df <-as.data.frame(data_berk_10_returns)
data_ibm_10.df <- as.data.frame(data_ibm_10_returns)
plot(data_berk_10.df$daily.returns, data_ibm_10.df$daily.returns,xlab='berk 10-day return',
     ylab='ibm 10-day return',main='Bivariate 10-day Return Data')
combined_10 <- cbind(data_berk_10_returns,data_ibm_10_returns)
combined_10 <- as.matrix(combined_10)
normal_10 <-fit.gaussmv(combined_10, na.rm = T, save.data = T)
normal_10
t_10 <- fit.tmv(combined_10, nu = 3.5,
               opt.pars = c(lambda = T, mu = T, sigma = T, gamma = F,symmetric = T))
t_10
vg_10 <-fit.VGmv(combined_10, lambda = 1, opt.pars = c(lambda = T, mu = T, sigma = T, gamma = F))
vg_10
# 30-day return
data_berk1_30 <- get_data(data_berk1,30) # taking 30 rows away to calculate next return
data_ibm1_30 <- get_data(data_ibm1,30) # taking 30 rows away to calculate next return
data_berk_30_returns <- dailyReturn(data_berk1_30[,6], subset=NULL, type='arithmetic', leading=TRUE)
data_ibm_30_returns <- dailyReturn(data_ibm1_30[,6], subset=NULL, type='arithmetic', leading=TRUE)
plot(data_berk_30_returns)
plot(data_ibm_30_returns)
data_berk_30.df <-as.data.frame(data_berk_30_returns)
data_ibm_30.df <- as.data.frame(data_ibm_30_returns)
plot(data_berk_30.df$daily.returns, data_ibm_30.df$daily.returns,xlab='berk 30-day return',
     ylab='ibm 30-day return',main='Bivariate 30-day Return Data')
combined_30 <- cbind(data_berk_30_returns,data_ibm_30_returns)
combined_30 <- as.matrix(combined_30)
normal_30 <-fit.gaussmv(combined_30, na.rm = T, save.data = T)
normal_30
t_30 <- fit.tmv(combined_30, nu = 3.5,
                opt.pars = c(lambda = T, mu = T, sigma = T, gamma = F,symmetric = T))
t_30
vg_30 <-fit.VGmv(combined_30, lambda = 1, opt.pars = c(lambda = T, mu = T, sigma = T, gamma = F))
vg_30
## Simulation, 2-dimensional Daily returns
set.seed(123)
N <- 200 # Number of random samples
# Function to draw ellipse for bivariate normal data
ellipse_bvn <- function(bvn, alpha){
  Xbar <- apply(bvn,2,mean)
  S <- cov(bvn)
  ellipse(Xbar, S, alpha = alpha, col="red")
}
mu_n_daily <- c(0.0012778142, 0.0005787398)
sigma_n_daily <- matrix(c(2.007310e-04, 3.928569e-05, 3.928569e-05, 2.673923e-04),2)
library('MASS')
bvn1 <- mvrnorm(N, mu = mu_n_daily, Sigma = sigma_n_daily)
colnames(bvn1) <- c("bvn_n_daily_berk","bvn_n_daily_ibm")
mu_t_daily <- c(0.0007520335, 0.0002037140)
sigma_t_daily <- matrix(c(1.898900e-04, 2.632227e-05, 2.632227e-05, 3.333447e-04),2)
bvn2 <- mvrnorm(N, mu = mu_t_daily, Sigma = sigma_t_daily)
colnames(bvn2) <- c("bvn_t_daily_berk","bvn_t_daily_ibm")
mu_vg_daily <- c(1.437006e-17, 2.075176e-17)
sigma_vg_daily <- matrix(c(1.771995e-04, 2.502633e-05, 2.502633e-05, 4.085398e-04),2)
bvn3 <- mvrnorm(N, mu = mu_vg_daily, Sigma = sigma_vg_daily)
colnames(bvn3) <- c("bvn_vg_daily_berk","bvn_vg_daily_ibm")
#plot
bvn <- list(bvn1,bvn2,bvn3)
par(mfrow=c(2,2)) 
plot(bvn1, xlab="berk",ylab="ibm",main= "Daily Return of All Samples",col=4)
# install.packages("mixtools")
library('mixtools')
for(i in 2:3){
  points(bvn[[i]],col=i+3)
}
for(i in 1:3){
  #item <- paste("bvn",i,sep="")
  item <- c('Normal Distribution','T Distribution','Variance Gamma  Distribution')
  plot(bvn[[i]],xlab="berk",ylab="ibm",main=item[i], col=i+3)
  ellipse_bvn(bvn[[i]],.5) # the larger ellipse
  ellipse_bvn(bvn[[i]],.05) # the smaller ellipse
}
par(mfrow=c(1,1))
## Heat map image
# Calculate kernel density estimate for normal distribution
bivn.kde_n_daily <- kde2d(bvn1[,1], bvn1[,2], n = 50)
# Contour plot overlayed on heat map image of results
image(bivn.kde_n_daily, xlab = 'BERK',ylab = 'IBM',main='Heat Map of Daily Return (Normal Distribution)')     
contour(bivn.kde_n_daily, add = TRUE)  
# Calculate kernel density estimate for t distribution
bivn.kde_t_daily <- kde2d(bvn2[,1], bvn2[,2], n = 50)
# Contour plot overlayed on heat map image of results
image(bivn.kde_t_daily, xlab = 'BERK',ylab = 'IBM',main='Heat Map of Daily Return (T Distribution)')     
contour(bivn.kde_t_daily, add = TRUE)  
# Calculate kernel density estimate for variance gamma distribution
bivn.kde_vg_daily <- kde2d(bvn3[,1], bvn2[,2], n = 50)
# Contour plot overlayed on heat map image of results
image(bivn.kde_vg_daily, xlab = 'BERK',ylab = 'IBM',main='Heat Map of Daily Return (Variance Gamma Distribution)')     
contour(bivn.kde_vg_daily, add = TRUE)  
## 3-D :threejs Javascript plot
# install.packages("threejs")
library('threejs')
# Normal Distribution
# Unpack data from kde grid format
x <- bivn.kde_n_daily$x 
y <- bivn.kde_n_daily$y
z <- bivn.kde_n_daily$z
# Construct x,y,z coordinates
xx <- rep(x,times=length(y))
yy <- rep(y,each=length(x))
zz <- z; dim(zz) <- NULL
# Set up color range
ra <- ceiling(16 * zz/max(zz))
col <- rainbow(16, 2/3)
# 3D interactive scatter plot
scatterplot3js(x=xx,y=yy,z=zz,size=0.4,color = col[ra],bg="black")
# t Distribution
x <- bivn.kde_t_daily$x 
y <- bivn.kde_t_daily$y
z <- bivn.kde_t_daily$z
xx <- rep(x,times=length(y))
yy <- rep(y,each=length(x))
zz <- z; dim(zz) <- NULL
ra <- ceiling(16 * zz/max(zz))
col <- rainbow(16, 2/3)
scatterplot3js(x=xx,y=yy,z=zz,size=0.4,color = col[ra],bg="black")
# Vg Distribution
x <- bivn.kde_vg_daily$x 
y <- bivn.kde_vg_daily$y
z <- bivn.kde_vg_daily$z
xx <- rep(x,times=length(y))
yy <- rep(y,each=length(x))
zz <- z; dim(zz) <- NULL
ra <- ceiling(16 * zz/max(zz))
col <- rainbow(16, 2/3)
scatterplot3js(x=xx,y=yy,z=zz,size=0.4,color = col[ra],bg="black")
## Simulation, 2-dimensional 5-day returns
set.seed(123)
mu_n_5 <- c(0.006463430, 0.002822228)
sigma_n_5 <- matrix(c(0.0011389018, 0.0002171612, 0.0002171612, 0.0011891374),2)
bvn1 <- mvrnorm(N, mu = mu_n_5, Sigma = sigma_n_5)
colnames(bvn1) <- c("bvn_n_5_berk","bvn_n_5_ibm")
mu_t_5 <- c(0.005131390, 0.002312292)
sigma_t_daily <- matrix(c(0.0010446546, 0.0001714626, 0.0001714626, 0.0012922084),2)
bvn2 <- mvrnorm(N, mu = mu_t_5, Sigma = sigma_t_daily)
colnames(bvn2) <- c("bvn_t_5_berk","bvn_t_5_ibm")
mu_vg_5 <- c(0.004774874, 0.001854854)
sigma_vg_5 <- matrix(c(0.0009997376, 0.0001672785, 0.0001672785, 0.0012079432),2)
bvn3 <- mvrnorm(N, mu = mu_vg_5, Sigma = sigma_vg_5)
colnames(bvn3) <- c("bvn_vg_5_berk","bvn_vg_5_ibm")
bvn <- list(bvn1,bvn2,bvn3)
par(mfrow=c(2,2)) 
plot(bvn1, xlab="berk",ylab="ibm",main= "5-day Return of All Samples",col=4)
for(i in 2:3){
  points(bvn[[i]],col=i+3)
}
for(i in 1:3){
  #item <- paste("bvn",i,sep="")
  item <- c('Normal Distribution','T Distribution','Variance Gamma  Distribution')
  plot(bvn[[i]],xlab="berk",ylab="ibm",main=item[i], col=i+3)
  ellipse_bvn(bvn[[i]],.5) # the larger ellipse
  ellipse_bvn(bvn[[i]],.05) # the smaller ellipse
}
par(mfrow=c(1,1))
bivn.kde_n_5 <- kde2d(bvn1[,1], bvn1[,2], n = 50)
image(bivn.kde_n_5, xlab = 'BERK',ylab = 'IBM',main='Heat Map of 5-day Return (Normal Distribution)')     
contour(bivn.kde_n_5, add = TRUE)  
bivn.kde_t_5 <- kde2d(bvn2[,1], bvn2[,2], n = 50)
image(bivn.kde_t_5, xlab = 'BERK',ylab = 'IBM',main='Heat Map of 5-day Return (T Distribution)')     
contour(bivn.kde_t_5, add = TRUE)  
bivn.kde_vg_5 <- kde2d(bvn3[,1], bvn2[,2], n = 50)
image(bivn.kde_vg_5, xlab = 'BERK',ylab = 'IBM',main='Heat Map of 5-day Return (Variance Gamma Distribution)')     
contour(bivn.kde_vg_5, add = TRUE)  
# Normal Distribution
x <- bivn.kde_n_5$x 
y <- bivn.kde_n_5$y
z <- bivn.kde_n_5$z
xx <- rep(x,times=length(y))
yy <- rep(y,each=length(x))
zz <- z; dim(zz) <- NULL
ra <- ceiling(16 * zz/max(zz))
col <- rainbow(16, 2/3)
scatterplot3js(x=xx,y=yy,z=zz,size=0.4,color = col[ra],bg="black")
# t Distribution
x <- bivn.kde_t_5$x 
y <- bivn.kde_t_5$y
z <- bivn.kde_t_5$z
xx <- rep(x,times=length(y))
yy <- rep(y,each=length(x))
zz <- z; dim(zz) <- NULL
ra <- ceiling(16 * zz/max(zz))
col <- rainbow(16, 2/3)
scatterplot3js(x=xx,y=yy,z=zz,size=0.4,color = col[ra],bg="black")
# Vg Distribution
x <- bivn.kde_vg_5$x 
y <- bivn.kde_vg_5$y
z <- bivn.kde_vg_5$z
xx <- rep(x,times=length(y))
yy <- rep(y,each=length(x))
zz <- z; dim(zz) <- NULL
ra <- ceiling(16 * zz/max(zz))
col <- rainbow(16, 2/3)
scatterplot3js(x=xx,y=yy,z=zz,size=0.4,color = col[ra],bg="black")
## Simulation, 2-dimensional 10-day returns
set.seed(123)
mu_n_10 <- c(0.012946110, 0.005755622)
sigma_n_10 <- matrix(c(0.0022897350, 0.0005130391, 0.0005130391, 0.0024735465),2)
bvn1 <- mvrnorm(N, mu = mu_n_10, Sigma = sigma_n_10)
colnames(bvn1) <- c("bvn_n_10_berk","bvn_n_10_ibm")
mu_t_10 <- c(0.009793609, 0.005299804)
sigma_t_10 <- matrix(c(0.0021945925, 0.0005254773, 0.0005254773, 0.0026337181),2)
bvn2 <- mvrnorm(N, mu = mu_t_10, Sigma = sigma_t_10)
colnames(bvn2) <- c("bvn_t_10_berk","bvn_t_10_ibm")
mu_vg_10 <- c(0.009396459, 0.004750057)
sigma_vg_10 <- matrix(c(0.0021222268, 0.0005003912, 0.0005003912, 0.0025360141),2)
bvn3 <- mvrnorm(N, mu = mu_vg_10, Sigma = sigma_vg_10)
colnames(bvn3) <- c("bvn_vg_10_berk","bvn_vg_10_ibm")
bvn <- list(bvn1,bvn2,bvn3)
par(mfrow=c(2,2)) 
plot(bvn1, xlab="berk",ylab="ibm",main= "10-day Return of All Samples",col=4)
for(i in 2:3){
  points(bvn[[i]],col=i+3)
}
for(i in 1:3){
  #item <- paste("bvn",i,sep="")
  item <- c('Normal Distribution','T Distribution','Variance Gamma  Distribution')
  plot(bvn[[i]],xlab="berk",ylab="ibm",main=item[i], col=i+3)
  ellipse_bvn(bvn[[i]],.5) # the larger ellipse
  ellipse_bvn(bvn[[i]],.05) # the smaller ellipse
}
par(mfrow=c(1,1))
bivn.kde_n_10 <- kde2d(bvn1[,1], bvn1[,2], n = 50)
image(bivn.kde_n_10, xlab = 'BERK',ylab = 'IBM',main='Heat Map of 10-day Return (Normal Distribution)')     
contour(bivn.kde_n_10, add = TRUE)  
bivn.kde_t_10 <- kde2d(bvn2[,1], bvn2[,2], n = 50)
image(bivn.kde_t_10, xlab = 'BERK',ylab = 'IBM',main='Heat Map of 10-day Return (T Distribution)')     
contour(bivn.kde_t_10, add = TRUE)  
bivn.kde_vg_10 <- kde2d(bvn3[,1], bvn2[,2], n = 50)
image(bivn.kde_vg_10, xlab = 'BERK',ylab = 'IBM',main='Heat Map of 10-day Return (Variance Gamma Distribution)')     
contour(bivn.kde_vg_10, add = TRUE)  
# Normal Distribution
x <- bivn.kde_n_10$x 
y <- bivn.kde_n_10$y
z <- bivn.kde_n_10$z
xx <- rep(x,times=length(y))
yy <- rep(y,each=length(x))
zz <- z; dim(zz) <- NULL
ra <- ceiling(16 * zz/max(zz))
col <- rainbow(16, 2/3)
scatterplot3js(x=xx,y=yy,z=zz,size=0.4,color = col[ra],bg="black")
# t Distribution
x <- bivn.kde_t_10$x 
y <- bivn.kde_t_10$y
z <- bivn.kde_t_10$z
xx <- rep(x,times=length(y))
yy <- rep(y,each=length(x))
zz <- z; dim(zz) <- NULL
ra <- ceiling(16 * zz/max(zz))
col <- rainbow(16, 2/3)
scatterplot3js(x=xx,y=yy,z=zz,size=0.4,color = col[ra],bg="black")
# Vg Distribution
x <- bivn.kde_vg_10$x 
y <- bivn.kde_vg_10$y
z <- bivn.kde_vg_10$z
xx <- rep(x,times=length(y))
yy <- rep(y,each=length(x))
zz <- z; dim(zz) <- NULL
ra <- ceiling(16 * zz/max(zz))
col <- rainbow(16, 2/3)
scatterplot3js(x=xx,y=yy,z=zz,size=0.4,color = col[ra],bg="black")
## Simulation, 2-dimensional 30-day returns
set.seed(123)
mu_n_30 <- c(0.03874097, 0.01768223)
sigma_n_30 <- matrix(c(0.0062190534, 0.0009888482, 0.0009888482, 0.0082518341),2)
bvn1 <- mvrnorm(N, mu = mu_n_30, Sigma = sigma_n_30)
colnames(bvn1) <- c("bvn_n_30_berk","bvn_n_30_ibm")
mu_t_30 <- c(0.03798158, 0.01821170)
sigma_t_30 <- matrix(c(0.006300650, 0.000976472, 0.000976472, 0.008071939),2)
bvn2 <- mvrnorm(N, mu = mu_t_30, Sigma = sigma_t_30)
colnames(bvn2) <- c("bvn_t_30_berk","bvn_t_30_ibm")
mu_vg_30 <- c(0.03787357, 0.01794595)
sigma_vg_30 <- matrix(c(0.0062760642, 0.0009933303, 0.0009933303, 0.0080818575),2)
bvn3 <- mvrnorm(N, mu = mu_vg_30, Sigma = sigma_vg_30)
colnames(bvn3) <- c("bvn_vg_30_berk","bvn_vg_30_ibm")
bvn <- list(bvn1,bvn2,bvn3)
par(mfrow=c(2,2)) 
plot(bvn1, xlab="berk",ylab="ibm",main= "30-day Return of All Samples",col=4)
for(i in 2:3){
  points(bvn[[i]],col=i+3)
}
for(i in 1:3){
  #item <- paste("bvn",i,sep="")
  item <- c('Normal Distribution','T Distribution','Variance Gamma  Distribution')
  plot(bvn[[i]],xlab="berk",ylab="ibm",main=item[i], col=i+3)
  ellipse_bvn(bvn[[i]],.5) # the larger ellipse
  ellipse_bvn(bvn[[i]],.05) # the smaller ellipse
}
par(mfrow=c(1,1))
bivn.kde_n_30 <- kde2d(bvn1[,1], bvn1[,2], n = 50)
image(bivn.kde_n_30, xlab = 'BERK',ylab = 'IBM',main='Heat Map of 30-day Return (Normal Distribution)')     
contour(bivn.kde_n_30, add = TRUE)  
bivn.kde_t_30 <- kde2d(bvn2[,1], bvn2[,2], n = 50)
image(bivn.kde_t_30, xlab = 'BERK',ylab = 'IBM',main='Heat Map of 30-day Return (T Distribution)')     
contour(bivn.kde_t_30, add = TRUE)  
bivn.kde_vg_30 <- kde2d(bvn3[,1], bvn2[,2], n = 50)
image(bivn.kde_vg_30, xlab = 'BERK',ylab = 'IBM',main='Heat Map of 30-day Return (Variance Gamma Distribution)')     
contour(bivn.kde_vg_30, add = TRUE)  
# Normal Distribution
x <- bivn.kde_n_30$x 
y <- bivn.kde_n_30$y
z <- bivn.kde_n_30$z
xx <- rep(x,times=length(y))
yy <- rep(y,each=length(x))
zz <- z; dim(zz) <- NULL
ra <- ceiling(16 * zz/max(zz))
col <- rainbow(16, 2/3)
scatterplot3js(x=xx,y=yy,z=zz,size=0.4,color = col[ra],bg="black")
# t Distribution
x <- bivn.kde_t_30$x 
y <- bivn.kde_t_30$y
z <- bivn.kde_t_30$z
xx <- rep(x,times=length(y))
yy <- rep(y,each=length(x))
zz <- z; dim(zz) <- NULL
ra <- ceiling(16 * zz/max(zz))
col <- rainbow(16, 2/3)
scatterplot3js(x=xx,y=yy,z=zz,size=0.4,color = col[ra],bg="black")
# Vg Distribution
x <- bivn.kde_vg_30$x 
y <- bivn.kde_vg_30$y
z <- bivn.kde_vg_30$z
xx <- rep(x,times=length(y))
yy <- rep(y,each=length(x))
zz <- z; dim(zz) <- NULL
ra <- ceiling(16 * zz/max(zz))
col <- rainbow(16, 2/3)
scatterplot3js(x=xx,y=yy,z=zz,size=0.4,color = col[ra],bg="black")
## Calculating VaR for the 10000 simulated data points
set.seed(123)
# Daily returns, normal, then t, then vg
out_sample_normal_daily <- rghyp(10000, object = normal_daily)
out_berk_normal_daily <- out_sample_normal_daily[,1]
out_ibm_normal_daily <- out_sample_normal_daily[,2]
quantile(out_berk_normal_daily,c(0.001,0.01,0.05))
quantile(out_ibm_normal_daily,c(0.001,0.01,0.05))
out_sample_t_daily <- rghyp(10000, object = t_daily)
out_berk_t_daily <- out_sample_t_daily[,1]
out_ibm_t_daily <- out_sample_t_daily[,2]
quantile(out_berk_t_daily,c(0.001,0.01,0.05))
quantile(out_ibm_t_daily,c(0.001,0.01,0.05))
out_sample_vg_daily <- rghyp(10000, object = vg_daily)
out_berk_vg_daily <- out_sample_vg_daily[,1]
out_ibm_vg_daily <- out_sample_vg_daily[,2]
quantile(out_berk_vg_daily,c(0.001,0.01,0.05))
quantile(out_ibm_vg_daily,c(0.001,0.01,0.05))
# 5-day returns, normal, then t, then vg
out_sample_normal_5 <- rghyp(10000, object = normal_5)
out_berk_normal_5 <- out_sample_normal_5[,1]
out_ibm_normal_5 <- out_sample_normal_5[,2]
quantile(out_berk_normal_5,c(0.001,0.01,0.05))
quantile(out_ibm_normal_5,c(0.001,0.01,0.05))
out_sample_t_5 <- rghyp(10000, object = t_5)
out_berk_t_5 <- out_sample_t_5[,1]
out_ibm_t_5 <- out_sample_t_5[,2]
quantile(out_berk_t_5,c(0.001,0.01,0.05))
quantile(out_ibm_t_5,c(0.001,0.01,0.05))
out_sample_vg_5 <- rghyp(10000, object = vg_5)
out_berk_vg_5 <- out_sample_vg_5[,1]
out_ibm_vg_5 <- out_sample_vg_5[,2]
quantile(out_berk_vg_5,c(0.001,0.01,0.05))
quantile(out_ibm_vg_5,c(0.001,0.01,0.05))
# 10-day returns, normal, then t, then vg
out_sample_normal_10 <- rghyp(10000, object = normal_10)
out_berk_normal_10 <- out_sample_normal_10[,1]
out_ibm_normal_10 <- out_sample_normal_10[,2]
quantile(out_berk_normal_10,c(0.001,0.01,0.05))
quantile(out_ibm_normal_10,c(0.001,0.01,0.05))
out_sample_t_10 <- rghyp(10000, object = t_10)
out_berk_t_10 <- out_sample_t_10[,1]
out_ibm_t_10 <- out_sample_t_10[,2]
quantile(out_berk_t_10,c(0.001,0.01,0.05))
quantile(out_ibm_t_10,c(0.001,0.01,0.05))
out_sample_vg_10 <- rghyp(10000, object = vg_10)
out_berk_vg_10 <- out_sample_vg_10[,1]
out_ibm_vg_10 <- out_sample_vg_10[,2]
quantile(out_berk_vg_10,c(0.001,0.01,0.05))
quantile(out_ibm_vg_10,c(0.001,0.01,0.05))
# 30-day returns, normal, then t, then vg
out_sample_normal_30 <- rghyp(10000, object = normal_30)
out_berk_normal_30 <- out_sample_normal_30[,1]
out_ibm_normal_30 <- out_sample_normal_30[,2]
quantile(out_berk_normal_30,c(0.001,0.01,0.05))
quantile(out_ibm_normal_30,c(0.001,0.01,0.05))
out_sample_t_30 <- rghyp(10000, object = t_30)
out_berk_t_30 <- out_sample_t_30[,1]
out_ibm_t_30 <- out_sample_t_30[,2]
quantile(out_berk_t_30,c(0.001,0.01,0.05))
quantile(out_ibm_t_30,c(0.001,0.01,0.05))
out_sample_vg_30 <- rghyp(10000, object = vg_30)
out_berk_vg_30 <- out_sample_vg_30[,1]
out_ibm_vg_30 <- out_sample_vg_30[,2]
quantile(out_berk_vg_30,c(0.001,0.01,0.05))
quantile(out_ibm_vg_30,c(0.001,0.01,0.05))
## Get 2nd half returns
data_berk2 <- data_berk[4658:9314,]
data_ibm2 <- data_ibm[4658:9314,]
data_berk_daily <- dailyReturn(data_berk2[,6], subset=NULL, type='arithmetic', leading=TRUE)
data_ibm_daily <- dailyReturn(data_ibm2[,6], subset=NULL, type='arithmetic', leading=TRUE)
data_berk2_5 <- get_data(data_berk2,5) # taking 5 rows away to calculate next return
data_ibm2_5 <- get_data(data_ibm2,5) # taking 5 rows away to calculate next return
data_berk_5_returns <- dailyReturn(data_berk2_5[,6], subset=NULL, type='arithmetic', leading=TRUE)
data_ibm_5_returns <- dailyReturn(data_ibm2_5[,6], subset=NULL, type='arithmetic', leading=TRUE)
data_berk2_10 <- get_data(data_berk2,10) # taking 10 rows away to calculate next return
data_ibm2_10 <- get_data(data_ibm2,10) # taking 10 rows away to calculate next return
data_berk_10_returns <- dailyReturn(data_berk2_10[,6], subset=NULL, type='arithmetic', leading=TRUE)
data_ibm_10_returns <- dailyReturn(data_ibm2_10[,6], subset=NULL, type='arithmetic', leading=TRUE)
data_berk2_30 <- get_data(data_berk2,30) # taking 30 rows away to calculate next return
data_ibm2_30 <- get_data(data_ibm2,30) # taking 30 rows away to calculate next return
data_berk_30_returns <- dailyReturn(data_berk2_30[,6], subset=NULL, type='arithmetic', leading=TRUE)
data_ibm_30_returns <- dailyReturn(data_ibm2_30[,6], subset=NULL, type='arithmetic', leading=TRUE)
plot(data_berk_daily)
plot(data_ibm_daily)
plot(data_berk_5_returns)
plot(data_ibm_5_returns)
plot(data_berk_10_returns)
plot(data_ibm_10_returns)
plot(data_berk_30_returns)
plot(data_ibm_30_returns)
## Actual VaR in 2nd half compared with quantile of out-of-sample simulated data points
quantile(data_berk_daily,c(0.001,0.01,0.05))
quantile(data_ibm_daily,c(0.001,0.01,0.05))
quantile(out_berk_normal_daily,c(0.001,0.01,0.05))
quantile(out_ibm_normal_daily,c(0.001,0.01,0.05))
quantile(out_berk_t_daily,c(0.001,0.01,0.05))
quantile(out_ibm_t_daily,c(0.001,0.01,0.05))
quantile(out_berk_vg_daily,c(0.001,0.01,0.05))
quantile(out_ibm_vg_daily,c(0.001,0.01,0.05))
quantile(data_berk_5_returns,c(0.001,0.01,0.05))
quantile(data_ibm_5_returns,c(0.001,0.01,0.05))
quantile(out_berk_normal_5,c(0.001,0.01,0.05))
quantile(out_ibm_normal_5,c(0.001,0.01,0.05))
quantile(out_berk_t_5,c(0.001,0.01,0.05))
quantile(out_ibm_t_5,c(0.001,0.01,0.05))
quantile(out_berk_vg_5,c(0.001,0.01,0.05))
quantile(out_ibm_vg_5,c(0.001,0.01,0.05))
quantile(data_berk_10_returns,c(0.001,0.01,0.05))
quantile(data_ibm_10_returns,c(0.001,0.01,0.05))
quantile(out_berk_normal_10,c(0.001,0.01,0.05))
quantile(out_ibm_normal_10,c(0.001,0.01,0.05))
quantile(out_berk_t_10,c(0.001,0.01,0.05))
quantile(out_ibm_t_10,c(0.001,0.01,0.05))
quantile(out_berk_vg_10,c(0.001,0.01,0.05))
quantile(out_ibm_vg_10,c(0.001,0.01,0.05))
quantile(data_berk_30_returns,c(0.001,0.01,0.05))
quantile(data_ibm_30_returns,c(0.001,0.01,0.05))
quantile(out_berk_normal_30,c(0.001,0.01,0.05))
quantile(out_ibm_normal_30,c(0.001,0.01,0.05))
quantile(out_berk_t_30,c(0.001,0.01,0.05))
quantile(out_ibm_t_30,c(0.001,0.01,0.05))
quantile(out_berk_vg_30,c(0.001,0.01,0.05))
quantile(out_ibm_vg_30,c(0.001,0.01,0.05))