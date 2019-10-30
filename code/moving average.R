dat1 <- read.csv("flow_20130225-20130303.csv",skip =1,header = TRUE)
tdata1 <- dat1[dat1$region_from == 5 &dat1$region_to == 1,]
tdata1[,8:13] <- list(NULL)
tdata1[,3:6] <- list(NULL)
dat2 <- read.csv("flow_20130304-20130310.csv",skip =1,header = TRUE)
tdata2 <- dat2[dat2$region_from == 5 &dat2$region_to == 1,]
tdata2[,8:13] <- list(NULL)
tdata2[,3:6] <- list(NULL)
dat3 <- read.csv("flow_20130311-20130317.csv",skip =1,header = TRUE)
tdata3 <- dat3[dat3$region_from == 5 &dat3$region_to == 1,]
tdata3[,8:13] <- list(NULL)
tdata3[,3:6] <- list(NULL)
dat4 <- read.csv("flow_20130318-20130324.csv",skip =1,header = TRUE)
tdata4 <- dat4[dat4$region_from == 5 &dat4$region_to == 1,]
tdata4[,8:13] <- list(NULL)
tdata4[,3:6] <- list(NULL)
#average model for prediction
install.packages('smooth')
require(smooth)
pacf(ts(tdata$v0_num_traj))
#ma model moving average training
tma <- ma(ts(tdata$v0_num_traj[1:300]),order=3)
summary(tma)
f <- forecast(tma,h =4)
f$mean
#plotting the ma
plot(forecast(tma))
#test data error and accruracy for 4 data points
accuracy(tma,tdata$v0_num_traj[301:304])
#model for prediction all values
tma2 <- ma(tdata$v0_num_traj,order=3)
tma2
plot(forecast(tma2,h =100))


