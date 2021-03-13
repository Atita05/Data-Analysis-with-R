
# Task:1 BDS1 file
#---------------------------------PART:-1------------------------------------------
#Train your DS implementation on the training set. 
#Find the MSE on the test set. Include it in your report.
#Splitting the data 
library(MASS)
attach(Boston)
#load data
Boston.X <- data.frame(lstat,rm)
# giving 50% test size here
Test_data_Size = nrow(Boston.X) /2
# random size
set.seed(1811)
#first will take test data and we will exclude test data from training 
Test_Data_Index <- sample(seq_len(nrow(Boston.X)),size = Test_data_Size)
#splitting into lstat and rm
TrainX <- Boston.X[-Test_Data_Index,] #excluding test data
TrainY <- medv[-Test_Data_Index]
TestX <- Boston.X[Test_Data_Index,]
Testy <- medv[Test_Data_Index]
#---------------------------------PART:-2 FINDING MSE FOR LSTAT WITH GRAPH------------------------------------------
# as per mentioned in DS algoritm:- 
#A decision stump is specified by its attribute (lstat or rm) and the threshold s.
#(Consider, e.g., s = 1.8, 1.9, . . . , 37.9 in the case of lstat and s = 3.6, 3.7, . . . , 8.7 in the case of rm.) 
#The training RSS of a decision stump (lstat, s) is

#Finding the best split for lstat and rm here

s.lstat<-seq(1.8, 37.9, by=0.1)
s.rm<-seq(3.6, 8.7, by=0.1)

#lstat
RSS.lstat <- c()  #initializing first null array for lstat
s.lstat.LT.avg <- c() #initializing first null array for lstat less than
s.lstat.GT.avg <- c()#initializing first null array for lstat greater than


for (i in 1:length(s.lstat)) {
  s.lstat.LT <- sum(TrainY[TrainX$lstat < s.lstat[i]])
  div.LT <- sum(TrainX$lstat < s.lstat[i])
  div.GT <-sum(TrainX$lstat >= s.lstat[i])
  s.lstat.GT <- sum(TrainY[TrainX$lstat >= s.lstat[i]])
  sum(TrainY[TrainX$lstat < s.lstat[i]])/sum(TrainX$lstat < s.lstat[i])
  s.lstat.LT.avg[i] <- s.lstat.LT/div.LT
  s.lstat.GT.avg[i] <- s.lstat.GT/div.GT
  RSS.lstat[i] <- sum((TrainY[TrainX$lstat < s.lstat[i]]-s.lstat.LT.avg[i])^2) + sum((TrainY[TrainX$lstat >= s.lstat[i]]-s.lstat.GT.avg[i])^2)
}
#-------------------------------------graph for lstat--------------------------------
par(mfrow=c(1,2))
plot(s.lstat,RSS.lstat, type="l",main="RSS with splits of lstat",xlab="split for lstat",ylab="RSS", col="red", lty = 1:2, cex = 0.8)
min(RSS.lstat)

#--------------------#PART:-3 FINDING MSE FOR RSS WITH GRAPH (SIMILAR AS ABOVE)------------------------------------


RSS.rm <- c()  #initializing first null array for rm
s.rm.LT.avg <- c() #initializing first null array for rm less than
s.rm.GT.avg <- c()#initializing first null array for rm greater than


for (i in 1:length(s.rm)) {
  s.rm.LT <- sum(TrainY[TrainX$rm < s.rm[i]])
  div.LT <- sum(TrainX$rm < s.rm[i])
  div.GT <-sum(TrainX$rm >= s.rm[i])
  s.rm.GT <- sum(TrainY[TrainX$rm >= s.rm[i]])
  sum(TrainY[TrainX$rm < s.rm[i]])/sum(TrainX$rm < s.rm[i])
  s.rm.LT.avg[i] <- s.rm.LT/div.LT
  s.rm.GT.avg[i] <- s.rm.GT/div.GT
  RSS.rm[i] <- sum((TrainY[TrainX$rm < s.rm[i]]-s.rm.LT.avg[i])^2) + sum((TrainY[TrainX$rm >= s.rm[i]]-s.rm.GT.avg[i])^2)
}
#-------------------------------GRAPH FOR RM----------------------------------------------------------
plot(s.rm,RSS.rm, type="l",main="RSS with split of rm",xlab="split with RM",ylab="",col="green", lty = 1:2, cex = 0.8)
min(RSS.rm)
train.MSE <- min(RSS.rm)/nrow(TrainX)
#------------------------------PART :4 FINDING TEST MSE BY ABOVE OBSERVATION---------------------------------------------
#LS RSS IS 11425 AND RM RSS IS 9821.422 SO RM IS HAVING LOWER RSS 
# WE WILL CONSIDER RSS FOR THE TEST MSE CALCULATION

split <- s.rm[RSS.rm==min(RSS.rm)]
test.lessthan = 0
test.greterthan = 0
for (i in 1:nrow(TestX)){
  if (TestX$rm[i] < split){
    test.lessthan <- test.lessthan + (Testy[i]-s.rm.lessthan.ave[RSS.rm==min(RSS.rm)])^2
  }
  else{
    test.greterthan <- test.greterthan + (Testy[i]-s.rm.greaterthan.ave[RSS.rm==min(RSS.rm)])^2
  }
}

test.MSE <- (test.lessthan + test.greterthan)/nrow(TestX)
print(test.MSE)