

# -------------------TASK 1:- SPLITTING THE DATA INTO TRAIN ANS TEST SPLIT---------------------
#Train your BDS implementation on the training set for learning rate η = 0.01 and B = 1000 trees. 
#Find the MSE on the test set. Include it in your report


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
Trainy <- medv[-Test_Data_Index]
TestX <- Boston.X[Test_Data_Index,]
Testy <- medv[Test_Data_Index]
#---------------------------------PART:-2 FINDING MSE FOR LSTAT WITH GRAPH------------------------------------------
# as per mentioned in DS algoritm:- 
#A decision stump is specified by its attribute (lstat or rm) and the threshold s.
#(Consider, e.g., s = 1.8, 1.9, . . . , 37.9 in the case of lstat and s = 3.6, 3.7, . . . , 8.7 in the case of rm.) 
#The training RSS of a decision stump (lstat, s) is

#Finding the best split for lstat and rm here

s.lstat<-seq(1.08, 37.9, by=0.1)
s.rm<-seq(3.6, 8.7, by=0.1)

#------------ PART:-3 assigning values of n=0.01 -----------------
Learning_Rate = 0.01
B=1

#----------------------------PART:-4 LSTAT---------------
test.pred <- matrix(0,1,length(Testy))
# b is given 1000
# this while loop will run 1000 times then b=b+1
while (B<1001) {
  RSS.lstat <- c() 
  s.lstat.LT.avg <- c()
  s.lstat.GT.avg <- c()
  for (i in 1:length(s.lstat)) {
    s.lstat.LT <- sum(Trainy[TrainX$lstat < s.lstat[i]])
    div.LT <- sum(TrainX$lstat < s.lstat[i])
    div.GT <-sum(TrainX$lstat >= s.lstat[i])
    s.lstat.GT <- sum(Trainy[TrainX$lstat >= s.lstat[i]])
    sum(Trainy[TrainX$lstat < s.lstat[i]])/sum(TrainX$lstat < s.lstat[i])
    s.lstat.LT.avg[i] <- s.lstat.LT/div.LT
    s.lstat.GT.avg[i] <- s.lstat.GT/div.GT
    RSS.lstat[i] <- sum((Trainy[TrainX$lstat < s.lstat[i]]-s.lstat.LT.avg[i])^2) + sum((Trainy[TrainX$lstat >= s.lstat[i]]-s.lstat.GT.avg[i])^2)
  }
  minimum.lstat <- min(RSS.lstat) #assigning value
  #---------------------------- PART:-5 RM---------------------------
  RSS.rm <- c() 
  s.rm.LT.avg  <- c()
  s.rm.GT.avg <- c()
  
  for (i in 1:length(s.rm)) {
    s.rm.LT <- sum(Trainy[TrainX$rm < s.rm[i]])
    div.LT <- sum(TrainX$rm < s.rm[i])
    div.GT <-sum(TrainX$rm >= s.rm[i])
    s.rm.GT <- sum(Trainy[TrainX$rm >= s.rm[i]])
    sum(Trainy[TrainX$rm < s.rm[i]])/sum(TrainX$rm < s.rm[i])
    s.rm.LT.avg[i] <- s.rm.LT/div.LT
    s.rm.GT.avg[i] <- s.rm.GT/div.GT
    # RSS calculations
    RSS.rm[i] <- sum((Trainy[TrainX$rm < s.rm[i]]-s.rm.LT.avg[i])^2) + sum((Trainy[TrainX$rm >= s.rm[i]]-s.rm.GT.avg[i])^2)
  }
  #---------------------------------PART:-6 ABOVE PART WE ALREADY DID IN TASK 1---------------------------------
  
  minimum.rm <- min(RSS.rm) # ASSIGNING MINIMUM VALUE TO RM
  # IF min of rm is less than lstat then
  if (minimum.rm<minimum.lstat){
    split <- min(s.rm[RSS.rm==min(RSS.rm)])
    formulaLT.lstat<-min(s.rm.LT.avg [RSS.rm==min(RSS.rm)])
    formulaGT.lstat<-min(Learning_Rate * s.rm.GT.avg[RSS.rm==min(RSS.rm)])
    for (i in 1:nrow(TrainX)){
      if  (TrainX$rm[i] < split){
        Trainy[i] <- Trainy[i] - Learning_Rate * formulaLT.lstat
      }
      else{
        Trainy[i] <- Trainy[i] - formulaGT.lstat 
      }
      
    } 
    
    for (j in 1:length(test.pred)){
      if (TestX$rm[j] < split){
        test.pred[j] <- test.pred[j] + Learning_Rate * formulaLT.lstat
      }
      else{
        test.pred[j] <- test.pred[j] + formulaGT.lstat
      }
    }
  }
  else{
    split <- min(s.lstat[RSS.lstat==min(RSS.lstat)])
    formula.lstat.lt=min(s.lstat.LT.avg[RSS.lstat==min(RSS.lstat)])
    formula.lstat.gt=min(s.lstat.GT.avg[RSS.lstat==min(RSS.lstat)])
    for (i in 1:nrow(TrainX)){
      if  (TrainX$lstat[i] < split){
        Trainy[i] <- Trainy[i] - Learning_Rate *formula.lstat.lt
      }
      else{
        Trainy[i] <- Trainy[i] - Learning_Rate * formula.lstat.gt
      }
      
    }
    
    for (j in 1:length(test.pred)){
      if (TestX$lstat[j] < split){
        test.pred[j] <- test.pred[j] + Learning_Rate * formula.lstat.lt
      }
      else{
        test.pred[j] <- test.pred[j] + Learning_Rate * formula.lstat.gt
      }
    }
  }
  B = B + 1
  
}
#-------------------PART:-6 FINDING MSE------------------
Test.MSE <- sum((Testy-test.pred)^2)/length(Testy)
print('your test mse is :')
print(Test.MSE)