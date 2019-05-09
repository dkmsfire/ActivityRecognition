#load("motion.rda")
load("motion26.rda")
motion[,19] = as.factor(motion[,19])
set.seed(999)
library(randomForest)
traini = sample(1:1917702,1534162)
traind = motion[traini,]
testd = motion[-traini,]

##5-folds cross-validation
folds = 5
holdout = split(sample(1:nrow(motion)), 1:folds)
for(i in 1:5){
  if(i != folds){
    test = motion[holdout[[i]],]
    validation = motion[holdout[[i + 1]],]
    train = motion[-c(holdout[[i]], holdout[[i + 1]]),]
  }else{
    test = motion[holdout[[folds]],]
    validation = motion[holdout[[1]],]
    train = motion[-c(holdout[[folds]], holdout[[1]]),]
  }
  model = randomForest(train[,1:9], y = train[,19], ntree = 5)
  pred_train = predict(model, train)
  pred_vali = predict(model, validation)
  pred_test = predict(model, test)
  print("Train")
  print(mean(pred_train == train[,19]))
  print("Validation")
  print(mean(pred_vali == validation[,19]))
  print("test")
  print(mean(pred_test == test[,19]))
}

##model1 using all attribute
model1 = randomForest(traind[,1:18], y = traind[,19], ntree = 15)
pred1 = predict(model1, newdata = testd)
result1 = as.data.frame(table(Real = testd[,19], Predict = pred1))
impor = importance(model1)

all = 0
for(i in 1:18){
  all = all + impor[i]
}
for(i in 1:18){
  impor[i] = impor[i] / all
}

#model2 using diff attribute
model2 = randomForest(x = traind[,10:18], y = traind[,19], ntree=15, importance = T)
pred2 = predict(model2, newdata = testd)
results2 = as.data.frame(table(Real = testd[,19],Predict = pred2))
impor = importance(model2)
#model3 using origing attribute
model3 = randomForest(x = traind[,1:9], y = traind[,19], ntree=15)
pred3 = predict(model3, newdata = testd)
mean(pred3 == testd[,19])
results3 = as.data.frame(table(Real = testd[,19],Predict = pred3))
importance(model3)
impor = importance(model3)
all = 0
for(i in 1:9){
  all = all + impor[i,9]
}
for(i in 1:9){
  impor[i,1] = impor[i,9]/all
}
##count accuracy
sum = 0
for(i in 1:49){
  if(result1[i,1] == result1[i,2]){
    sum = sum + result1[i,3]
  }
}
sum / nrow(testd)

write.csv(motion, file = "motion.csv")
