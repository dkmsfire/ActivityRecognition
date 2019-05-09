###we need to check data are normal and homogeneous
##normal
#first2
library(nortest)
load("first2.diff.rda")
for(i in 1:3){
  result = matrix(1:105, nrow = 15)
  colnames(result) = c("motion1", "motion2", "motion3", "motion4", "motion5", "motion6", "motion7")
  rownames(result) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
  for(j in 1:15){
    for(k in 1:7){
      adtest= ad.test(first2.diff[[i]][[j]][[k]])
      result[j,k] = adtest$p.value
    }
  }
  write.csv(result, file = paste0("First2.TestforNormal", i, ".csv"))
}
#last2
load("last2.diff.rda")
for(i in 1:3){
  result = matrix(1:105, nrow = 15)
  colnames(result) = c("motion1", "motion2", "motion3", "motion4", "motion5", "motion6", "motion7")
  rownames(result) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
  for(j in 1:15){
    for(k in 1:7){
      adtest= ad.test(last2.diff[[i]][[j]][[k]])
      result[j,k] = adtest$p.value
    }
  }
  write.csv(result, file = paste0("Last2.TestforNormal", i, ".csv"))
}

##Some datasets are normal, but some are not.

##Levene test
#first2
library(car)
load("first2.rda")
result = matrix(1:45, nrow = 15)
colnames(result) = c("X", "Y", "Z")
rownames(result) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
for(i in 1:3){
  for(j in 1:15){
    levenetest = leveneTest(first2[[i]][[j]][[1]], first2[[i]][[j]][[2]], center = mean)
    result[j,i] = levenetest[1,3]
  }
}
write.csv(result, file = "first2levene.csv")

#last2
load("last2.rda")
result = matrix(1:45, nrow = 15)
colnames(result) = c("X", "Y", "Z")
rownames(result) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
for(i in 1:3){
  for(j in 1:15){
    levenetest = leveneTest(last2[[i]][[j]][[1]], last2[[i]][[j]][[2]], center = mean)
    result[j,i] = levenetest[1,3]
  }
}
write.csv(result, file = "last2levene.csv")
#first10
load("first10.rda")
result = matrix(1:45, nrow = 15)
colnames(result) = c("X", "Y", "Z")
rownames(result) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
for(i in 1:3){
  for(j in 1:15){
    levenetest = leveneTest(first10[[i]][[j]][[1]], first10[[i]][[j]][[2]], center = mean)
    result[j,i] = levenetest[1,3]
  }
}
write.csv(result, file = "first2levene.csv")
##last10
load("last10.rda")
result = matrix(1:45, nrow = 15)
colnames(result) = c("X", "Y", "Z")
rownames(result) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
for(i in 1:3){
  for(j in 1:15){
    levenetest = leveneTest(last10[[i]][[j]][[1]], last10[[i]][[j]][[2]], center = mean)
    result[j,i] = levenetest[1,3]
  }
}
write.csv(result, file = "last10levene.csv")