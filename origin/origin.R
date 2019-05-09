##input data
load("person.motion.all.rda")
###x/y/z diff get the first 2 seconds each motion by one column and one column to record
for(i in 2:4){
  for(j in 1:15){
    person = person.motion.all[[j]]
    value = data.frame(value = integer())
    for(k in 1:7){
      num = data.frame(value = person[[k]][[i]][1:104])
      value = rbind(value, num)
    }
    motion = data.frame(motion = c(rep(1,104), rep(2,104), rep(3,104), rep(4,104), rep(5,104), rep(6,104), rep(7,104)))
    assign(paste0("person", j), cbind(value, motion))
  }
  if(i == 2){
    Ax = "x"
  }else if(i == 3){
    Ax = "y"
  }else{
    Ax = "z"
  }
  assign(paste0(Ax), list(person1, person2, person3, person4, person5, person6, person7, person8, person9, person10, person11, person12, person13, person14, person15))
  
}
names(x) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(y) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(z) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
first2 = list(x, y, z)
names(first2) = c("x", "y", "z")
save(first2, file = "first2.rda")

###x/y/z diff get the last 2 seconds each motion by one column and one column to record
for(i in 2:4){
  for(j in 1:15){
    person = person.motion.all[[j]]
    value = data.frame(value = integer())
    for(k in 1:7){
      num = data.frame(value = person[[k]][[i]][(nrow(person[[k]]-103)):nrow(person[[k]])])
      value = rbind(value, num)
    }
    motion = data.frame(motion = c(rep(1,104), rep(2,104), rep(3,104), rep(4,104), rep(5,104), rep(6,104), rep(7,104)))
    assign(paste0("person", j), cbind(value, motion))
  }
  if(i == 2){
    Ax = "x"
  }else if(i == 3){
    Ax = "y"
  }else{
    Ax = "z"
  }
  assign(paste0(Ax), list(person1, person2, person3, person4, person5, person6, person7, person8, person9, person10, person11, person12, person13, person14, person15))
  
}
names(x) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(y) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(z) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
last2 = list(x, y, z)
names(last2) = c("x", "y", "z")
save(last2, file = "last2.rda")

##test last 2 seconds / first 2 seconds
load("first2.rda")
result = matrix(1:45, nrow = 15)
colnames(result) = c("X", "Y", "Z")
rownames(result) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
for(i in 1:3){
  for(j in 1:15){
    data = first2[[i]][[j]]
    aov.data = aov(value ~ motion, data = data)
    pvalue = summary(aov.data)[[1]][["Pr(>F)"]][[1]]
    result[j, i] = pvalue
  }
}
write.csv(result, file = "first2Anova.csv")
##test last 2
load("last2.rda")
result = matrix(1:45, nrow = 15)
colnames(result) = c("X", "Y", "Z")
rownames(result) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
for(i in 1:3){
  for(j in 1:15){
    data = last2[[i]][[j]]
    aov.data = aov(value ~ motion, data = data)
    pvalue = summary(aov.data)[[1]][["Pr(>F)"]][[1]]
    result[j, i] = pvalue
  }
}
write.csv(result, file = "last2Anova.csv")

##function to plot density plot for first2.diff
load("first2.diff.rda")
load("last2.diff.rda")
first2density = function(axis, person, motion){
  plot(density(first2.diff[[axis]][[person]][[motion]]), main = paste("Density plot", "Axis", axis, "Person", person, "Motion", motion))
}
first2density(axis = 2,person = 3,motion = 6)

##function to plot density plot for last2.diff
last2density = function(axis, person, motion){
  plot(density(last2.diff[[axis]][[person]][[motion]]), main = paste("Density plot", "Axis", axis, "Person", person, "Motion", motion))
}
last2density(2, 3,2)


##ANOVA for a random range of observations
for(i in 2:4){
  for(j in 1:15){
    person = person.motion.all[[j]]
    value = data.frame(value = integer())
    for(k in 1:7){
      num = data.frame(value = person[[k]][[i]][50:153])
      value = rbind(value, num)
    }
    motion = data.frame(motion = c(rep(1,104), rep(2,104), rep(3,104), rep(4,104), rep(5,104), rep(6,104), rep(7,104)))
    assign(paste0("person", j), cbind(value, motion))
  }
  if(i == 2){
    Ax = "x"
  }else if(i == 3){
    Ax = "y"
  }else{
    Ax = "z"
  }
  assign(paste0(Ax), list(person1, person2, person3, person4, person5, person6, person7, person8, person9, person10, person11, person12, person13, person14, person15))
  
}
names(x) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(y) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(z) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
random2 = list(x, y, z)
names(random2) = c("x", "y", "z")
save(random2, file = "random2.rda")
##test
load("random2.rda")
result = matrix(1:45, nrow = 15)
colnames(result) = c("X", "Y", "Z")
rownames(result) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
for(i in 1:3){
  for(j in 1:15){
    data = random2[[i]][[j]]
    aov.data = aov(value ~ motion, data = data)
    pvalue = summary(aov.data)[[1]][["Pr(>F)"]][[1]]
    result[j, i] = pvalue
  }
}
write.csv(result, file = "random2Anova.csv")
