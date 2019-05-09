##split each motion the last 2 seconds by the same person using diff
load("person.motion.diff.rda")
plot(density(person.motion.diff$person1$motion1$z.diff))
##x, y, z diff get the last 2 seconds (104 points)
for(ax in 2:4){
  if(ax == 2){
    Ax = "x"
  }else if(ax == 3){
    Ax = "y"
  }else{
    Ax = "z"
  }
  for(per in 1:15){
    person = person.motion.diff[[per]]
    for(i in 1:7){
      assign(names(person)[[i]], person[[i]][[ax]][(nrow(person[[i]]) - 103):nrow(person[[i]])])
    }
    assign(paste0("person", per), data.frame(cbind(motion1, motion2, motion3, motion4, motion5, motion6, motion7)))
  }
  assign(paste0("last2.", Ax, "diff"), list(person1, person2, person3, person4, person5, person6, person7, person8, person9, person10, person11, person12, person13, person14, person15))
}
names(last2.xdiff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(last2.ydiff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(last2.zdiff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
last2.diff = list(last2.xdiff, last2.ydiff, last2.zdiff)
names(last2.diff) = c("xdiff" ,"ydiff", "zdiff")
save(last2.diff, file = "last2.diff.rda")

###x/y/z diff get the first 2 seconds each motion by one column and one column to record
for(i in 2:4){
  for(j in 1:15){
    person = person.motion.diff[[j]]
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
  assign(paste0(Ax,".diff"), list(person1, person2, person3, person4, person5, person6, person7, person8, person9, person10, person11, person12, person13, person14, person15))
  
}
names(x.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(y.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(z.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
first2 = list(x.diff, y.diff, z.diff)
names(first2) = c("x.diff", "y.diff", "z.diff")
save(first2, file = "first2.rda")

###x/y/z diff get the last 2 seconds each motion by one column and one column to record
for(i in 2:4){
  for(j in 1:15){
    person = person.motion.diff[[j]]
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
  assign(paste0(Ax,".diff"), list(person1, person2, person3, person4, person5, person6, person7, person8, person9, person10, person11, person12, person13, person14, person15))
  
}
names(x.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(y.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(z.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
last2 = list(x.diff, y.diff, z.diff)
names(last2) = c("x.diff", "y.diff", "z.diff")
save(last2, file = "last2.rda")

##x, y, z diff get the first 2 seconds (104 points)
for(ax in 2:4){
  if(ax == 2){
    Ax = "x"
  }else if(ax == 3){
    Ax = "y"
  }else{
    Ax = "z"
  }
  for(per in 1:15){
    person = person.motion.diff[[per]]
    for(i in 1:7){
      assign(names(person)[[i]], person[[i]][[ax]][1:104])
    }
    assign(paste0("person", per), data.frame(cbind(motion1, motion2, motion3, motion4, motion5, motion6, motion7)))
  }
  assign(paste0("first2.", Ax, "diff"), list(person1, person2, person3, person4, person6, person6, person7, person8, person9, person10, person11, person12, person13, person14, person15))
}
names(first2.xdiff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(first2.ydiff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(first2.zdiff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
first2.diff = list(first2.xdiff, first2.ydiff, first2.zdiff)
names(first2.diff) = c("xdiff" ,"ydiff", "zdiff")
save(first2.diff, file = "first2.diff.rda")
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
    data = first2[[i]][[j]]
    aov.data = aov(value ~ motion, data = data)
    pvalue = summary(aov.data)[[1]][["Pr(>F)"]][[1]]
    result[j, i] = pvalue
  }
}
write.csv(result, file = "last2Anova.csv")
 ###result is all the person's motion's first 2 seconds are the same
###result is all the person's motion's first 2 seconds are the same
###but maybe it is all the same at any points.
###need to do last 2 seconds and random select 2 seconds.


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
    person = person.motion.diff[[j]]
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
  assign(paste0(Ax,".diff"), list(person1, person2, person3, person4, person5, person6, person7, person8, person9, person10, person11, person12, person13, person14, person15))
  
}
names(x.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(y.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(z.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
random2 = list(x.diff, y.diff, z.diff)
names(random2) = c("x.diff", "y.diff", "z.diff")
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

###pairwise t-test for first/last 2 seconds
load("first2.rda")
for(i in 1:3){
  result = matrix(1:315, ncol = 15)
  colnames(result) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
  rownames(result) = c("1vs2","1vs3","1vs4","1vs5","1vs6","1vs7","2vs3","2vs4","2vs5","2vs6","2vs7",
                       "3vs4","3vs5","3vs6","3vs7","4vs5","4vs6","4vs7","5vs6","5vs7","6vs7")
  for (j in 1:15) {
    ptest = pairwise.t.test(x = first2[[i]][[j]]$value, g = first2[[i]][[j]]$motion, p.adjust.method = "bonferroni")
    pvalue = c()
    for (c in 1:6) {
      for(r in c:6){
       p = ptest$p.value[r,c]
       pvalue = c(pvalue , p)
      }
    }
    for(k in 1:21){
      result[k,j] = pvalue[k]
    }
  }
  write.csv(result, file = paste0("axis", i, " first2pairwise.csv"))
}
pair = pairwise.t.test(x = last2$x.diff$person1$value, g = last2$x.diff$person1$motion, p.adjust.method = "bonferroni")

###test first/last 10 seconds
##last 10 data
load("person.motion.diff.rda")
for(i in 2:4){
  for(j in 1:15){
    person = person.motion.diff[[j]]
    value = data.frame(value = integer())
    for(k in 1:7){
      num = data.frame(value = person[[k]][[i]][(nrow(person[[k]]-519)):nrow(person[[k]])])
      value = rbind(value, num)
    }
    motion = data.frame(motion = c(rep(1,520), rep(2,520), rep(3,520), rep(4,520), rep(5,520), rep(6,520), rep(7,520)))
    assign(paste0("person", j), cbind(value, motion))
  }
  if(i == 2){
    Ax = "x"
  }else if(i == 3){
    Ax = "y"
  }else{
    Ax = "z"
  }
  assign(paste0(Ax,".diff"), list(person1, person2, person3, person4, person5, person6, person7, person8, person9, person10, person11, person12, person13, person14, person15))
  
}
names(x.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(y.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(z.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
last10 = list(x.diff, y.diff, z.diff)
names(last10) = c("x.diff", "y.diff", "z.diff")
save(last10, file = "last10.rda")

##first10 data
for(i in 2:4){
  for(j in 1:15){
    person = person.motion.diff[[j]]
    value = data.frame(value = integer())
    for(k in 1:7){
      num = data.frame(value = person[[k]][[i]][1:104])
      value = rbind(value, num)
    }
    motion = data.frame(motion = c(rep(1,520), rep(2,520), rep(3,520), rep(4,520), rep(5,520), rep(6,520), rep(7,520)))
    assign(paste0("person", j), cbind(value, motion))
  }
  if(i == 2){
    Ax = "x"
  }else if(i == 3){
    Ax = "y"
  }else{
    Ax = "z"
  }
  assign(paste0(Ax,".diff"), list(person1, person2, person3, person4, person5, person6, person7, person8, person9, person10, person11, person12, person13, person14, person15))
  
}
names(x.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(y.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
names(z.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
first10 = list(x.diff, y.diff, z.diff)
names(first10) = c("x.diff", "y.diff", "z.diff")
save(first10, file = "first10.rda")
###test first 10 
load("first10.rda")
load("last10.rda")
result = matrix(1:45, nrow = 15)
colnames(result) = c("X", "Y", "Z")
rownames(result) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
for(i in 1:3){
  for(j in 1:15){
    data = first10[[i]][[j]]
    aov.data = aov(value ~ motion, data = data)
    pvalue = summary(aov.data)[[1]][["Pr(>F)"]][[1]]
    result[j, i] = pvalue
  }
}
write.csv(result, file = "first10Anova.csv")
##test last 10
result = matrix(1:45, nrow = 15)
colnames(result) = c("X", "Y", "Z")
rownames(result) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
for(i in 1:3){
  for(j in 1:15){
    data = last10[[i]][[j]]
    aov.data = aov(value ~ motion, data = data)
    pvalue = summary(aov.data)[[1]][["Pr(>F)"]][[1]]
    result[j, i] = pvalue
  }
}
write.csv(result, file = "last10Anova.csv")
##diff correlation
load("person.motion.all.rda")
load("person.motion.diff.rda")
cor(person.motion.diff$person1$motion1[2:4])
correlation = function(type, person, motion){
  if(type == "all"){
    print(cor(person.motion.all[[person]][[motion]][2:4]))
  }else if(type == "diff"){
    print(cor(person.motion.diff[[person]][[motion]][2:4]))
  }
}


##correlation matrix and heatmap
cor = correlation(type = "all", 1, 2)
col = colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cor, col = col, symm = TRUE)
##heatmap for only 3 variables are very bad

##