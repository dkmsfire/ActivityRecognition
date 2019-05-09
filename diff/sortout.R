##input data
load("diff/rda/person.motion.diff.rda")
##library package
library(zoo)
point = 3
for(i in 1:7){
  for(ax in 2:4){
    diff = data.frame(diff.mean = double(), diff.sd = double())
    for(j in 1:15){
      data = person.motion.diff[[j]][[i]][[ax]]
      diff.mean = rollmean(data, k = point, fill = NA)
      diff.sd = rollapply(data , width = point, FUN = sd, na.pad = TRUE)
      diff.median = rollapply(data, width = point, FUN = median, na.pad = TRUE)
      diff.local = cbind(diff.mean, diff.sd, diff.median)
      diff = rbind(diff, diff.local)
    }
    if(ax == 2){
      Ax = "x"
    }else if(ax ==3){
      Ax = "y"
    }else{
      Ax = "z"
    }
    assign(paste0(Ax, ".diff"), diff)
  }
  assign(paste0("motion", i), cbind(x.diff, y.diff, z.diff))
}
motion.diff = list(motion1, motion2, motion3, motion4, motion5, motion6, motion7)
names(motion.diff) = c("motion1", "motion2", "motion3", "motion4", "motion5", "motion6", "motion7")
for(mo in 1:7){
  motion.diff[[mo]]$label = mo
}
motion.diff = rbind(motion.diff[[1]], motion.diff[[2]], motion.diff[[3]], motion.diff[[4]], motion.diff[[5]], motion.diff[[6]], motion.diff[[7]])
motion.diff = motion.diff[complete.cases(motion.diff),]
colnames(motion.diff) = c("x diff mean", "x diff sd", "x diff median", "y diff mean", "y diff sd", "y diff median",
                          "z diff mean", "z diff sd", "z diff median", "label")
save(motion.diff, file = "diff/model/motion.diff3.rda")

#origin
load("diff/rda/person.motion.all.rda")
point = 3
for(i in 1:7){
  for(ax in 2:4){
    origin = data.frame(origin.mean = double(), origin.sd = double())
    for(j in 1:15){
      data = person.motion.all[[j]][[i]][[ax]][2:nrow(person.motion.all[[j]][[i]])]
      origin.mean = rollmean(data, k = point, fill = NA)
      origin.sd = rollapply(data , width = point, FUN = sd, na.pad = TRUE)
      origin.median = rollapply(data, width = point, FUN = median, na.pad = TRUE)
      origin.local = cbind(origin.mean, origin.sd, origin.median)
      origin = rbind(origin, origin.local)
    }
    if(ax == 2){
      Ax = "x"
    }else if(ax ==3){
      Ax = "y"
    }else{
      Ax = "z"
    }
    assign(paste0(Ax, ".origin"), origin)
  }
  assign(paste0("motion", i), cbind(x.origin, y.origin, z.origin))
}
motion.origin = list(motion1, motion2, motion3, motion4, motion5, motion6, motion7)
names(motion.origin) = c("motion1", "motion2", "motion3", "motion4", "motion5", "motion6", "motion7")
for(mo in 1:7){
  motion.origin[[mo]]$label = mo
}
motion.origin = rbind(motion.origin[[1]], motion.origin[[2]], motion.origin[[3]], motion.origin[[4]], motion.origin[[5]], motion.origin[[6]], motion.origin[[7]])
motion.origin = motion.origin[complete.cases(motion.origin),]
colnames(motion.origin) = c("x mean", "x sd", "x median", "y mean", "y sd", "y median",
                            "z mean", "z  sd", "z median", "label")
save(motion.origin, file = "diff/model/motion.origin3.rda")
load("diff/model/motion.diff3.rda")
load("diff/model/motion.origin3.rda")

motion = cbind(motion.origin, motion.diff)
motion = motion[,c(1:9,11:20)]
save(motion, file = "motion3.rda")
for(i in 1:1917702){
  if(motion[i,10] != motion[i,20]){
    print("Warniing!")
  }
}

