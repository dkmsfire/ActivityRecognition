### I want to use decision tree to find that the relationship about xyz and motion.
##input
for(i in 1:7){
  assign(paste0("motion", i), read.csv(paste0("motion", i, ".csv")))
}

motion = rbind(motion1, motion2, motion3, motion4, motion5, motion6, motion7)
motion = motion[,-1]
motion$label = as.factor(motion$label)
save(motion, file = "motion.rda")

##train the tree
load("motion.rda")
library(rpart)
train.index <- sample(x=1:nrow(motion), size=ceiling(0.8*nrow(motion) ))
train <- motion[train.index, ]
test <- motion[-train.index, ]
cart.model<- rpart(label ~. , data=train, method = "class")
cart.model
##plot the tree
library(rpart.plot) 
prp(cart.model,
    faclen=0,
    fallen.leaves=TRUE, shadow.col="gray", extra=2)  
