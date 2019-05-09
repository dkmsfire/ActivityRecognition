##hierarchical cluster
data = person1$motion1[2:4]
E.dist <- dist(data, method="euclidean")
h.E.cluster <- hclust(E.dist)
##memory explosion.