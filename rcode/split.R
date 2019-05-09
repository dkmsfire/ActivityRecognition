#input data
for(i in 1:15){
  data = read.csv(paste0(i,".csv"))
  colnames(data) = c("time", "x", "y", "z", "label")
  assign(paste0("person",i), data)
}#merge 15 person data to person 
person = list(person1, person2, person3, person4, person5, person6, person7, person8, person9, person10, person11, person12, person13, person14, person15)
names(person) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")

#split data to all / person / motion
for(i in 1:15){
  person.data = person[[i]]
  for(j in 1:7){
    motion = person.data[which(person.data$label == j),]
    assign(paste0("motion", j), motion)
  }
  person.motion = list(motion1, motion2, motion3, motion4, motion5, motion6, motion7)
  names(person.motion) = c("motion1", "motion2", "motion3", "motion4", "motion5", "motion6", "motion7")
  assign(paste0("person", i), person.motion)
}
person.motion.all = list(person1, person2, person3, person4, person5, person6, person7, person8, person9, person10, person11, person12, person13, person14, person15)
names(person.motion.all) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
save(person.motion.all, file = "person.motion.all.rda")
#split data to all / person / motion / diff
for(i in 1:15){
  person.data = person[[i]]
  for(j in 1:7){
    motion = person.data[which(person.data$label == j),]
    time = motion[1:(nrow(motion)-1),1]
    x.diff = diff(motion$x)
    y.diff = diff(motion$y)
    z.diff = diff(motion$z)
    label = motion[1:(nrow(motion)-1),5]
    motion.diff = data.frame(cbind(time, x.diff, y.diff, z.diff, label))
    assign(paste0("motion", j, ".diff"), motion.diff)
  }
  person.motion = list(motion1.diff, motion2.diff, motion3.diff, motion4.diff, motion5.diff, motion6.diff, motion7.diff)
  names(person.motion) = c("motion1", "motion2", "motion3", "motion4", "motion5", "motion6", "motion7")
  assign(paste0("person", i), person.motion)
}
person.motion.diff = list(person1, person2, person3, person4, person5, person6, person7, person8, person9, person10, person11, person12, person13, person14, person15)
names(person.motion.diff) = c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15")
save(person.motion.diff, file = "person.motion.diff.rda")
#plot some basic plot to see
library(ggplot2)
plot.person.motion.diff.xyz = function(i, j){
  ggplot(person.motion.diff[[i]][[j]], aes(time)) +
    geom_line(aes(y = x.diff, colour = "x.diff")) +
    geom_line(aes(y = y.diff, colour = "y.diff")) +
    geom_line(aes(y = z.diff, colour = "z.diff"))
}
plot.person.motion.diff.xyz(1,1)

#basic 3D scatter plot
library(plotly)
#api to cennect ## must run this every time starting R
Sys.setenv("plotly_username" = "dkmsfire")
Sys.setenv("plotly_api_key" = "GZZQ64OewOxnK3Vgj3Cu")
plot.person.motion.diff.3d = function(i, j){
  p = plot_ly(person.motion.diff[[i]][[j]], x = x.diff, y = y.diff, z = z.diff) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = "x.diff"),
                        yaxis = list(title = "y.diff"),
                        zaxis = list(title = "z.diff")))
  chart_link = api_create(p, filename = "scatter3d-basic")
  chart_link
}
plot.person.motion.diff.3d(5,2)
##in my opinion , I thick it look like the same

#try gg3D to plot 3D
library(gg3D)
ggplot(person.motion.diff$person1$motion7, aes(x = x.diff, y = y.diff, z = z.diff, color = "red")) + 
  theme_void() +
  axes_3D() +
  stat_3D()
##I can't move the 3D plot, also the points are too many.

#rgl plot3d
library(rgl)
load("person.motion.diff.rda")
open3d()
x = person.motion.diff$person1$motion1$x.diff
y = person.motion.diff$person1$motion1$y.diff
z = person.motion.diff$person1$motion1$z.diff
plot3d(x, y, z, size = 10)

plo3d = function(i,j){
  x = person.motion.diff[[i]][[j]]$x.diff
  y = person.motion.diff[[i]][[j]]$y.diff
  z = person.motion.diff[[i]][[j]]$z.diff
  plot3d(x, y, z, size = 10, colors = rainbow(1000))
}
plo3d(1,7)
#
library(plotly)
p = plot_ly(person.motion.diff$person1$motion7, x = x.diff, y = y.diff, z = z.diff, type = "scatter3d", mode = "lines",
            opacity = 1, line = list(width = 3, reverscale = FALSE))
chart_link = api_create(p, filename = "line3d-diff")
chart_link

p = plot_ly(person.motion.all$person1$motion6, x = x, y = y, z = z, type = "scatter3d", mode = "lines",
            opacity = 1, line = list(width = 3, reverscale = FALSE))
chart_link1 = api_create(p, filename = "line3d-person1-motion6")
chart_link1

##hierarchical clustering by sexaul (split two clusters)
