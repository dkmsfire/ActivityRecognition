library(shiny)
library(rgl)
library(zoo)
server = function(input, output){
  load("person.motion.all.rda")
  load("person.motion.diff.rda")
  output$plot = renderPlot({
    numofperson = switch(input$Person,
                         person1 = 1,
                         person2 = 2,
                         person3 = 3,
                         person4 = 4,
                         person5 = 5,
                         person6 = 6,
                         person7 = 7,
                         person8 = 8,
                         person9 = 9,
                         person10 = 10,
                         person11 = 11,
                         person12 = 12,
                         person13 = 13,
                         person14 = 14,
                         person15 = 15)
    numofmotion = switch(input$Motion,
                         motion1 = 1,
                         motion2 = 2,
                         motion3 = 3,
                         motion4 = 4,
                         motion5 = 5,
                         motion6 = 6,
                         motion7 = 7)
    xyzaxis = switch(input$Axis,
                     x = 2,
                     y = 3,
                     z = 4)
    plot(person.motion.all[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[1]:input$Range[2]], xlab = input$Axis, ylab = input$Motion, main = input$Person)
  })
  output$`3dplot` = renderRglwidget({
    numofperson = switch(input$Person,
                         person1 = 1,
                         person2 = 2,
                         person3 = 3,
                         person4 = 4,
                         person5 = 5,
                         person6 = 6,
                         person7 = 7,
                         person8 = 8,
                         person9 = 9,
                         person10 = 10,
                         person11 = 11,
                         person12 = 12,
                         person13 = 13,
                         person14 = 14,
                         person15 = 15)
    numofmotion = switch(input$Motion,
                         motion1 = 1,
                         motion2 = 2,
                         motion3 = 3,
                         motion4 = 4,
                         motion5 = 5,
                         motion6 = 6,
                         motion7 = 7)
    open3d(useNULL = TRUE)
    plot3d(x = person.motion.all[[numofperson]][[numofmotion]][[2]][input$Range[1]:input$Range[2]], y = person.motion.all[[numofperson]][[numofmotion]][[3]][input$Range[1]:input$Range[2]], z = person.motion.all[[numofperson]][[numofmotion]][[4]][input$Range[1]:input$Range[2]], xlab = "x", ylab = "y", zlab = "z")
    rglwidget()
  })
  output$diff = renderPlot({
    numofperson = switch(input$Person,
                         person1 = 1,
                         person2 = 2,
                         person3 = 3,
                         person4 = 4,
                         person5 = 5,
                         person6 = 6,
                         person7 = 7,
                         person8 = 8,
                         person9 = 9,
                         person10 = 10,
                         person11 = 11,
                         person12 = 12,
                         person13 = 13,
                         person14 = 14,
                         person15 = 15)
    numofmotion = switch(input$Motion,
                         motion1 = 1,
                         motion2 = 2,
                         motion3 = 3,
                         motion4 = 4,
                         motion5 = 5,
                         motion6 = 6,
                         motion7 = 7)
    xyzaxis = switch(input$Axis,
                     x = 2,
                     y = 3,
                     z = 4)
    plot(person.motion.diff[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[1]:input$Range[2]], xlab = input$Axis, ylab = input$Motion, main = input$Person)
  })
  output$`3ddiff` = renderRglwidget({
    numofperson = switch(input$Person,
                         person1 = 1,
                         person2 = 2,
                         person3 = 3,
                         person4 = 4,
                         person5 = 5,
                         person6 = 6,
                         person7 = 7,
                         person8 = 8,
                         person9 = 9,
                         person10 = 10,
                         person11 = 11,
                         person12 = 12,
                         person13 = 13,
                         person14 = 14,
                         person15 = 15)
    numofmotion = switch(input$Motion,
                         motion1 = 1,
                         motion2 = 2,
                         motion3 = 3,
                         motion4 = 4,
                         motion5 = 5,
                         motion6 = 6,
                         motion7 = 7)
    open3d(useNULL = TRUE)
    plot3d(x = person.motion.diff[[numofperson]][[numofmotion]][[2]][input$Range[1]:input$Range[2]], y = person.motion.diff[[numofperson]][[numofmotion]][[3]][input$Range[1]:input$Range[2]], z = person.motion.diff[[numofperson]][[numofmotion]][[4]][input$Range[1]:input$Range[2]], xlab = "x", ylab = "y", zlab = "z")
    rglwidget()
  })
  output$diffmean = renderPlot({
    numofperson = switch(input$Person,
                         person1 = 1,
                         person2 = 2,
                         person3 = 3,
                         person4 = 4,
                         person5 = 5,
                         person6 = 6,
                         person7 = 7,
                         person8 = 8,
                         person9 = 9,
                         person10 = 10,
                         person11 = 11,
                         person12 = 12,
                         person13 = 13,
                         person14 = 14,
                         person15 = 15)
    numofmotion = switch(input$Motion,
                         motion1 = 1,
                         motion2 = 2,
                         motion3 = 3,
                         motion4 = 4,
                         motion5 = 5,
                         motion6 = 6,
                         motion7 = 7)
    xyzaxis = switch(input$Axis,
                     x = 2,
                     y = 3,
                     z = 4)
    plot(rollmean(person.motion.diff[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], k = as.numeric(input$Num), fill = NA), type = "l", xlab = "Time", ylab = input$Axis, main = "Moving Average")
  })
  output$`3ddiffmean` = renderRglwidget({
    numofperson = switch(input$Person,
                         person1 = 1,
                         person2 = 2,
                         person3 = 3,
                         person4 = 4,
                         person5 = 5,
                         person6 = 6,
                         person7 = 7,
                         person8 = 8,
                         person9 = 9,
                         person10 = 10,
                         person11 = 11,
                         person12 = 12,
                         person13 = 13,
                         person14 = 14,
                         person15 = 15)
    numofmotion = switch(input$Motion,
                         motion1 = 1,
                         motion2 = 2,
                         motion3 = 3,
                         motion4 = 4,
                         motion5 = 5,
                         motion6 = 6,
                         motion7 = 7)
    xyzaxis = switch(input$Axis,
                     x = 2,
                     y = 3,
                     z = 4)
    open3d(useNULL = TRUE)
    plot3d(x = rollmean(person.motion.diff[[numofperson]][[numofmotion]][[2]][input$Range[1]:input$Range[2]], k = as.numeric(input$Num), fill = NA), y = rollmean(person.motion.diff[[numofperson]][[numofmotion]][[3]][input$Range[1]:input$Range[2]], k = as.numeric(input$Num), fill = NA), z = rollmean(person.motion.diff[[numofperson]][[numofmotion]][[4]][input$Range[1]:input$Range[2]], k = as.numeric(input$Num), fill = NA), xlab = "x", ylab = "y", zlab = "z", main = "Moving Average for 3D")
    rglwidget()
  })
  output$diffsd = renderPlot({
    numofperson = switch(input$Person,
                         person1 = 1,
                         person2 = 2,
                         person3 = 3,
                         person4 = 4,
                         person5 = 5,
                         person6 = 6,
                         person7 = 7,
                         person8 = 8,
                         person9 = 9,
                         person10 = 10,
                         person11 = 11,
                         person12 = 12,
                         person13 = 13,
                         person14 = 14,
                         person15 = 15)
    numofmotion = switch(input$Motion,
                         motion1 = 1,
                         motion2 = 2,
                         motion3 = 3,
                         motion4 = 4,
                         motion5 = 5,
                         motion6 = 6,
                         motion7 = 7)
    xyzaxis = switch(input$Axis,
                     x = 2,
                     y = 3,
                     z = 4)
    plot(rollapply(person.motion.diff[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], width = as.numeric(input$Num), FUN = sd, na.pad = TRUE), type = "l", xlab = "Time", ylab = input$Axis, main = "Moving Standard Deviation")
  })
  output$`3ddiffsd` = renderRglwidget({
    numofperson = switch(input$Person,
                         person1 = 1,
                         person2 = 2,
                         person3 = 3,
                         person4 = 4,
                         person5 = 5,
                         person6 = 6,
                         person7 = 7,
                         person8 = 8,
                         person9 = 9,
                         person10 = 10,
                         person11 = 11,
                         person12 = 12,
                         person13 = 13,
                         person14 = 14,
                         person15 = 15)
    numofmotion = switch(input$Motion,
                         motion1 = 1,
                         motion2 = 2,
                         motion3 = 3,
                         motion4 = 4,
                         motion5 = 5,
                         motion6 = 6,
                         motion7 = 7)
    xyzaxis = switch(input$Axis,
                     x = 2,
                     y = 3,
                     z = 4)
    open3d(useNULL = TRUE)
    plot3d(x = rollapply(person.motion.diff[[numofperson]][[numofmotion]][[2]][input$Range[1]:input$Range[2]], width = as.numeric(input$Num), FUN = sd, na.pad = TRUE), y = rollapply(person.motion.diff[[numofperson]][[numofmotion]][[3]][input$Range[1]:input$Range[2]], width = as.numeric(input$Num), FUN = sd, na.pad = TRUE), z = rollapply(person.motion.diff[[numofperson]][[numofmotion]][[4]][input$Range[1]:input$Range[2]], width = as.numeric(input$Num), FUN = sd, na.pad = TRUE), xlab = "x", ylab = "y", zlab = "z", main = "Moving Standard Deviation for 3D")
    rglwidget()
  })
}
