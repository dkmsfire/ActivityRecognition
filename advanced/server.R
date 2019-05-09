library(shiny)
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
    plot(person.motion.all[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], type = "l", xlab = "Time", ylab = input$Motion, main = paste0(input$Person, "'s ", input$Axis))
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
    plot(person.motion.diff[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], type = "l", xlab = "Time", ylab = input$Motion, main = paste0(input$Person, "'s ", input$Axis, ".diff"))
  })
  output$omovage = renderPlot({
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
    plot(person.motion.all[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], type = "l", xlab = "Time", ylab = input$Motion, main = paste0(input$Person, "'s ", input$Axis, ". moving average"))
    lines(rollmean(person.motion.all[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], k = as.numeric(input$Num), fill = NA), col = "red")
  })
  output$omovsd = renderPlot({
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
    plot(person.motion.all[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], type = "l", xlab = "Time", ylab = input$Motion, main = paste0(input$Person, "'s ", input$Axis, ". moving sd"))
    lines(rollapply(person.motion.all[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], width = as.numeric(input$Num), FUN = sd, na.pad = TRUE), col = "red")
  })
  output$omovmedian = renderPlot({
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
    plot(person.motion.all[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], type = "l", xlab = "Time", ylab = input$Motion, main = paste0(input$Person, "'s ", input$Axis, ". moving median"))
    lines(rollapply(person.motion.all[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], width = as.numeric(input$Num), FUN = median, na.pad = TRUE), col = "red")
  })
  output$movage = renderPlot({
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
    plot(person.motion.diff[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], type = "l", xlab = "Time", ylab = input$Motion, main = paste0(input$Person, "'s ", input$Axis, ".diff moving average"))
    lines(rollmean(person.motion.diff[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], k = as.numeric(input$Num), fill = NA), col = "red")
  })
  output$movsd = renderPlot({
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
    plot(person.motion.diff[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], type = "l", xlab = "Time", ylab = input$Motion, main = paste0(input$Person, "'s ", input$Axis, ".diff moving sd"))
    lines(rollapply(person.motion.diff[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], width = as.numeric(input$Num), FUN = sd, na.pad = TRUE), col = "red")
  })
  output$movmedian = renderPlot({
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
    plot(person.motion.diff[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], type = "l", xlab = "Time", ylab = input$Motion, main = paste0(input$Person, "'s ", input$Axis, ".diff moving median"))
    lines(rollapply(person.motion.diff[[numofperson]][[numofmotion]][[xyzaxis]][input$Range[[1]]:input$Range[[2]]], width = as.numeric(input$Num), FUN = median, na.pad = TRUE), col = "red")
  })
}
