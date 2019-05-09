##record what I want to do in this shiny app
##using person.motion.diff 
##1. moving average by a parameter that can change
##2. standard deviation by a parameter that can change
##packages : shiny -- ggplot2 -- plotly 
##using ggploty to combine ggplot2 and plotly.

shinyApp(ui = ui, server = server)

#publish
library(rsconnect)
deployApp("C:/Users/User/Documents/ActivityRecognition/advanced")

##maybe memory explosion
## as a free user, we can only have the biggest size equals to 1024 MB, that is 
rsconnect::configureApp("personmotionxyz", size="xlarge")

###example
runExample("02_text")
runExample("03_reactivity")
runExample("04_mpg")
runExample("05_sliders") ## range
runExample("06_tabsets")
runExample("07_widgets")
runExample("08_html")
runExample("09_upload")
runExample("10_download")
runExample("11_timer")
###
rollmean(person.motion.diff$person1$motion1$x.diff, k = 26, fill = NA)
plot(rollmean(person.motion.diff$person1$motion1$x.diff[1:1000], k = 26, fill = NA), type = "l")


plot(person.motion.all[[numofperson]][[numofmotion]][[xyzaxis]][1:4000], type = "l")
lines(rollapply(person.motion.all[[numofperson]][[numofmotion]][[xyzaxis]][1:4000], width = 26, FUN = sd, na.pad = TRUE), col = "red")

plot(rollapply(person.motion.all[[numofperson]][[numofmotion]][[xyzaxis]][1:4000], width = 26, FUN = sd, na.pad = TRUE), col = "red")
lines(rollapply(person.motion.all[[numofperson]][[numofmotion]][[xyzaxis]][1:4000], width = 26, FUN = sd, na.pad = TRUE), col = "red")
