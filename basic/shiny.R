shinyApp(ui = ui, server = server)

library(rsconnect)
deployApp("C:/Users/User/Documents/ActivityRecognition/basic")

##maybe memory explosion
rsconnect::configureApp("personmotionxyz", size="xlarge")

###this app is for plot the origin and diff plot
###moreover, we can display 3dplot to see the xyz axis 
###but the points are too much, only can see the position/center is change a little