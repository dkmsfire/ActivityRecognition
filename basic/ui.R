ui = fluidPage(
  #App title
  titlePanel("Activity Recognition"),
  
  #Sidebar layout
  sidebarLayout(
    #Input
    sidebarPanel(
      selectInput("Person", "person", c("person1", "person2", "person3", "person4", "person5", "person6", "person7", "person8", "person9", "person10", "person11", "person12", "person13", "person14", "person15"), multiple = F),
      
      br(),
      
      selectInput("Motion", "motion", c("motion1", "motion2", "motion3", "motion4", "motion5", "motion6", "motion7"), multiple = F),
      
      br(),
      
      selectInput("Axis", "axis", c("x", "y", "z"), multiple = F),
      
      br(),
      
      textInput("Num", "Number for moving average and standard deviation", value = 26),
      helpText("Note: Enter a number for scanning"),
      br(),
      
      
      sliderInput("Range", "range", min = 0, max = 100000, value = c(0, 4000))
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Origin", plotOutput("plot"), rglwidgetOutput("3dplot")),
                  tabPanel("Diff", plotOutput("diff"), rglwidgetOutput("3ddiff")),
                  tabPanel("Diff Mean", plotOutput("diffmean"), rglwidgetOutput("3ddiffmean")),
                  tabPanel("Diff SD", plotOutput("diffsd"), rglwidgetOutput("3ddiffsd"))
      )
    )
  )
)
