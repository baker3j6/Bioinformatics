#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Dehydration experiment data"),
   
   # Sidebar where users choose yvalue they would like to see 
   sidebarLayout(
      sidebarPanel(
         selectInput("yvalues",
                     "Y-axis choice:",
                    choices=c(
                      "Total weight lost"="total.loss",
                      "Total percent body weight lost"="total.percent",
                      "Average body weight lost per day"= "avg.dailey",
                      "Average percent body weight lost per day"= "avgpercentdaily",
                      "Number of days survived at 0% relative humidity"="days.survived"
                      
                    ))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("waterplot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) 
   
   output$waterplot <- renderPlot({
      # generate bins based on input$bins from ui.R
      if (input$yvalues=="total.loss"){
        y=waterstats$total.loss
        ylabs= "Total weight loss"
      }
     
     else if (input$yvalues == "total.percent") {
       y=waterstats$total.percent
       ylabs= "Total percent body weight lost"
     }
     
     else if (input$yvalues== "avg.daily") {
       y=waterstats$avg.daily
       ylabs= "Average body weight loss per day"}
     
     else if (input$yvalues== "avgdailypercent") {
       y=waterstats$avgdailypercent
       ylabs= "Average percent body weight loss per day"}
     
     else{
       y=waterstats$days.survived
       ylabs= "Days survived at 0% humidity"
     }
     
     #make ggplot object
     
      ggplot(waterstats,aes(factor(status),y))+
        geom_boxplot(fill="green")+
        stat_boxplot(geom="errorbar")

   })

# Run the application 
shinyApp(ui = ui, server = server)

