#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
source("R/prediction.R")
data <- read.csv("Data/wasting_disease.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage("",id="mainpage",
               tabPanel("Page 1",value="page1",
                        # Application title
                        titlePanel("Shiney Test"),
                        #Two columns
                        fluidRow(
                            column(width=8,
                                   h1("h1 heading hello"),
                                   h3("h3 heading"),
                                   p("This is a paragraph"),
                                   #this is a horizontal line break
                                   hr(),
                                   
                                   p(tags$strong("This is bolded test")),
                                   p(tags$ul(
                                       tags$li("This is the first element in a list"),
                                       tags$li("This is the second element in a list")
                                   ))
                            ),
                            column(width=4,
                                   #this is how you include an image
                                   tags$img(src="test.jpg",width="300px"),
                            )
                        )
               ),
               tabPanel("Page2",
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("prop_vacc","Proportion vaccinated",0,1,0,step=0.01),
                                p("the slider above controls beta, which is the transmission parameter")
                            ),
                            mainPanel(
                                plotOutput("i_plot")
                            )
                        ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$i_plot <-renderPlot({
        predictionYears<-2019:2050
        predictionPrevalence<-predict_i(predictionYears,parms=c(beta=1.398834*(1-input$prop_vacc),
                                                                gamma=1),
                                        xinit = c(S=1-max(data$Prevalence),I=max(data$Prevalence)))
        
        predictionData<-data.frame(Year=predictionYears,Prevalence=predictionPrevalence)
        
        ggplot(predictionData,aes(x=Year,y=Prevalence))+geom_line(color="blue")+
            geom_line(data=data,aes(x=Year,y=Prevalence))+theme_bw()
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
