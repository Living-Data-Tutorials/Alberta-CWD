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
                        titlePanel("Modeling the spread of Chronic Wasting Disease in mule deer"),
                        p("In this tutorial, we're going to use a simple compartmental model to track the spread of Chronic Wasting Disease (CWD) in an Albertan mule deer over time. We’ll then use the same model to look at how vaccination could be used to drive CWD to extinction in our mule deer population."),
                        #Two columns
                        fluidRow(
                            column(width=12,
                                   withMathJax(),
                                   h1("What is CWD?"),
                                   p("CWD is a prion disease that affects deer, elk, reindeer, and moose. It has a long incubation period (typically 18-24 months), and symptoms include excessive salivation, increased drinking and urination, weight loss, confusion, and tremors (Fig. 1)."),
                                   tags$img(src="CWDMuleDeer.jpg",width="300px"),
                                   p("There is no recovery and no treatment, so eventual death is certain (mean time from oral infection to death is ~23 months). CWD is increasing exponentially in Albertan mule deer (Fig. 2); if we can predict the spread of CWD in the future, we can decide how concerned we should be about this disease taking over the population."),
                                   tags$img(src="CWDPrevalence.jpg",width="300px"),
                                   #this is a horizontal line break
                                   hr(),
                                   h1("How can we model the spread of CWD?"),
                                   helpText("We will use a basic compartmental model, known as an SI model (Fig 3). Assume that the mule deer population consists of \\(N\\) individuals that are split into two groups: \\(S\\) (susceptible) and \\(I\\) (infected, with CWD). The population is at equilibrium, such that the birth and death rate are exactly equal. The transmission rate, \\(\\beta\\), determines how quickly the disease spreads from infected to susceptible individuals."),
                                   tags$img(src="BasicModel.jpg",width="300px"),
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
