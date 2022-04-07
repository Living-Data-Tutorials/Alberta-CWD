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
data <- read.csv("Data/wasting_disease.csv")
 
source("R/prediction.R")
source("R/base_graph.R")


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
                                   tags$img(src="CWDPrevalence.jpeg",width="300px"),
                                   #this is a horizontal line break
                                   hr(),
                                   h1("How can we model the spread of CWD?"),
                                   helpText("We will use a basic compartmental model, known as an SI model (Fig 3). Assume that the mule deer population consists of \\(N\\) individuals that are split into two groups: \\(S\\) (susceptible) and \\(I\\) (infected, with CWD). The population is at equilibrium, such that the birth and death rate are exactly equal. The transmission rate, \\(\\beta\\), determines how quickly the disease spreads from infected to susceptible individuals."),
                                   tags$img(src="BasicModel.jpg",width="300px")
                            )
                        )
               ),
               tabPanel("Transmission",
                        titlePanel("The beta parameter for the model"),
                        p("Here is a description of what we're doing here, which is letting students examine what the beta parameter does!"),
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("beta_parameter","Beta parameter",0,3,0,step=0.001),
                                p("the slider above controls beta, which is the transmission parameter")
                            ),
                            mainPanel(
                                plotOutput("beta_plot")
                            )
                        ),
                        hr(),
                        p("Maybe here is more descriptions?")),
               tabPanel("Death",
                        titlePanel("The gamma parameter"),
                        p("Here is a description of what we're doing here, which is letting students examine what the gamma parameter does!"),
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("gamma_beta_parameter","Beta parameter",min=0,max=2,step=0.001,value=0),
                                p("the slider above controls beta, which is the transmission parameter"),
                                hr(),
                                sliderInput("gamma_gamma_parameter","Gamma parameter",min=0,max=3,step=0.001,value=0),
                                p("the slider above controls gamma, which is the death parameter")
                            ),
                            mainPanel(
                                h2("Infected"),
                                plotOutput("gamma_plot_i"),
                                hr(),
                                h2("Susceptible"),
                                plotOutput("gamma_plot_s")
                            )
                        ),
                        p("Maybe here is more descriptions?"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$beta_plot <-renderPlot({
        xinit=c(S=1-0.0022,I=0.0022)
        predict_years=seq(2006,2050,by=0.25)
        predict_prevalence <- predict_i(predict_years,c(beta=input$beta_parameter,gamma=0),xinit)
        predict_df <- data.frame(Year=predict_years,Prevalence=predict_prevalence)
        base_graph(data)+geom_line(data=predict_df,aes(x=Year,y=Prevalence),size=1,color="red")+ylim(0,1)
    })
    output$gamma_plot_i <-renderPlot({
        xinit=c(S=1-0.0022,I=0.0022)
        predict_years=seq(2006,2050,by=0.25)
        predict_prevalence_i <- predict_i(predict_years,c(beta=input$gamma_beta_parameter,
                                                        gamma=input$gamma_gamma_parameter),xinit)
        predict_prevalence_s <- predict_s(predict_years,c(beta=input$gamma_beta_parameter,
                                                          gamma=input$gamma_gamma_parameter),xinit)
        predict_prevalence_i <- predict_prevalence_i/(predict_prevalence_i+predict_prevalence_s)
        predict_prevalence_s <- predict_prevalence_s/(predict_prevalence_i+predict_prevalence_s)
        predict_df <- data.frame(Year=predict_years,Prevalence=predict_prevalence_i)

        base_data <- data %>% arrange(Year)
        print(base_data$Year)
        base_data$Prevalence <- base_data$Prevalence/((predict_prevalence_i+predict_prevalence_s)[(1:14)*4])
        print(base_data$Prevalence)
        predict_df <- data.frame(Year=predict_years,Prevalence=predict_prevalence_i)
        base_graph(base_data)+geom_line(data=predict_df,aes(x=Year,y=Prevalence),size=1,color="red")+ylim(0,1)
    })
    output$gamma_plot_s <-renderPlot({
        xinit=c(S=1-0.0022,I=0.0022)
        predict_years=seq(2006,2050,by=0.25)
        predict_prevalence <- predict_s(predict_years,c(beta=input$gamma_beta_parameter,
                                                        gamma=input$gamma_gamma_parameter),xinit)
        predict_df <- data.frame(Year=predict_years,Prevalence=predict_prevalence)
        base_graph(data)+geom_line(data=predict_df,aes(x=Year,y=Prevalence),size=1,color="red")+ylim(0,1)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
