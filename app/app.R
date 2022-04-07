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
               tabPanel("Intro",value="page1",
                        # Application title
                        titlePanel("Modeling the spread of Chronic Wasting Disease in mule deer"),
                        br(),
                        p("In this tutorial, we're going to use a simple compartmental model to track the spread of Chronic Wasting Disease (CWD) in an Albertan mule deer over time. We’ll then use the same model to look at how vaccination could be used to drive CWD to extinction in our mule deer population."),
                        #Two columns
                        fluidRow(
                            column(width=12,
                                   withMathJax(),
                                   h3("What is CWD?"),
                                   p("CWD is a prion disease that affects deer, elk, reindeer, and moose. It has a long incubation period (typically 18-24 months), and symptoms include excessive salivation, increased drinking and urination, weight loss, confusion, and tremors (Fig. 1)."),
                                   br(),
                                   tags$img(src="CWDMuleDeer.jpg",width="300px"),
                                   br(),
                                   br(),
                                   br(),
                                   p("There is no recovery and no treatment, so eventual death is certain (mean time from oral infection to death is ~23 months). CWD is increasing exponentially in Albertan mule deer (Fig. 2); if we can predict the spread of CWD in the future, we can decide how concerned we should be about this disease taking over the population."),
                                   tags$img(src="CWDPrevalence1.jpeg",width="300px"),
                                   #this is a horizontal line break
                                   hr(),
                                   h3("How can we model the spread of CWD?"),
                                   helpText("We will use a basic compartmental model, known as an SI model (Fig 3). Assume that the mule deer population consists of \\(N\\) individuals that are split into two groups: \\(S\\) (susceptible) and \\(I\\) (infected, with CWD). The population is at equilibrium, such that the birth and death rate are exactly equal. The transmission rate, \\(\\beta\\), determines how quickly the disease spreads from infected to susceptible individuals."),
                                   tags$img(src="BasicModel.jpg",width="300px"),
                                   helpText("If the proportion of infected individuals in a population is \\(\\frac{I}{N}\\), and \\(\\beta\\) is the rate at which individuals interact with each other, then the number of new infections in a single time period is $$S \\cdot \\beta \\cdot \\frac{I}{N}$$ This is the average number of susceptible individuals that interact with an infected individual, multiplied by the transmission rate. The rate of change in susceptible individuals in a single time period is therefore
                                   $$ \\frac{dS}{dt} = - S \\cdot \\beta \\cdot \\frac{I}{N}$$
Note that this term is negative because the susceptible individuals are lost to the infected class. Similarly, since all newly infected individuals go directly to the infected class, the rate of change in infected individuals in a single time period is
$$ \\frac{dI}{dt} = S \\cdot \\beta \\cdot \\frac{I}{N}$$
Based on these equations, we expect that a larger transmission rate (\\(\\beta\\)) will mean a faster spread of CWD as more susceptible individuals become infected, compared to a smaller transmission rate. Let’s check it out on the next page!
"),
                                   br(),
                                   br(),
                                   br()
            
                            )
                        )
               ),
               tabPanel("Transmission",
                        withMathJax(),
                        br(),
                        titlePanel("The \\(\\beta\\) parameter for the model"),
                        tags$div(
                            HTML(paste("Try exploring how different values of \\(\\beta\\) change the spread of the disease over time! The ", tags$strong("dots"), " are our real data, and the " , tags$span(style="color:red", "red line "), "is our model prediction.", sep = ""))
                        ),
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("beta_parameter","\\(\\beta\\)",0,3,0,step=0.001),
                                helpText("The slider above controls \\(\\beta\\), which is the transmission parameter")
                            ),
                            mainPanel(
                                plotOutput("beta_plot")
                            )
                        ),
                        hr(),
                        p("Maybe here is more descriptions?")),
               tabPanel("Death",
                        withMathJax(),
                        br(),
                        titlePanel("The \\(\\gamma\\) parameter"),
                        tags$img(src="ModelWithDeath.jpg",width="300px"),
                        helpText("Now we're going to look at what happens when infected individuals have a higher or lower death rate than the susceptible individuals. This death rate is measured by \\(\\gamma\\), where a higher \\(\\gamma\\) means a higher death rate for infected compared to susceptible individuals."),
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("gamma_beta_parameter","\\(\\beta\\)",min=0,max=2,step=0.001,value=0),
                                helpText("The slider above controls \\(\\beta\\), which is the transmission parameter"),
                                hr(),
                                sliderInput("gamma_gamma_parameter","\\(\\gamma\\)",min=0,max=3,step=0.001,value=0),
                                helpText("the slider above controls \\(\\gamma\\), which is the death parameter"),
                            ),
                            mainPanel(
                                h2("Infected"),
                                plotOutput("gamma_plot_i"),
                                hr(),
                                h2("Susceptible"),
                                plotOutput("gamma_plot_s")
                            )
                        ),
                        p("Maybe here is more descriptions?")),
               tabPanel("Vaccination",
                        titlePanel("The vaccination parameter, \\(v\\)"),
                        tags$img(src="ModelWithVax.jpg",width="300px"),
                        withMathJax(),
                        helpText("Is there a benefit to developing a vaccine to fight CWD in mule deer? Even though one doesn't exist yet, we'll look at how a vaccine might be able to help control the infected population. Assume that we can afford to vaccinate only a proportion, \\(v\\) of the population, how large does \\(v\\) need to be?"),
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("c_beta_parameter","\\(\\beta\\)",min=0,max=2,step=0.001,value=0),
                                p("the slider above controls \\(\\beta\\), which is the transmission parameter"),
                                hr(),
                                sliderInput("c_gamma_parameter","\\(\\gamma\\)",min=0,max=3,step=0.001,value=0),
                                p("the slider above controls \\(\\gamma\\), which is the death parameter"),
                                hr(),
                                sliderInput("c_c_parameter","\\(v\\)",min=0,max=1,step=0.001,value=0),
                                p("the slider above controls \\(v\\), which is the vaccination parameter"),
                                hr(),
                                span(textOutput("R0"),style="font-size:large")
                            ),
                            mainPanel(
                                plotOutput("R0_plot")
                            )
                        )),
               tabPanel("Birth",
                        titlePanel("The birth parameter"))
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
                                                        gamma=input$gamma_gamma_parameter,
                                                        c=0),xinit)
        predict_prevalence_s <- predict_s(predict_years,c(beta=input$gamma_beta_parameter,
                                                          gamma=input$gamma_gamma_parameter,
                                                          c=0),xinit)
        predict_prevalence_i <- predict_prevalence_i
        predict_prevalence_s <- predict_prevalence_s
        
        predict_df <- data.frame(Year=predict_years,Prevalence_i=predict_prevalence_i,Prevalence_s=predict_prevalence_s)

        base_data <- inner_join(data,predict_df)
        
        base_data$Prevalence <- base_data$Prevalence/(base_data$Prevalence_i+base_data$Prevalence_s)
        
        base_graph(base_data)+geom_line(data=predict_df,aes(x=Year,y=Prevalence_i),size=1,color="red")+ylim(0,1)
    })
    output$gamma_plot_s <-renderPlot({
        xinit=c(S=1-0.0022,I=0.0022)
        predict_years=seq(2006,2050,by=0.25)
        predict_prevalence_i <- predict_i(predict_years,c(beta=input$gamma_beta_parameter,
                                                          gamma=input$gamma_gamma_parameter,
                                                          c=0),xinit)
        predict_prevalence_s <- predict_s(predict_years,c(beta=input$gamma_beta_parameter,
                                                          gamma=input$gamma_gamma_parameter,
                                                          c=0),xinit)
        predict_prevalence_i <- predict_prevalence_i
        predict_prevalence_s <- predict_prevalence_s
        
        predict_df <- data.frame(Year=predict_years,Prevalence_i=predict_prevalence_i,Prevalence_s=predict_prevalence_s)
        
        
        ggplot(predict_df,aes(x=Year,y=Prevalence_s))+geom_line(size=1,color="red")+theme_classic()+ylim(0,1)
    })
    
    output$R0_plot <- renderPlot({
        xinit=c(S=1-0.0022,I=0.0022)
        predict_years=seq(2006,2050,by=0.25)
        predict_prevalence_i <- predict_i(predict_years,c(beta=input$c_beta_parameter,
                                                          gamma=input$c_gamma_parameter,
                                                          c=input$c_c_parameter),xinit)
        predict_prevalence_s <- predict_s(predict_years,c(beta=input$c_beta_parameter,
                                                          gamma=input$c_gamma_parameter,
                                                          c=input$c_c_parameter),xinit)
        predict_prevalence_i <- predict_prevalence_i
        predict_prevalence_s <- predict_prevalence_s
        
        predict_df <- data.frame(Year=predict_years,Prevalence_i=predict_prevalence_i,Prevalence_s=predict_prevalence_s)
        
        
        ggplot(predict_df,aes(x=Year,y=Prevalence_i))+geom_line(size=1,color="red")+theme_classic()+ylim(0,1)
    })
    output$R0 <- renderText({
        round(input$c_beta_parameter/input$c_gamma_parameter,2)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
