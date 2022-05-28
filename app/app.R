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
               tabPanel("Introduction",value="page1",
                        # Application title
                        titlePanel("Modeling the spread of Chronic Wasting Disease in mule deer"),
                        br(),
                        p("In this tutorial, we're going to use a simple SI model to track the spread of Chronic Wasting Disease (CWD) in an Albertan mule deer over time. We’ll then use the same model to look at how culling of susceptibles could be used eliminate CWD from our mule deer population."),
                        withMathJax(),
                        h3("Learning Objectives"),
                        tags$ol(
                            tags$li("Become familiar with the components of a simple SI model, and how they can be applied to predict the spread of a disease through time."),
                            tags$li("Understand the role of \\(R_0\\) in determining the spread of a disease."),
                            tags$li("Learn how strategies such as culling can be used with knowledge of \\(R_0\\) to prevent the spread of a disease.")
                        ),
                        hr(),
                        h3("What is CWD?"),
                        p("CWD is a prion disease that affects deer, elk, reindeer, and moose. It has a long incubation period (typically 18-24 months), and symptoms include excessive salivation, increased drinking and urination, weight loss, confusion, and tremors. There is no recovery and no treatment, so eventual death is certain (mean time from oral infection to death is ~23 months).
                          Below is a picture of our host of interest, the mule deer (", tags$a(href="https://www.rmef.org/wp-content/uploads/2020/09/CO-Mule-deer-buck.jpg","image source"), ")."),
                        br(),
                        tags$img(src="CWDMuleDeer.jpg",height= "40%", width="40%",style="display: block; margin-left: auto; margin-right: auto;"),
                        br(),
                        br(),
                        br(),
                        h3("The Problem"),
                        p("Data show that CWD has increased exponentially in Albertan mule deer from 2006-2019, as is clearly evident in the graph below."),
                        br(),
                        tags$img(src="CWDPrevalence2.jpeg",height="50%",width="50%",style="display: block; margin-left: auto; margin-right: auto;"),
                        br(),
                        tags$strong("Will this trend continue?"),
                        p("If we can predict the spread of CWD in Albertan mule deer, we can decide how concerned we should be about this disease taking over the population. Click 'start' below to see how we can construct a simplified model of the disease to help predict how it will spread through the population. "),
                        br(),
                        br(),
                        br(),
                        fluidRow(
                            column(width=12,align="center",
                                   actionButton("start","Start",
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4; padding:5px; font-size:150%")
                            )
                        ),
                        br(),
                        br(),
                        br()),
               
               tabPanel("Modeling CWD",value="page2",
                        titlePanel("How can we model the spread of CWD?"),
                        br(),
                        tags$img(src="SI.jpg",height="25%",width="25%",style="display: block; margin-left: auto; margin-right: auto;"),
                        br(),
                        HTML(paste("We will use a basic compartmental model, known as an SI model, to track the ", tags$strong("prevalence "),"of CWD in mule deer over time. We assume that the mule deer population consists of \\(N\\) individuals that are split into two groups: ", tags$strong("\\(S\\) (susceptible) "), "and ", tags$strong("\\(I\\) (infected)"), ". The birth and death rates are exactly equal in the two groups, so we can say that the population is at ", tags$strong("equilibrium."),  sep="")),
                        br(),
                        br(),
                        HTML(paste(tags$strong("Prevalence: "), "The proportion of infected individuals in a population.", sep="")),
                        br(),
                        HTML(paste(tags$strong("Susceptible: "), "Individuals capable of being affected by the disease.", sep="")),
                        br(),
                        HTML(paste(tags$strong("Infected: "), "Individuals who have the disease.", sep="")),
                        br(),
                        HTML(paste(tags$strong("Equilibrium (of a population): "), "A population with equal birth and death rates, such that the number of individuals remains constant through time", sep="")),
                        br(),
                        hr(),
                        HTML(paste("In this tutorial, we are going to look at 3 factors that could influence the spread of CWD over time: the ", tags$strong("transmission rate (\\(\\beta\\)), "), "the ", tags$strong("excess death rate of infected individuals (\\(\\gamma\\))"), ", and the effects of ", tags$strong("culling susceptible individuals. "), sep="")),
                        br(),
                        HTML(paste("Click the 'next page' button to explore what happens when we allow susceptible individuals to become infected - in other words, when we allow transmission of the disease.")),
                        br(),
                        br(),
                        br(),
                        fluidRow(
                            column(width=12,align="center",
                                   actionButton("next1","Next Page",
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4; padding:5px; font-size:150%")
                            )
                        ),
                        br(),
                        br(),
                        br()
               ),
               tabPanel("Transmission",value="page3",
                        withMathJax(),
                        titlePanel("Transmission (the \\(\\beta\\) parameter)"),
                        br(),
                        tags$img(src="BasicModel.jpg",height="25%",width="25%",style="display: block; margin-left: auto; margin-right: auto;"),
                        br(),
                        helpText("The transmission rate, \\(\\beta\\), determines how quickly the disease spreads from infected to susceptible individuals. Once infected, the susceptible individuals move from class 'S' to class 'I'."),
                        helpText("If the proportion of infected individuals in a population is \\(\\frac{I}{N}\\), and \\(\\beta\\) is the rate at which individuals interact with each other, then the number of new infections in a single year is $$S \\cdot \\beta \\cdot \\frac{I}{N}$$ This is the average number of susceptible individuals that interact with an infected individual, multiplied by the transmission rate. The rate of change in susceptible individuals in a single year is therefore
                        $$ \\frac{dS}{dt} = - S \\cdot \\beta \\cdot \\frac{I}{N}$$
                            Note that this term is negative because the susceptible individuals are lost to the infected class. Similarly, since all newly infected individuals go directly to the infected class, the rate of change in infected individuals in a single year is
                        $$ \\frac{dI}{dt} = S \\cdot \\beta \\cdot \\frac{I}{N}$$
                            Based on these equations, we expect that a larger transmission rate (\\(\\beta\\)) will mean a faster spread of CWD as more susceptible individuals become infected, compared to a smaller transmission rate. Let’s check it out below!
                            "),
                        br(),
                        hr(),
                        tags$div(
                            HTML(paste("Try exploring how different values of \\(\\beta\\) change the spread of the disease over time! The ", tags$strong(" black dots"), " are our real data, the " , tags$span(style="color:red", "red line "), "is our model prediction for prevalence, the ",  tags$span(style="color:blue", "blue line "), "is our prediction for the number of susceptible individuals, and the ", tags$strong("black dashed line"), " is our prediction for the total number of individuals, all relative to the size of the population before it was infected by CWD.", sep = ""))
                        ),
                        br(),
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
                        fluidRow(
                            column(width=8,
                                   h3("Think about..."),
                                   tags$ol(
                                       tags$li("What happens to the prevalence when the transmission rate is 0? What about when it is at it's maximum (3 here)?"), 
                                       tags$li("What value of \\(\\beta\\) allows the model to best fit the data?")
                                   )
                            ),
                            column(width=4,align="center",
                                   actionButton("next2","Next Page",
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4; padding:5px; font-size:150%")
                            )
                        ),
                        br(),
                        br(),
                        br()),
               tabPanel("Death",value="page4",
                        withMathJax(),
                        br(),
                        titlePanel("Excess death from disease (the \\(\\gamma\\) parameter)"),
                        br(),
                        tags$img(src="ModelWithDeath.jpg",height="35%",width="35%",style="display: block; margin-left: auto; margin-right: auto;"),
                        br(),
                        br(),
                        helpText("Now we're going to look at what happens when infected individuals have a higher or lower death rate than the susceptible individuals. This death rate is measured by \\(\\gamma\\), where a higher \\(\\gamma\\) means a higher death rate for infected compared to susceptible individuals."),
                        helpText("In the previous section we assumed that the birth and death rate of all individuals were equal, regardless of whether or not they were infected. This meant that the population size remained constant (why would this be the case?).
                                 Now, we introduce the \\(\\gamma\\) parameter, which represents how much greater the death rate of infected individuals is compared to susceptible individuals."),
                        helpText("We can represent this by updating our equation for the rate of change in infected individuals. We now have $$ \\frac{dI}{dt} = S \\cdot \\beta \\cdot \\frac{I}{N} - \\gamma \\cdot I$$ where
                                 the \\(\\gamma\\) parameter is negative because when infected individuals die, they leave the infected category. In other disease models a certain fraction of infected individuals recover and move back
                                 into the susceptible category, but evidence suggests that mule deer cannot recover from CWD."),
                        hr(),
                        p("Try exploring how different values of both \\(\\gamma\\) and \\(\\beta\\) contribute to the course of the disease in the population. In particular, pay attention to how the " ,tags$strong("total population size"), "changes.
                          In the sidebar, we have also included the population size after 100 years (scaled to the original population size) 
                          in order to illustrate the long-term dynamics of the disease." ),
                        tags$div(
                            HTML(paste("As a reminder, the ", tags$strong(" black dots"), " are our real data, the " , tags$span(style="color:red", "red line "), "is our model prediction for prevalence, the ",  tags$span(style="color:blue", "blue line "), "is our prediction for the number of susceptible individuals, and the ", tags$strong("black dashed line"), " is our prediction for the total number of individuals, all relative to the size of the population before it was infected by CWD.", sep = ""))
                        ),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("gamma_beta_parameter","\\(\\beta\\)",min=0,max=2,step=0.001,value=0),
                                helpText("The slider above controls \\(\\beta\\), which is the transmission parameter"),
                                hr(),
                                sliderInput("gamma_gamma_parameter","\\(\\gamma\\)",min=0,max=3,step=0.001,value=0),
                                helpText("the slider above controls \\(\\gamma\\), which is the excess death parameter"),
                                hr(),
                                span(textOutput("final_pop1"),style="font-size:large")
                            ),
                            mainPanel(
                                plotOutput("gamma_plot_i")
                            )
                        ),
                        hr(),
                        fluidRow(
                            column(width=8,
                                   h3("Think about..."),
                                   tags$ol(
                                       tags$li("What happens to the disease prevalence when we account for extra death of infected individuals (\\(\\gamma > 0)\\) ? Can you find values for both \\(\\gamma\\) and \\(\\beta\\) that fit the data points?"), 
                                       tags$li("What happens to the disease prevalence and total population size when the transmission rate is greater than the death rate (\\(\\beta > \\gamma\\)) ? What about when \\(\\beta < \\gamma\\) ? Why is this the case? To answer this,
                                               it might be helpful to remember that \\(\\beta\\) is the rate at which infected individuals pass on the disease per year, and \\(\\gamma\\) is the rate at which individuals die from the disease (in excess of the equilibrium death rate). 
                                               You can then ask what happens when the disease spreads faster than it kills its hosts, and what happens when the disease kills faster than it spreads."),
                                   ),
                                   br(),
                            ),
                            column(width=4,align="center",
                                   actionButton("next3","Next Page",
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4; padding:5px; font-size:150%")
                            )
                        ),
                        
                        br(),
                        br(),
                        br(),
                        
               ),
               tabPanel("Culling",value="page5",
                        titlePanel("Culling and vaccination (the \\(c\\) parameter)"),
                        br(),
                        tags$img(src="ModelWithCull.jpg",height="40%",width="40%",style="display: block; margin-left: auto; margin-right: auto;"),
                        withMathJax(),
                        br(),
                        br(),
                        h4("Would initiating culls of susceptibles help fight CWD in mule deer?"),
                        p("In order to answer this question, we need to introduce the concept of \\(R_0\\) (pronounced",tags$em("r-naught"),")"),
                        hr(),
                        h3("What is \\(R_0\\)?"),
                        helpText("The rate of spread of CWD increases with the transmission rate \\(\\beta\\), but decreases with the infectious death rate \\(\\gamma\\) because we assume that individuals can only spread the disease while they're still alive. In fact, we can calculate the number of new infections that a single infected individual produces in a new population of all susceptible individuals as $$R_0=\\frac{\\beta}{\\gamma}$$ When \\(R_0 > 1\\), a typical infected individual will infect more than one susceptible individual and the disease spreads. However, if \\(R_0 < 1\\), infected individuals cannot replace themselves and CWD will not spread in the new population."),
                        br(),
                        tags$div(
                            HTML(paste(tags$strong("KEY POINT :"), "Culling changes this prediction. By culling a proportion of the susceptible population equal to \\(c\\), we reduce the number of susceptible individuals to a fraction, \\((1-c)\\) of their number before the culls. These culls effectively reduce the contact rate among individuals, so that the transmission rate is scaled to \\((1-c) \\cdot \\beta\\). With culling, the average number of new infections from a single infected individual becomes $$(1-c) \\cdot \\frac{\\beta}{\\gamma}$$"), sep = "")),
                        p("In our simple model, this is is also equivalent to vaccinating a proportion of the susceptible population equal to \\(c\\) with a vaccine that perfectly prevents infection."),
                        hr(),
                        p("Try exploring how culling different proportions of the susceptible population (\\(c\\)) changes how the disease spreads. In the sidebar, we have also included a calculation of \\(R_0\\), 
                          which is an intrinsic property of the disease and only depends on \\(\\beta\\) and \\(\\gamma\\). We have also included the population size after 100 years (scaled to the original population size) 
                          in order to illustrate the long-term dynamics of the disease."),
                        tags$div(
                            HTML(paste("As a reminder, the ", tags$strong(" black dots"), " are our real data, the " , tags$span(style="color:red", "red line "), "is our model prediction for prevalence, the ",  tags$span(style="color:blue", "blue line "), "is our prediction for the number of susceptible individuals, and the ", tags$strong("black dashed line"), " is our prediction for the total number of individuals, all relative to the size of the population before it was infected by CWD.", sep = ""))
                        ),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("c_beta_parameter","\\(\\beta\\)",min=0,max=2,step=0.001,value=0),
                                p("the slider above controls \\(\\beta\\), which is the transmission parameter"),
                                hr(),
                                sliderInput("c_gamma_parameter","\\(\\gamma\\)",min=0,max=3,step=0.001,value=0),
                                p("the slider above controls \\(\\gamma\\), which is the death parameter"),
                                hr(),
                                sliderInput("c_c_parameter","\\(c\\)",min=0,max=1,step=0.001,value=0),
                                p("the slider above controls \\(c\\), which is the culling parameter"),
                                hr(),
                                span(uiOutput("R0"),style="font-size:large"),
                                hr(),
                                span(textOutput("final_pop"),style="font-size:large"),
                            ),
                            mainPanel(
                                plotOutput("R0_plot")
                            ),
                        ),
                        hr(),
                        
                        fluidRow(
                            column(width=8,
                                   h3("Think about..."),
                                   tags$ol(
                                       tags$li("If we have a population of only susceptibles, what proportion do we need to cull to prevent CWD from spreading in the population? Why is this the case?"),
                                       tags$li("How does culling susceptible individuals change the relative proportions of susceptible and infected individuals? What about the (scaled) population size?")
                                   ),
                            ),
                            column(width=4,align="center",
                                   actionButton("next4","Next Page",
                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4; padding:5px; font-size:150%")
                            )
                        ),
                        
                        br(),
                        br(),
                        br()
               ),
               tabPanel("Summary and references",value="page6",
                        titlePanel("Summary"),
                        p("In this tutorial, we have seen how a simple", tags$strong("SI (Susceptible and Infected)"), "compartmental model can be used to model disease spread."),
                        tags$ul(
                            tags$li("By increasing the", tags$strong("rate at which individuals interact"), "(\\(\\beta\\)) the disease spreads faster through the population."),
                            tags$li("If infected individuals die faster than susceptible individuals, two possile scenarios can occur:",
                                    tags$ul(
                                        tags$li("If", tags$strong("\\(\\gamma\\) < \\(\\beta\\)"), "then", tags$strong("the population will go extinct"), "as each infected individual will infect more than one susceptible individual before it dies."),
                                        tags$li("If", tags$strong("\\(\\gamma\\) > \\(\\beta\\)"), "then", tags$strong("the disease will be eliminated in the population"), "as each infected individual will infect less than one susceptible individual before it dies")
                                        )
                                    ),
                            tags$li("We call this average number of infections caused by one infected individual",tags$strong("\\(R_0\\)"), ". 
                                    More specifically, this number is the average number of infections cause by a single individual in a disease-free population."),
                            tags$li("By ", tags$strong("culling or vaccinating"), "a portion of the susceptible population, we can reduce the average number of infections per infected individual to below 1, which results in a elimination of the disease. 
                                    In our model, a fraction of the susceptible population more than \\(1-1/R_0\\) must be culled or vaccinated to eliminate the disease.")
                        ),
                        p("It is important to note that this is a simple model of a potentially complex system, and does not necessarily reflect how the disease will progress through the population.
                           For example, it may be the case that when the population size decreases from excess deaths by the disease, more resources are available to the surviving deer, thus increasing their birth rates.
                           Under certain circumstances, this can lead to a smaller stable equilibrium population size instead of complete population extinction. Additionally, this model is deterministic, meaning that random differences
                           in the number of infections between individuals are not considered. Especially in small population sizes, this can be an important consideration for predicting the course of a disease."),
                        hr(),
                        p(tags$strong("Data Sources")),
                        p("The number of infected mule deer in Alberta was compiled from the following two sources:"),
                        p("Government of Alberta. (n.d.). Chronic wasting disease – history in Alberta. Alberta.ca. Retrieved May 27, 2022, from https://www.alberta.ca/chronic-wasting-disease-history-in-alberta.aspx "),
                        p("Potapov, A., Merrill, E., Pybus, M., & Lewis, M. A. (2015). Empirical Estimation of R 0 for Unknown Transmission Functions: The Case of Chronic Wasting Disease in Alberta. Plos one, 10(10), e0140024.")
                        
                   
               )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observe({
        beta1 <- input$beta_parameter
        updateSliderInput(session,"gamma_beta_parameter",value=beta1)
    })
    observe({
        beta2 <- input$gamma_beta_parameter
        gamma2 <- input$gamma_gamma_parameter
        updateSliderInput(session,"beta_parameter",value=beta2)
        updateSliderInput(session,"c_beta_parameter",value=beta2)
        updateSliderInput(session,"c_gamma_parameter",value=gamma2)
    })
    observe({
        beta3 <- input$c_beta_parameter
        gamma3 <- input$c_gamma_parameter
        updateSliderInput(session,"beta_parameter",value=beta3)
        updateSliderInput(session,"gamma_beta_parameter",value=beta3)
        updateSliderInput(session,"gamma_gamma_parameter",value=gamma3)
    })
    output$beta_plot <-renderPlot({
        xinit=c(S=1-0.0022,I=0.0022)
        predict_years=seq(2006,2050,by=0.25)
        prediction_i <- predict_i(predict_years,c(beta=input$beta_parameter,gamma=0,c=0),xinit)
        prediction_s <- predict_s(predict_years,c(beta=input$beta_parameter,gamma=0,c=0),xinit)
        
        predict_df <- data.frame(Year=predict_years,prevalence_i=prediction_i,prevalence_s=prediction_s)
        base_graph(data)+geom_line(data=predict_df,aes(x=Year,y=prevalence_i),size=1,color="red")+
            geom_line(data=predict_df,aes(x=Year,y=prevalence_s),size=1,color="blue")+
            geom_abline(slope=0,intercept=1,linetype="dashed",size=1)+ylim(0,1)
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
        
        predict_df <- data.frame(Year=predict_years,Prevalence_i=predict_prevalence_i,Prevalence_s=predict_prevalence_s,
                                 N=predict_prevalence_s+predict_prevalence_i)
        
        base_data <- inner_join(data,predict_df)
        
        base_data$Prevalence <- base_data$Prevalence/(base_data$Prevalence_i+base_data$Prevalence_s)
        
        base_graph(base_data)+geom_line(data=predict_df,aes(x=Year,y=Prevalence_i),size=1,color="red")+
            geom_line(data=predict_df,aes(x=Year,y=Prevalence_s),size=1,color="blue")+
            geom_line(data=predict_df,aes(x=Year,y=N),size=1,linetype="dashed")+ylim(0,1.01)
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
        predict_df <- data.frame(Year=predict_years,Prevalence_i=predict_prevalence_i,Prevalence_s=predict_prevalence_s,
                                 N=predict_prevalence_s+predict_prevalence_i)
        
        base_data <- inner_join(data,predict_df)
        
        base_data$Prevalence <- base_data$Prevalence/(base_data$Prevalence_i+base_data$Prevalence_s)
        
        ggplot(data=predict_df,aes(x=Year,y=Prevalence_i))+geom_line(size=1,color="red")+
            geom_line(data=predict_df,aes(x=Year,y=Prevalence_s),size=1,color="blue")+
            geom_line(data=predict_df,aes(x=Year,y=N),size=1,linetype="dashed")+ylim(0,1.01)+theme_classic()+
            xlab("Year")+ylab("Population Size (scaled to the \n pre-pandemic population size)")+
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=16,face="bold"))
    })
    output$R0 <- renderUI({
        r0=round(input$c_beta_parameter/input$c_gamma_parameter,2)
        if(is.nan(r0)){r0 <- 0}
        if(is.infinite(r0)){r0 <- "Not defined"}
        withMathJax(p("\\(R_0\\) for this disease: ",r0))
    })
    output$final_pop <- renderText({
        xinit=c(S=1-0.0022,I=0.0022)
        final_pop_i = predict_i(c(2006,2106),c(beta=input$c_beta_parameter,
                                               gamma=input$c_gamma_parameter,
                                               c=input$c_c_parameter),xinit)[2]
        final_pop_s = predict_s(c(2006,2106),c(beta=input$c_beta_parameter,
                                               gamma=input$c_gamma_parameter,
                                               c=input$c_c_parameter),xinit)[2]
        final_pop <- round(final_pop_i+final_pop_s,2)
        paste0("Fraction of the population surviving after 100 years: ",final_pop)
    })
    
    output$final_pop1 <- renderText({
        xinit=c(S=1-0.0022,I=0.0022)
        final_pop_i = predict_i(c(2006,2106),c(beta=input$c_beta_parameter,
                                               gamma=input$c_gamma_parameter,
                                               c=input$c_c_parameter),xinit)[2]
        final_pop_s = predict_s(c(2006,2106),c(beta=input$c_beta_parameter,
                                               gamma=input$c_gamma_parameter,
                                               c=input$c_c_parameter),xinit)[2]
        final_pop <- round(final_pop_i+final_pop_s,2)
        paste0("Fraction of the population surviving after 100 years: ",final_pop)
    })
    
    #button processes
    observeEvent(input$start,{
        updateTabsetPanel(session,"mainpage",selected="page2")
    })
    observeEvent(input$next1,{
        updateTabsetPanel(session,"mainpage",selected="page3")
    })
    observeEvent(input$next2,{
        updateTabsetPanel(session,"mainpage",selected="page4")
    })
    observeEvent(input$next3,{
        updateTabsetPanel(session,"mainpage",selected="page5")
    })
    observeEvent(input$next4,{
        updateTabsetPanel(session,"mainpage",selected="page6")
    })
}
# Run the application 
shinyApp(ui = ui, server = server)