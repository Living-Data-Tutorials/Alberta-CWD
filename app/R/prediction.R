#I'm pretty sure deSolve is included by default with R
library(deSolve)
library(tidyverse)

#load in the data
data <-read.csv("Data/wasting_disease.csv")
data <- data%>%arrange(Year)

#this is our funtion that represents the differential equations.
#t is the time for it to be evaluated at
#x is the state vector of our system. Here, it contains "S" (susceptible) and "I"
#(infections) classes
#parms is the list of parameters. Here, we just have one: beta (the transmission coefficient)
#it returns a list of the derivatives, here represented as R. For reference, here we have:
#dS/dt=-beta*S*I/N, where N=S+I
#dI/dt=beta*S*I/N
#This is just an exponential model, but we'll start with this!
simodel=function(t,x,parms){
  beta=parms["beta"]
  r=rep(0,2)
  r[1]=-beta*x["S"]*x["I"]/(x["S"]+x["I"])
  r[2]=beta*x["S"]*x["I"]/(x["S"]+x["I"])-0.5*x["I"]
  
  return(list(r))
}
#These are some functions for predicting the I and S states separately
#Times is the times for predictions
#parms is the parameters for the ODE
#xint is the initial state of the system
#These functions return a vector of the state for each time point
predict_i<- function(times,parms,xinit){
  #ode is the ode solver, func here is the simodel function that takes in t,x,parms and returns the derivatives
  out=ode(y=xinit,times=times,func=simodel,parms=parms)
  predictions=out[,3]
  predictions
}
predict_s<- function(times,parms,xinit){
  out=ode(y=xinit,times=times,func=simodel,parms=parms)
  predictions=out[,2]
  predictions
}

#This is our cost function, which gives the sum of squares between predictions for a given parameter set,
#and the actual values in our data.
#We will minimize this function later to fit the paramaters
#parms is the parameters with which we are going to calculate the sum of squares
ssq<- function(parms){
  times=data$Year
  prevalences=data$Prevalence
  xinit=c(S=1-min(data$Prevalence),I=min(data$Prevalence))
  predictions=predict_i(times,parms,xinit)
  return(sum((predictions-prevalences)^2))
}


#optim is what fits the parameters. The method, lower, and upper parameters aren't really important,
#but the first value is a vector of the starting values for our parameters
#from these starting values, optim will minimize the second argument, which is the sum of squares between our predictions and actual values
#fiteval=optim(c(beta=1),ssq,method="L-BFGS-B",lower=0,upper=10)

#fit eval now has the best(ish) fitting parameters

#Here we make a simple plot comparing our results. We could extend the time vector to make predictions
# 
# time=2006:2019
# 
# xinit=c(S=1-0.0022,I=0.0022)
# out=ode(y=xinit,times=time,func=simodel,parms=fiteval$par)
# plot(x=time,y=out[,3],ylab="Prevalence",xlab="Year")
# points(x=time,y=data$Prevalence,col="red")

