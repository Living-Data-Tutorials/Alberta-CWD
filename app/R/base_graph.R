library(tidyverse)
base_graph <- function(base_data){
  print("hello!")
  print(base_data$Prevalence)
  ggplot(base_data,aes(x=Year,y=Prevalence))+geom_point(size=2)+theme_classic()
}
