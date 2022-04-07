library(tidyverse)
base_graph <- function(base_data){
  ggplot(base_data,aes(x=Year,y=Prevalence))+geom_point(size=2)+theme_classic()
}
