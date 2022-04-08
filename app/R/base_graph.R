library(tidyverse)
base_graph <- function(base_data){
  ggplot(base_data,aes(x=Year,y=Prevalence))+geom_point(size=2)+theme_classic()+
    xlab("Year")+ylab("Population Size (scaled to the \n pre-pandemic population size)")+
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"))
}
