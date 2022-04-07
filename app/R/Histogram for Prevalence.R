cwd<-read.csv("app/Data/wasting_disease.csv")
library(ggplot2)

#I lied with the title, I'm making a barplot because it's by year

ggplot(cwd,aes(x=Year,y=Prevalence,fill=Year))+
  geom_bar(stat="identity")+
  ylab("CWD Prevalence (%)")+
  theme_classic()+
  scale_x_continuous(breaks=c(2006:2019))
