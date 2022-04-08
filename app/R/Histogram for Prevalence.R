cwd<-read.csv("Data/wasting_disease.csv")
library(ggplot2)

#I lied with the title, I'm making a barplot because it's by year

ggplot(cwd,aes(x=Year,y=Prevalence,fill=Year))+
  geom_bar(stat="identity")+
  ylab("CWD Prevalence (%)")+
  theme_classic()+
  scale_x_continuous(breaks=c(2006:2019))+
  theme(legend.position="none")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))
