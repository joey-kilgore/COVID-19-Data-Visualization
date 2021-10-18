# Simplified Graph
covid<- read.csv(file ="owid-covid-data.csv", TRUE)
head(covid)
library(ggplot2)
library(viridis)

USA <- covid[as.Date(covid$date, origin="2019-12-31")<=as.Date(curDate, origin = "2019-12-31"),]
USA <- USA[USA$location=="United States",]
head(USA)

plot <- ggplot(data=USA)+
          geom_point(aes(x=as.Date(date),y=new_cases,size=new_tests,color=new_deaths))+
          scale_color_viridis() +
          scale_size(range= c(1,10))+
          ggtitle("United States")+
          xlab("Date")+
          ylab("New Cases (Daily)")+
          guides(size=guide_legend(title="New Tests (Daily)"), color=guide_legend(title="New Deaths (Daily)"))
show(plot)
ggsave("./US_Simple.png",plot, width=16, height=9)