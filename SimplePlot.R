# Simplified Graph
covid<- read.csv(file ="owid-covid-data.csv", TRUE)
head(covid)
library(ggplot2)

USA <- covid[as.Date(covid$date, origin="2019-12-31")<=as.Date(curDate, origin = "2019-12-31"),]
USA <- USA[USA$location=="United States",]
head(USA)

plot <- ggplot(data=USA)+
          geom_point(aes(x=as.Date(date),y=new_cases,size=new_tests,color=new_deaths))+
          scale_color_gradientn(colours=rev(rainbow(7)))+
          scale_size(range= c(1,10))
show(plot)
