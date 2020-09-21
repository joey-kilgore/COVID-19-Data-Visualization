# Load up data https://github.com/owid/covid-19-data/tree/master/public/data
covid<- read.csv(file ="owid-covid-data.csv", TRUE)
head(covid)

# check unique locations
unique(covid$location)

# Gathering US covid and basic graphs
date = covid[covid$location=="United States",]$date
totalCases = covid[covid$location=="United States",]$total_cases
totalDeaths = date = covid[covid$location=="United States",]$total_deaths

plot(date,totalCases)
plot(date,totalDeaths)

# GGPlot graphs
library(ggplot2)
library(gridExtra)

lay <- rbind(c(1,1,2,2),
             c(1,1,3,3))
loc <- "United States"
#curDate <- "2020-09-20"
for(curDate in unique(covid$date)){
  curData <- covid[as.Date(covid$date)<as.Date(curDate),]
  
  p1 <- ggplot(data=curData[curData$location==loc,])+
          geom_abline(slope=0.05, color="#FF0000", size = 1)+
          geom_path(aes(x=new_cases_smoothed,y=new_deaths_smoothed, color=as.Date(date, origin = "2019-12-31")), size=1)+
          geom_point(aes(x=new_cases,y=new_deaths, color=as.Date(date, origin = "2019-12-31")), size=1)+
          theme(legend.position="none")
  
  
  p2 <- ggplot(data=curData[curData$location==loc,])+
          geom_path(aes(x=as.Date(date),y=new_cases_smoothed, color=as.Date(date, origin = "2019-12-31")), size=1)+
          geom_point(aes(x=as.Date(date),y=new_cases, color=as.Date(date, origin = "2019-12-31")), size=1)+
          theme(legend.position="none")+
          xlab("")+
          scale_x_date(date_labels = "%Y-%m-%d")
  
  
  p3 <- ggplot(data=curData[curData$location==loc,])+
          geom_path(aes(x=as.Date(date),y=new_deaths_smoothed, color=as.Date(date, origin = "2019-12-31")), size=1)+
          geom_point(aes(x=as.Date(date),y=new_deaths, color=as.Date(date, origin = "2019-12-31")), size=1)+
          theme(legend.position="none")+
          xlab("")+
          scale_x_date(date_labels = "%Y-%m-%d")
  
  
  p4 <- grid.arrange(p1,p2,p3, layout_matrix=lay)
  
 
  ggsave(paste("./plots/",curDate,".jpg",sep=""), p4, width = 14, heigh=7)
}
