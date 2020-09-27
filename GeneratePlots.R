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
             c(1,1,3,3),
             c(4,4,5,5),
             c(4,4,6,6))
loc <- "United States"
curDate <- "2020-09-20"
for(curDate in unique(covid$date)){
  curData <- covid[as.Date(covid$date)<as.Date(curDate),]
  
  p1 <- ggplot(data=curData[curData$location==loc,])+
          geom_abline(slope=0.05, color="#FF0000", size = 1)+
          geom_path(aes(x=new_cases_smoothed,y=new_deaths_smoothed, color=as.Date(date, origin = "2019-12-31")), size=1)+
          geom_point(aes(x=new_cases,y=new_deaths, color=as.Date(date, origin = "2019-12-31")), size=1)+
          xlab("New Cases")+
          ylab("New Deaths")+
          theme(legend.position="none")
  
  p2 <- ggplot(data=curData[curData$location==loc,])+
          geom_abline(slope=10, color="#FF0000", size = 1)+
          geom_path(aes(x=new_cases_smoothed,y=new_tests_smoothed, color=as.Date(date, origin = "2019-12-31")), size=1)+
          geom_point(aes(x=new_cases,y=new_tests, color=as.Date(date, origin = "2019-12-31")), size=1)+
          xlab("New Cases")+
          ylab("New Tests")+
          theme(legend.position="none")
  
  
  cases <- ggplot(data=curData[curData$location==loc,])+
          geom_path(aes(x=as.Date(date),y=new_cases_smoothed, color=as.Date(date, origin = "2019-12-31")), size=1)+
          geom_point(aes(x=as.Date(date),y=new_cases, color=as.Date(date, origin = "2019-12-31")), size=1)+
          theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
          xlab("")+
          ylab("")+
          ggtitle("New Cases")+
          scale_x_date(date_labels = "%Y-%m-%d")
  

  deaths <- ggplot(data=curData[curData$location==loc,])+
          geom_path(aes(x=as.Date(date),y=new_deaths_smoothed, color=as.Date(date, origin = "2019-12-31")), size=1)+
          geom_point(aes(x=as.Date(date),y=new_deaths, color=as.Date(date, origin = "2019-12-31")), size=1)+
          theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
          xlab("")+
          ylab("")+
          ggtitle("New Deaths")+
          scale_x_date(date_labels = "%Y-%m-%d")
  
  tests <- ggplot(data=curData[curData$location==loc,])+
          geom_path(aes(x=as.Date(date),y=new_tests_smoothed, color=as.Date(date, origin = "2019-12-31")), size=1)+
          geom_point(aes(x=as.Date(date),y=new_tests, color=as.Date(date, origin = "2019-12-31")), size=1)+
          theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
          xlab("")+
          ylab("")+
          ggtitle("New Tests")+
          scale_x_date(date_labels = "%Y-%m-%d")
  
  
  today <- curData[as.Date(curData$date, origin="2019-12-31")==as.Date(curDate, origin="2019-12-31"),]
  todayLoc <- today[today$location==loc,]
  totalCases <- todayLoc$total_cases
  totalDeaths <- todayLoc$total_deaths
  totalTests <- todayLoc$total_tests
  totals <- c(totalTests,totalCases,totalDeaths)
  totalsLabs <- c("Tests","Cases","Deaths")
  
  bars <- ggplot()+
            geom_bar(aes(x=totalsLabs,y=totals), stat="identity")+
            theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
            xlab("")+
            ggtitle("Total")+
            scale_y_log10()
  
  p4 <- grid.arrange(p1,cases,deaths,p2, tests, bars, layout_matrix=lay)
  
 
  ggsave(paste("./plots/",curDate,".jpg",sep=""), p4, width = 14, heigh=7)
}



ggplot(data=covid[which(covid$location!="World" & covid$date == "2020-03-20"),]%>%, 
       aes(x=1, y=total_deaths, fill=location))+
  geom_bar(stat="identity", width=1, color="black")+
  coord_polar(theta = "y")+
  theme_void()


