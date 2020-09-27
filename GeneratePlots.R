# Load up data https://github.com/owid/covid-19-data/tree/master/public/data
covid<- read.csv(file ="owid-covid-data.csv", TRUE)
head(covid)

# check unique locations
unique(covid$location)

# Gathering US covid and basic graphs
date = covid[covid$location=="United States",]$date
totalCases = covid[covid$location=="United States",]$total_cases
totalDeaths = date = covid[covid$location=="United States",]$total_deaths
totaltests = date = covid[covid$location=="United States",]$total_tests

plot(date,totalCases)
plot(date,totalDeaths)
plot(date,totalTests)

# GGPlot graphs
library(ggplot2)
library(gridExtra)

lay <- rbind(c(1,1,2,2),
             c(1,1,3,3),
             c(4,4,5,5),
             c(4,4,6,6))
loc <- "United States"
curDate <- "2020-09-10"
for(curDate in unique(covid$date)){
  curData <- covid[as.Date(covid$date, origin="2019-12-31")<=as.Date(curDate, origin = "2019-12-31"),]
  
  maxCases <- max(curData[curData$location==loc,]$new_cases, na.rm=TRUE)
  maxDeaths <- max(curData[curData$location==loc,]$new_deaths, na.rm=TRUE)
  maxTests <- max(curData[curData$location==loc,]$new_tests, na.rm=TRUE)
  
  p1 <- ggplot(data=curData[curData$location==loc,])+
          geom_abline(slope=0.05, color="#FF0000", size = 1)+
          geom_path(aes(x=new_cases_smoothed,y=new_deaths_smoothed, color=as.Date(date, origin = "2019-12-31")), size=1)+
          geom_point(aes(x=new_cases,y=new_deaths, color=as.Date(date, origin = "2019-12-31")), size=1)+
          xlab("New Cases")+
          ylab("New Deaths")+
          annotate("text", x=maxCases*0.9,y=maxCases*0.9*0.05,label="DEATH RATE = 5%")+
          annotate("text", x=maxCases*0.1,y=maxDeaths*0.9,label="HIGH DEATH RATE")+
          ggtitle(paste(loc,curDate, sep=" "))+
          theme(legend.position="none", plot.title=element_text(hjust=0.5))
  
  p2 <- ggplot(data=curData[curData$location==loc,])+
          geom_abline(slope=10, color="#FF0000", size = 1)+
          geom_path(aes(x=new_cases_smoothed,y=new_tests_smoothed, color=as.Date(date, origin = "2019-12-31")), size=1)+
          geom_point(aes(x=new_cases,y=new_tests, color=as.Date(date, origin = "2019-12-31")), size=1)+
          xlab("New Cases")+
          ylab("New Tests")+
          annotate("text", x=maxCases*0.9,y=maxCases*0.9*10,label="POSITIVE RATE = 10%")+
          annotate("text", x=maxCases*0.1,y=maxTests*0.9,label="LOW POSITIVE RATE")+
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
            ylab("")+
            ggtitle("Total (Log Scale)")+
            scale_y_log10()
  
  p4 <- grid.arrange(p1,cases,deaths,p2, tests, bars, layout_matrix=lay)
  
 
  ggsave(paste("./plots/",curDate,".jpg",sep=""), p4, width = 16, heigh=9)
}



ggplot(data=covid[which(covid$location!="World" & covid$date == "2020-03-20"),]%>%, 
       aes(x=1, y=total_deaths, fill=location))+
  geom_bar(stat="identity", width=1, color="black")+
  coord_polar(theta = "y")+
  theme_void()


