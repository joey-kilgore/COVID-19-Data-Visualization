# Load up data https://github.com/owid/covid-19-data/tree/master/public/data
covid<- read.csv(file ="owid-covid-data.csv", TRUE)
head(covid)

# check unique locations
unique(covid$location)

# Gathering US covid and basic graphs
date = covid[covid$location=="United States",]$date
totalCases = covid[covid$location=="United States",]$total_cases
totalDeaths = date = covid[covid$location=="United States",]$total_deaths
totalTests = date = covid[covid$location=="United States",]$total_tests

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
curDate <- "2021-08-07"
mycol <- c("red", "orange", "yellow", "green", "blue", "navy", "violet")
#for(curDate in unique(covid$date)){
  curData <- covid[as.Date(covid$date, origin="2019-12-31")<=as.Date(curDate, origin = "2019-12-31"),]
  
  maxCases <- max(curData[curData$location==loc,]$new_cases, na.rm=TRUE)
  maxDeaths <- max(curData[curData$location==loc,]$new_deaths, na.rm=TRUE)
  maxTests <- max(curData[curData$location==loc,]$new_tests, na.rm=TRUE)
  
  p1 <- ggplot(data=curData[curData$location==loc,])+
          geom_abline(slope=0.02, color="#FF0000", size = 1)+
          geom_path(aes(x=new_cases_smoothed,y=new_deaths_smoothed, color=as.Date(date, origin = "2019-12-31")), size=1)+
          geom_point(aes(x=new_cases,y=new_deaths, color=as.Date(date, origin = "2019-12-31")), size=1)+
          xlab("New Cases")+
          ylab("New Deaths")+
          annotate("text", x=maxCases*0.8,y=maxCases*0.8*0.02,label="DEATH RATE = 2%")+
          annotate("text", x=maxCases*0.1,y=maxDeaths*0.9,label="HIGH DEATH RATE")+
          ggtitle(paste(loc,curDate, sep=" "))+
          theme(legend.position="none", plot.title=element_text(hjust=0.5))+
          scale_color_gradientn(colours=mycol)
  
  p2 <- ggplot(data=curData[curData$location==loc,])+
          geom_abline(slope=10, color="#FF0000", size = 1)+
          geom_path(aes(x=new_cases_smoothed,y=new_tests_smoothed, color=as.Date(date, origin = "2019-12-31")), size=1)+
          geom_point(aes(x=new_cases,y=new_tests, color=as.Date(date, origin = "2019-12-31")), size=1)+
          xlab("New Cases")+
          ylab("New Tests")+
          annotate("text", x=maxCases*0.9,y=maxCases*0.9*10,label="POSITIVE RATE = 10%")+
          annotate("text", x=maxCases*0.1,y=maxTests*0.9,label="LOW POSITIVE RATE")+
          theme(legend.position="none")+
          scale_color_gradientn(colours=mycol)
  
  
  cases <- ggplot(data=curData[curData$location==loc,])+
          geom_path(aes(x=as.Date(date),y=new_cases_smoothed, color=as.Date(date, origin = "2019-12-31")), size=1)+
          geom_point(aes(x=as.Date(date),y=new_cases, color=as.Date(date, origin = "2019-12-31")), size=1)+
          theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
          xlab("")+
          ylab("")+
          ggtitle("New Cases")+
          scale_x_date(date_labels = "%Y-%m-%d")+
          scale_color_gradientn(colours=mycol)
  

  deaths <- ggplot(data=curData[curData$location==loc,])+
          geom_path(aes(x=as.Date(date),y=new_deaths_smoothed, color=as.Date(date, origin = "2019-12-31")), size=1)+
          geom_point(aes(x=as.Date(date),y=new_deaths, color=as.Date(date, origin = "2019-12-31")), size=1)+
          theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
          xlab("")+
          ylab("")+
          ggtitle("New Deaths")+
          scale_x_date(date_labels = "%Y-%m-%d")+
          geom_hline(yintercept=1660, size=1, linetype="dashed", color="red")+
          annotate("text", x=as.Date("2020-03-30"),y=3000,label="1,660 die everyday from cancer")+
          scale_color_gradientn(colours=mycol)
  
  tests <- ggplot(data=curData[curData$location==loc,])+
          geom_path(aes(x=as.Date(date),y=new_tests_smoothed, color=as.Date(date, origin = "2019-12-31")), size=1)+
          geom_point(aes(x=as.Date(date),y=new_tests, color=as.Date(date, origin = "2019-12-31")), size=1)+
          theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
          xlab("")+
          ylab("")+
          ggtitle("New Tests")+
          scale_x_date(date_labels = "%Y-%m-%d")+
          scale_color_gradientn(colours=mycol)
  
  
  today <- curData[as.Date(curData$date, origin="2019-12-31")==as.Date(curDate, origin="2019-12-31"),]
  todayLoc <- today[today$location==loc,]
  totalCases <- todayLoc$total_cases
  totalDeaths <- todayLoc$total_deaths
  totalTests <- todayLoc$total_tests
  
  deathPer1000 <- todayLoc$total_deaths_per_million/1000
  casesPer1000 <- todayLoc$total_cases_per_million/1000
  testsPer1000 <- todayLoc$total_tests_per_thousand
  vaccinesPer1000 <- todayLoc$total_vaccinations_per_hundred*10
  totalVaccinesPer1000 <- todayLoc$people_fully_vaccinated_per_hundred*10
  totals <- c(testsPer1000,casesPer1000,deathPer1000,vaccinesPer1000,totalVaccinesPer1000)
  totalsLabs <- c("Tests","Cases","Deaths","Vaccinated","Fully Vaccinated")
  
  totalData <- data.frame(x=totalsLabs, y=totals)
  bars <- ggplot(totalData, aes(x=factor(totalData$x, c("Tests","Cases","Deaths","Vaccinated","Fully Vaccinated")),y))+
            geom_bar(stat="identity")+
            theme(legend.position="none", plot.title = element_text(hjust = 0.5))+
            xlab("")+
            ylab("")+
            ggtitle("Totals (Per 1000)")+
            scale_y_continuous(breaks=c(0,500, 1000))+
            geom_text(aes(label = signif(totalData$y, digits = 4)), nudge_y=150, color="black")
  
  p4 <- grid.arrange(p1,cases,deaths,p2, tests, bars, layout_matrix=lay)
  
  show(p4)
  ggsave(paste("./plots/",loc,"_",curDate,".jpg",sep=""), p4, width = 16, heigh=9)
#}



#ggplot(data=covid[which(covid$location!="World" & covid$date == "2020-03-20"),]%>%, 
#       aes(x=1, y=total_deaths, fill=location))+
#  geom_bar(stat="identity", width=1, color="black")+
#  coord_polar(theta = "y")+
#  theme_void()

#shifted <- covid[as.Date(covid$date, origin="2019-12-31")>as.Date("2020-01-10", origin="2019-12-31"),]
#shiftedLoc <- shifted[shifted$location==loc,]
#newDeathsSmooth <- shiftedLoc$new_deaths_smoothed

#newCasesSmooth <- covid[covid$location==loc,]$new_cases_smoothed
#days <- seq(from=1, to=length(newDeathsSmooth), by=1)
#newCasesSmooth <- newCasesSmooth[days]
#plot(newCasesSmooth,newDeathsSmooth)
#ggplot()+
#  geom_abline(slope=0.05, color="#FF0000", size = 1)+
#  geom_path(aes(x=newCasesSmooth,y=newDeathsSmooth), size=1)+
#  #geom_point(aes(x=new_cases,y=new_deaths, color=as.Date(date, origin = "2019-12-31")), size=1)+
#  xlab("New Cases")+
#  ylab("New Deaths")+
#  annotate("text", x=maxCases*0.8,y=maxCases*0.8*0.05,label="DEATH RATE = 5%")+
#  annotate("text", x=maxCases*0.1,y=maxDeaths*0.8,label="HIGH DEATH RATE")+
#  ggtitle("Shifted 14 day death rate")+
#  theme(legend.position="none", plot.title=element_text(hjust=0.5))
