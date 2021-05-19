install.packages("ggplot2")
install.packages("ggplot")
install.packages("fredr")
library(tidyr)
library(fredr)
library(ggplot2)
library(knitr)
#SETTING UP
fredr_set_key("bf2bcfa30bca51b9d589467640fc2ff3")
fredr_series_search_text(
  search_text = "unemployment",
  limit = 100L
)

#Importing
ur <- fredr(
  series_id = "UNRATE",
  observation_start = as.Date("2020-01-01")
)
View(ur)

pcenofood <- fredr(
  series_id = "PCEPILFE",
  observation_start = as.Date("2020-01-01")
)
View(ur)

pceindex <- fredr(
  series_id = "PCEPI",
  observation_start = as.Date("2020-01-01")
)
View(pceindex)

psr <- fredr(
  series_id = "PSAVERT",
  observation_start = as.Date("2020-01-01")
)
View(psr)

pce <- fredr(
  series_id = "PCEC96", #REALPCE
  observation_start = as.Date("2020-01-01")
)

#playing around with data
ggplot(data = pce)
unlist(pce)
View(psr)
ur <- ur[-c(15),]
tst <- cbind(ur, psr, pce)
View(tst)
tst <- tst[,1:3]
ur2 <- ur[,1:3]
ur2 <- cbind(ur[1], ur[3])
View(ur2)
names(ur2)[2] <- "UR"
View(ur2)

psr2 <- psr[,1:3]
psr2 <- cbind(psr[1], psr[3])
View(psr2)
names(psr2)[2] <- "PSR"

pce2 <- pce[,1:3]
pce2 <- cbind(pce[1], pce[3])
View(pce)
names(pce2)[2] <- "PCE"

#Setting up data
library(data.table)
(setattr(tst, "row.names", c("Jan 2020", "Feb 2020", "Mar 2020", "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020", "Jan 2021")))
View(tst)
tst2 <- tst[,-1]
View(tst2)

final_df <- as.data.frame(t(tst))
final_df
View(final_df)

View(psr2)
tst10 <- cbind(psr2, ur2)
fm()
View(tst10)

View
dfnopce$id = rownames(dfnopcee)
View(dfnopce)

#Prepping data
library(reshape2)    
mm = melt(dfnopce, id='id')

#Line graph of PSR and UR over time
library(ggplot2)
psrurgr <- ggplot(mm)+geom_line(aes(x=variable, y=value, group=id, color=id)) + theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=0)) + xlab("Month") + ylab("Rate") + ggtitle("Unemployment Rate and Personal Saving Rate Over Time")
psrurgr
library(ggplot2)
psrurgr2 <- ggplot(data = tst12, aes(x = d, y=PSR)) + geom_line(color = "red") #+ theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=0)) + xlab("Month") + ylab("Rate") + ggtitle("Unemployment Rate and Personal Saving Rate Over Time")
psrurgr2
rownames(tst10) <- 1:13
colnames(tst10) <- c("d", "PSR", "UR")
tst11 <- tst10[, -3]
View(tst10)
View(tst11)
##FIX ----
df <- data.frame(date = c(as.Date("2020-01-01"),
                          as.Date("2020-02-01"),
                          as.Date("2020-03-01")),
                 PSR = c(20, 23.2, 25.1),
                 UR = c(10, 20, 21))
ggplot(data = df, aes(x = date, y = PSR)) + geom_line()
str(tst11$PSR)
tst12 <- tst10[-c(14), ]
View(tst12)
tst12
library(reshape2)
mm = melt(tst12, id = "d")
View(mm)
psrurgr3 <- ggplot(data = mm, aes(x=d, y=value, color=variable)) + geom_line() + theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=0)) + xlab("Month") + ylab("Rate") + ggtitle("Unemployment Rate and Personal Saving Rate Over Time")
psrurgr3
library(plotly)
psrurgrf <- ggplotly(psrurgr3)
dfnopce <- final_df[-c(2), ]
View(dfnopce)

View(dfnopce)

View(pcenofood)

install.packages("ggpubr")
library("ggpubr")

View(mm)
#Attempted a scatter plot... looks weird
ggscatter(mm, x = "variable", y = "value", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
View(dfnopce)

#Attempting correlation test
res <- cor.test(tst2$UR, tst2$PSR, 
                method = "pearson")
res
----------------
  
#---NEED TO FIX THIS-----
##-------ADDING PCE PERCENT CHANGE-----
pcepercent <- fredr(
  series_id = "DPCCRAM1M225NBEA",
  observation_start = as.Date("2020-01-01")
)
View(pcepercent)
pcepercent <- pcepercent[,-1]
(setattr(pcepercent, "row.names", c("Jan 2020", "Feb 2020", "Mar 2020", "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020", "Jan 2021")))
View(pcepercent)
names(pcepercent)[2] <- "PCE PercentChng"
pcepercent$realtime_end <- c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01")
tst14 <- tst14[, -5]
tst3 <- tst2
tst3 <- tst3[,-2]
tst3
tst4 <- cbind(tst3, pcepercent)
View(tst3)
View(tst14)
colnames(pcepercent) <- c("PCE", "d")
tst14 <- cbind(tst12, pcepercent)
wpce = melt(tst14, id = "d")
View(wpce)
psrurgr3 <- ggplot(data = mm, aes(x=d, y=value, color=variable)) + geom_line() + theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=0)) + xlab("Month") + ylab("Rate") + ggtitle("Unemployment Rate and Personal Saving Rate Over Time")
psrurgr3

pcefinaldf <- as.data.frame(t(tst4))
View(pcefinaldf)

pcefinaldf$id = rownames(pcefinaldf)
wpce = melt(pcefinaldf, id='id')
View(wpce)
wpcegr <- ggplot(wpce)+geom_line(aes(x=d, y=value, color=variable)) + xlab("Month") + ylab("Rate") + ggtitle("PSR, UR, PCE (as percent change) Over Time") + theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=0))
wpcegr
##----ADDING PCE CATEGORIES----
realpce <- fredr(
  series_id = "PCEC96",
  observation_start = as.Date("2020-01-01")
)
realpce <- (setattr(realpce, "row.names", c("Jan 2020", "Feb 2020", "Mar 2020", "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020", "Jan 2021")))
realpce <- realpce[,-3]
names(realpce)[3] <- "RealPCE"
View(realpce)

realpceeegs <- fredr( #realpce energy goods and services
    series_id = "DNRGRX1M020SBEA",
    observation_start = as.Date("2020-01-01")
  )
realpceeegs <- (setattr(realpceeegs, "row.names", c("Jan 2020", "Feb 2020", "Mar 2020", "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020", "Jan 2021")))
realpceeegs <- realpceeegs[,-3]
names(realpceeegs)[3] <- "PCEEnergy"
View(realpceeegs)

realpcegoods <- fredr( #general goods
  series_id = "DGDSRX1",
  observation_start = as.Date("2020-01-01")
)
realpcegoods <- (setattr(realpcegoods, "row.names", c("Jan 2020", "Feb 2020", "Mar 2020", "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020", "Jan 2021")))
realpcegoods <- realpcegoods[,-3]
names(realpcegoods)[3] <- "PCEGenGoods"
View(realpcegoods)

realpcenondur <- fredr( #nondurable goods
  series_id = "PCENDC96",
  observation_start = as.Date("2020-01-01")
)
realpcenondur <- (setattr(realpcenondur, "row.names", c("Jan 2020", "Feb 2020", "Mar 2020", "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020", "Jan 2021")))
realpcenondur <- realpcenondur[,-3]
names(realpcenondur)[3] <- "PCENondurableGoods"
View(realpcenondur)

realpcesrv <- fredr( #services
  series_id = "PCESC96",
  observation_start = as.Date("2020-01-01")
)
realpcesrv <- (setattr(realpcesrv, "row.names", c("Jan 2020", "Feb 2020", "Mar 2020", "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020", "Jan 2021")))
realpcesrv <- realpcesrv[,-3]
names(realpcesrv)[2] <- "PCEServices"
View(realpcesrv)

realpcedur <- fredr( #durable goods
  series_id = "PCEDGC96",
  observation_start = as.Date("2020-01-01")
)
realpcedur <- (setattr(realpcedur, "row.names", c("Jan 2020", "Feb 2020", "Mar 2020", "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020", "Jan 2021")))
realpcedur <- realpcedur[,-3]
names(realpcedur)[3] <- "PCEDurableGoods"
View(realpcedur)

realpcefd <- fredr( #food
  series_id = "DFXARX1M020SBEA",
  observation_start = as.Date("2020-01-01")
)
realpcefd <- (setattr(realpcefd, "row.names", c("Jan 2020", "Feb 2020", "Mar 2020", "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020", "Dec 2020", "Jan 2021")))
realpcefd <- realpcefd[,-3]
names(realpcefd)[3] <- "PCEFood"
View(realpcefd)
View(realpce2)
realpce2 <- realpce
realpcedur2 <- realpcedur
realpceeegs2 <-realpceeegs
realpcefd2 <-realpcefd
realpcegoods2 <- realpcegoods
realpcenondur2 <- realpcenondur
realpcesrv2 <- realpcesrv

library(reshape2)
combinedpces2 <- cbind(realpce2, realpcedur2, realpceeegs2, realpcefd2, realpcegoods2, realpcenondur2, realpcesrv2)
View(combinedpces2)

combpcesdf2 <- combpcesdf2[, -2]
View(combpcesdf)
View(t)

combpcesdf2 <- combpcesdf

combpcesdf2$id = rownames(combpcesdf2)
View(combpcesdf2)
library(plotly)
library(reshape2)    
meltedpce <- melt(combinedpces2, id='date')
View(meltedpce)
#Comparing PCEs of each category
pcecats <- ggplot(meltedpce)+geom_line(aes(x=date,y = value, color=variable)) + 
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=0)) + xlab("Month") + 
  ylab("Index") + ggtitle("Various Personal Consumption Expenditure categories over time")
pcecats
----------------
##---ADDING COVID-19----
covid <- national_history_1_
rm(national_history_1_) #data from https://covidtracking.com/data/download
covid <- (setattr(covid, "col.names", c("date", "death", "deathIncrease", "incluCumulative", "incluCurrently", "hospitalizedInc", "hospitalizedCurr", "hospitalizedCumul", "negative", "negativeIncrease", "onVentilarCumul", "onVentilatorCurr", "positive", "positiveInc", "states", "totTestResults", "totTestResInc")))
View(covid)
colnames(covid) <- c("date", "death", "deathIncrease", "incluCumulative", "incluCurrently", "hospitalizedInc", "hospitalizedCurr", "hospitalizedCumul", "negative", "negativeIncrease", "onVentilarCumul", "onVentilatorCurr", "positive", "positiveInc", "states", "totTestResults", "totTestResInc")
covid2 <- covid[-c(1),]
covid <- covid2
rm(covid2)
View(covid)
covid$positiveInc <- as.numeric(covid$positiveInc)
covid$date2 <- as.Date(covid$date, origin = "2020-01-13")
#how to plot better and rearrange this data set
covidtestdf <- aggregate(covid["positiveInc"], by=covid["date2"], sum)
View(covidtestdf)
library(data.table)
DT<- data.table(covidtestdf) 
DT[,sum(positiveInc),by = date2]
View(DT)
library(dplyr)
install.packages("lubridate")
library(lubridate)
DT <- DT %>% group_by(month=floor_date(date2, "month")) %>%
  summarize(positiveInc=sum(positiveInc))
View(DT)
library(scales)
aggposinc <- ggplot(DT, aes(x=month, y = positiveInc)) + geom_line() + scale_y_continuous(labels = comma) + xlab("Month") + ylab("Positive Cases") + ggtitle("Positive COVID-19 Tests Over Time")
posicgr <- ggplot(covid) + geom_line(aes(x=date2, y = positiveInc, group = 2)) + scale_y_continuous(labels = comma) + xlab("Month") + ylab("Positive Cases") + ggtitle("Positive COVID-19 Tests Over Time")
library(plotly)
aggposinc

#data from https://ourworldindata.org/grapher/weekly-growth-covid-cases?tab=chart
# colnames(covidpercent) <- c("entity", "code", "day", "weekly case growth")
# covidpercent <- covidpercent[-c(1),]
# covidpercent <- covidpercent[,-1]
# View(covidpercent)
# str(covidpercent)
# dfcovperc <- as.data.frame(covidpercent)
# str(dfcovperc)
# str(dfcovperc$`weekly case growth`)
# str(dfcovperc$day)
# covidpercent$`weekly case growth` <- as.numeric(covidpercent$`weekly case growth`)
# dfcovperc$`weekly case growth` <- as.vector(dfcovperc$`weekly case growth`)
# dfcovperc$day <- as.vector(dfcovperc$day)
# names(dfcovperc)[1] <- "day"
# ggplot(dfcovperc) + geom_line(aes(x=day2, y = growth2)) + ylim(-10, 10)
# dfcovperc$day2 <- as.Date(dfcovperc$day, origin = "2020-02-04")
# dfcovperc$growth2 <- dfcovperc$growth/10000
# dfcovperc$growth2 <- as.numeric(dfcovperc$growth2)
# View(dfcovperc)

#how to plot this better?
summary(dfcovperc)
#how to rearrange this data set

#data from US Census: likelihood of receiving a COVID-19 vaccine
lklyvacc <- export
rm(export)
View(lklyvacc)
colnames(lklyvacc) <- c("week", "area", "totalPopOver18", "measureUniverse", "number", "marginOfError", "percent", "percentMargOfErr")
lklyvacc <- lklyvacc[-c(1),]
lklyvacc$region <- tolower(lklyvacc$area)
library(ggplot2)
install.packages("maps")
library(maps)
install.packages("mapproj")
library(mapproj)
states <- map_data("state")
lklyvacc$percent <- as.numeric(lklyvacc$percent)
map.df <- as.data.frame(lklyvacc)
map.df$percent <- as.numeric(map.df$percent)
map.df <- merge(states,lklyvacc, by="region", all.x=T)
map.df <- map.df[order(map.df$order),]
#MAP OF USA
w <- ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=percent))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map() + ggtitle("Percentage of people who said they are likely to receive a vaccine by state")
w
View(map.df)

#data from US Census: received a COVID-19 vaccine
rcvdvacc <- export_2_
rm(export_2_)
View(rcvdvacc)
rcvdvacc <- rcvdvacc[-c(269),]
colnames(rcvdvacc) <- c("week", "area", "totalPopOver18", "measureUniverse", "number", "marginOfError", "percent", "percentMargOfErr")
rcvdvacc <- rcvdvacc[,-9]
rcvdvacc$region <- tolower(rcvdvacc$area)
map.df2 <- as.data.frame(rcvdvacc)
map.df2 <- merge(states,rcvdvacc, by="region", all.x=T)
map.df2$percent <- as.numeric(map.df2$percent)
map.df2$number <- as.numeric(map.df2$number)
map.df2 <- map.df2[order(map.df2$order),]
#MAP OF USA
rcvacgr <- ggplot(map.df2, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=percent))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map() + ggtitle("Percentage of people who received COVID-19 vaccine by state")
rcvacgr
rcvdvacc <- rcvdvacc[-c(1),]
rcvdvacc <- 
lklyvacc <- lklyvacc[,-9]
combvacc <- rbind(lklyvacc, rcvdvacc)
View(combvacc)
cv = melt(combvacc, id='region.x')
#ggplot(cv)+geom_bar(aes(x=variable, y=value, group=id, color=id))
View(cv)
str(covid)
str(dfcovperc)

library(htmlwidgets)
library(htmltools)
library(leaflet)
library(geojsonio)


# map3$week <- revalue(map3$week, c("25" = "17-Feb-2021"))
# str(map3)
# map3 <- map.df2
# View(map3)
# map3$start <- do.call("as.Date", lis)
map3$end <- map3$start + 90
#
# # use geojsonio to convert our data.frame
# #  to GeoJSON which timeline expects
# map3_geo <- geojson_json(map3,lat="Latitude",long="Longitude")
#
# leaf <- leaflet() %>% addTiles()
#
# leaf$dependencies[[length(leaf$dependencies)+1]] <- htmlDependency(
#   name = "leaflet-timeline",
#   version = "1.0.0",
#   src = c("href" = "http://skeate.github.io/Leaflet.timeline/"),
#   script = "javascripts/leaflet.timeline.js",
#   stylesheet = "stylesheets/leaflet.timeline.css"
# )
# #
#  leaf %>%
#    setView(44.0665,23.74667,2) %>%
#    onRender(sprintf(
#      '
#  function(el,x){
#      var map3_data = %s;
# 
#      var timeline = L.timeline(map3_data, {
#        pointToLayer: function(percent, latlng){
#          var hue_min = 120;
#          var hue_max = 0;
#          var hue = hue_min;
#          return L.circleMarker(latlng, {
#            radius: 10,
#            color: "hsl("+hue+", 100%%, 50%%)",
#            fillColor: "hsl("+hue+", 100%%, 50%%)"
#          });
#        },
#        steps: 1000,
#        duration: 10000,
#        showTicks: true
#      });
#      timeline.addTo(HTMLWidgets.find(".leaflet"));
#  }
#      ',
#  map3_geo
#    ))

install.packages("plotly")
library(plotly)
library(ggplot2)
library(readr)

#---NEED TO FIX THIS-----
View(pceindex)
pceindex$date <- as.Date(pceindex$date)
str(pceindex)
subz <- pceindex
View(subz)
subz <- pceindex[(4),]
pceplot <- ggplot(pceindex, aes(x = date, y = value)) +
  xlab("Date") +
  ylab("PCE") +
  theme_minimal(base_size = 14, base_family = "Georgia") + geom_point() + geom_line() + geom_point(data = subz, color = "red") + geom_text(data=subz, label="Stimulus Check Distribution", vjust=0.5) + ggtitle("PCE over time") #geom_text(data = pceindex, aes(x = date, label = "my label")) 
pceplot
interactivepce <- ggplotly(pceplot)
print(interactivepce) #stimulus checks went out in april 2020
#-----------

posplotly <- ggplot(covid, aes(x=date2, y = positiveInc, group = 2)) +  xlab("Date") +
  ylab("Positive Increase") + theme_minimal(base_size = 14, base_family = "Georgia") + geom_line()
interactivepos <- ggplotly(posplotly)
print(interactivepos)

install.packages("devtools")
install.packages("cli")
install.packages("glue")
library("cli")
library(devtools)
devtools::install_version("plotly", version = "4.5.6", 
                          repos = "http://cran.us.r-project.org")

comppcen <- ggplot(meltedpce, aes(x=variable,y = value, group=id, color=id)) + xlab("Date") +  ylab("PCE") + theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=0)) + geom_point() + geom_line() 
interactivecomppce <- ggplotly(comppcen) 
print(interactivecomppce)
##X AXIS SPACING FIXED
#----------------------

View(cv)

engdot <- realpceeegs2
engdot <- engdot[c(4),]
View(engdot)
energypce <- ggplot(realpceeegs2, aes(x = date, y = PCEEnergy)) +  xlab("Date") +
  ylab("Energy PCE") + theme_minimal(base_size = 14, base_family = "Georgia") + geom_line() + geom_point() + geom_text(data=engdot, label="Stimulus Check Distribution", vjust=0.5) + geom_point(data = engdot, color = "red")
energypce
interactiveenergy <- ggplotly(energypce + ggtitle("Energy PCE over time"))
print(interactiveenergy)

#NEED TO FIC THIS-----------
library(plotly)
View(covid2)
View(realenggr2)
dtcov <- DT[-c(15), ]
colnames(dtcov) <- c("date", "positiveInc")
realenggr1 <- cbind(dtcov, realpceeegs2)
realenggr2 <- melt(realenggr1, id = "date")  
x2 <- melt(x, id = "month")  
require(scales)
subz2 <- realenggr2[c(18),]
View(subz2)
test <- ggplot(realenggr2, aes(x=date,y = value, color=variable)) + xlab("Date") + theme_minimal(base_size = 14, base_family = "Georgia") + geom_point() + geom_line() + theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=0)) + facet_wrap(~variable,scales = "free_y") + scale_y_continuous(labels = comma) + geom_point(data = subz2, color = "red") + geom_text(data=subz2, label="Stimulus Check Distribution", vjust=0.5)
test
testinteractive <- ggplotly(test + xlab("\nMonth") + theme(axis.title.x = element_text(vjust= -5)) + theme(axis.title.x = element_text(hjust= -5)) + ylab("Value") + ggtitle("Energy PCE vs COVID-19 rates over time"))
testinteractive
#-----------------------------
#how to fix plot

#2d maps that change over time
#how to put explanatory details
#DID
#county level data?

##CONTINUING
library(gganimate) ##gganimate ##gifski


str(map.df)
map.df$week <- as.numeric(map.df$week)
pinter <- ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=percent))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map() + transition_reveal(along = week) + 
  labs(caption = 'Week: {round(frame_along)}') + ggtitle("People likely to receive a COVID-19 vaccine over time")
pinter
anim_save("likely.gif")

map.df2$week <- as.numeric(map.df2$week)
p2 <- ggplot(map.df2, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=percent))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map() + transition_reveal(along = week) + 
  labs(caption = 'Week: {round(frame_along)}') + ggtitle("People who have received a COVID-19 vaccine over time")

View(map.df)
map.df2$week <- as.numeric(map.df$week)
animate(p2, height = 715, width = 600)   
p2
anim_save("rcvd.gif")
library(png)

View(realenggr2)
###CONTINUING ----
scatt <- ggplot(realenggr2, aes(x=date,y = value, color=variable)) + xlab("Date") + theme_minimal(base_size = 14, base_family = "Georgia") + geom_point()+ theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=0)) + scale_y_continuous(labels = comma) + facet_wrap(~variable,scales = "free_y") 
scatt
View(hh2)
hh <- rbind(DT3, pce)
psr$id = c("psr", "psr", "psr", "psr", "psr", "psr", "psr", "psr", "psr", "psr", "psr", "psr", "psr")
library(reshape2)
qq2 <- melt(qq)
qq <- rbind(DT3, psr)

psrcovdt <-cbind(dtcov, psr2)
psrcovdt2 <- melt(psrcovdt, id = "date")
scatt2 <- ggplot(psrcovdt2, aes(x=date,y = value, color= variable)) + xlab("Date") + theme_minimal(base_size = 14, base_family = "Georgia") + geom_point()+ theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=0)) + scale_y_continuous(labels = comma) + facet_wrap(~variable,scales = "free_y") 
scatt2
cor(DT3$value, psr$value)

View(pce2)
View(psr2)
dtcov <- DT[-c(15), ]
colnames(dtcov) <- c("date", "positiveInc")
pcescatt1 <- cbind(dtcov, pce2)
pcescatt2 <- melt(pcescatt1, id = "date")  
scatt3 <- ggplot(pcescatt2, aes(x=date,y = value, color=variable)) + xlab("Date") + theme_minimal(base_size = 14, base_family = "Georgia") + geom_point()+ theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=0)) + scale_y_continuous(labels = comma) + facet_wrap(~variable,scales = "free_y") 
scatt3
str(pce0)
View(pcescatt02)
str(DT03$positiveInc)
hh <- rbind(DT3, pce)
psr$id = c("psr", "psr", "psr", "psr", "psr", "psr", "psr", "psr", "psr", "psr", "psr", "psr", "psr")
library(reshape2)
qq2 <- melt(qq)
qq <- rbind(DT3, psr)
View(psr0)

View(DT0)
View(us_counties)
#COUNTY DATA--------
library(raster)
library(leaflet)
library(remotes)
install_github("skeate/Leaflet.timeline")

library(viridis)

# Get USA polygon data
library(dplyr)

usshapefile <- "cb_2014_us_county_5m/cb_2014_us_county_5m.shp"
library(oldtmaptools)
usgeo <- read_shape(file=usshapefile, as.sf = TRUE)
library(tmap)
qtm(usgeo)
str(usgeo$NAME)
usgeo$NAME <- as.character(usgeo$NAME)
str(us_counties$county)
usgeo <- usgeo[order(usgeo$NAME),]
us_counties <- us_counties[order(us_counties$county),]
identical(usgeo$NAME,us_counties$county )
map <- append_data(usgeo, us_counties, key.shp = "NAME", key.data="County")

View(usgeo)



### Create a color palette
library(leaflet)
mypal <- colorNumeric(palette = "viridis", domain = us_counties$cases)

leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  setView(lat = 39.8283, lng = -98.5795, zoom = 4) %>%
  addPolygons(data = usgeo, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.3,
              fillColor = ~mypal(us_counties$cases),
              popup = paste("Region: ", usgeo$NAME, "<br>",
                            "Avg_yield: ", us_counties$cases, "<br>")) %>%
  addLegend(position = "bottomleft", pal = mypal, values = us_counties$cases,
            title = "Avg_yield",
            opacity = 1)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
}

shinyApp(ui, server)

# library(shiny)
# library(xts)
# library(leaflet)
library(dplyr)
# 
# date1<-seq(as.Date("2020-01-21"), as.Date("2020-04-17"), by="day")
# date1 <- as.data.frame(date1)
# str(date1)
# class(date1$date1)
# a<-xts(1:88, order.by = as.POSIXct(date1$date))
# dfT = data.frame(Lat = rnorm(1)+10, Long = rnorm(1),Id=a)
# 
# data_a<-data.frame(us_counties)
# data_a1<-data_a %>%  
#   mutate("Lat" =as.numeric(dfT[1,1]),"Long"=as.numeric(dfT[2,1]),"Date"=rownames(data_a))            
# 
# ui <- fluidPage(
#   sliderInput("time", "date",min(date1$date), 
#               max(date1$date),
#               value = max(date1$date),
#               step=1,
#               animate=T),
#   leafletOutput("mymap")
# )
# 
# server <- function(input, output, session) {
#   points <- reactive({
#     data_a1 %>% 
#       filter(Date==input$time)
#   })
# 
#   output$mymap <- renderLeaflet({
#     leaflet() %>%
#       addMarkers(data = points(),popup=as.character(points()$a))
#   })
# }
# 
# mutate(date1, date = as.Date(as.numeric(date), 
#                                      origin = "2020-01-21"))
# 
# 
# shinyApp(ui, server)

# library(devtools)
# library(tidyverse)
# library(urbnmapr)
# library(ggplot2)
# testUrb <- ggplot() + 
#   geom_polygon(data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
#                fill = "grey", color = "white") +
#   coord_map(projection = "albers", lat0 = 39, lat1 = 45)
# testUrb
# 
# household_data <- left_join(countydata, counties, by = "county_fips") 
# 
# household_data %>%
#   ggplot(aes(long, lat, group = group, fill = medhhincome)) +
#   geom_polygon(color = NA) +
#   coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
#   labs(fill = "Median Household Income")
# 
# counties_sf <- get_urbn_map("counties", sf = TRUE)
# 
# counties_sf %>% 
#   ggplot(aes()) +
#   geom_sf(fill = "grey", color = "#ffffff")
# 
# spatial_data <- left_join(get_urbn_map(map = "counties", sf = TRUE),
#                           countydata,
#                           by = "county_fips")
# ggplot() +
#   geom_sf(spatial_data,
#           mapping = aes(fill = us_counties$cases),
#           color = "#ffffff", size = 0.25) +
#   labs(fill = "covid cases")
# 
# library(gifski) ##gganimate ##gifski
# 
# View(map.df)
# countyanim <- ggplot(us_counties, aes(x=long,y=lat,group=group))+
#   geom_polygon(aes(fill=percent))+
#   geom_path()+ 
#   scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
#   coord_map() + transition_reveal(along = week) + 
#   labs(caption = 'Week: {round(frame_along)}') + ggtitle("People likely to receive a COVID-19 vaccine over time")
# countyanim
# 
# map.df2$week <- as.numeric(map.df$week)
# animate(p2, height = 715, width = 600)   
# library(png)



Covid$date_infected <- as.Date(Covid$date_infected, "%Y.%m.%d")



p <- counties_cov %>%
  ggplot() +
  geom_sf(mapping = aes(fill = cases), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  coord_sf(datum = NA) +   
  scale_fill_gradient(name = "Cases", trans = "log", low='green', high='red',
                      na.value = "white",
                      breaks=c(1, max(counties_cov$cases))) +
  theme_bw() + 
  theme(legend.position="bottom", 
        panel.border = element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())

a2 <- p + transition_time(date_infected) + 
  labs(title='Confirmed COVID-19 Cases: {frame_time}')

str(Covid$date_infected)
as.Date(Covid$date_infected, "%Y.%m.%d")

animate(a2, nframes = 24, device = "png", renderer = file_renderer("C:/mypath", prefix = "gganim_plot", overwrite = TRUE))
#----

library(urbnmapr) # For map
library(ggplot2)  # For map
library(dplyr)    # For summarizing
library(tidyr)    # For reshaping
library(stringr)  # For padding leading zeros
library(ggrepel)
library(ggmap)
library(usmap)
library(gganimate)
library(magrittr)
library(gifski)
library(urbnmapr)
library(gganimate)
devtools::install_github("thomasp85/transformr")
library(transformr)
library(tweenr)
url <- "https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv"
COV <- read.csv(url, stringsAsFactors = FALSE)
str(COV)
Covid <- pivot_longer(COV, cols=starts_with("X"),
                      values_to="cases",
                      names_to=c("X","date_infected"),
                      names_sep="X") %>%
  mutate(infected = as.Date(date_infected, format="%m.%d.%Y"),
         countyFIPS = str_pad(as.character(countyFIPS), 5, pad="0"))

states_sf <- get_urbn_map(map = "states", sf = TRUE)
counties_sf <- get_urbn_map(map = "counties", sf = TRUE)

counties_cov <- inner_join(counties_sf, group_by(Covid, countyFIPS) %>%
                             summarise(cases=sum(cases)), by=c("county_fips"="countyFIPS"))
countycovmap <- counties_cov %>%
  ggplot() +
  geom_sf(mapping = aes(fill = cases), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  coord_sf(datum = NA) +   
  scale_fill_gradient(name = "Cases", trans = "log", low='yellow', high='red', 
                      na.value="white", breaks=c(1, max(counties_cov$cases))) +
  theme_bw() + theme(legend.position="bottom", panel.border = element_blank()) + ggtitle("Cumulative COVID-19 cases by county")


counties_cov <- inner_join(counties_sf, Covid, by=c("county_fips"="countyFIPS"))

countycovmap 

counties_cov$date_infected <- as.Date(counties_cov$date_infected, "%Y.%m.%d")
str(counties_cov$date_infected)
str(counties_covSmall)

library(lubridate)
counties_covSmall <- counties_cov[1:100000, ]



counties_covAgg <- counties_cov
View(DT)
library(dplyr)
install.packages("lubridate")
library(lubridate)
require(tidyverse)
counties_covAgg <- counties_covAgg %>% group_by(week = week(date_infected), county_fips) %>% 
  summarize(cases=sum(cases))
View(counties_covAgg)
library(dplyr)

require(tidyverse)
counties_covAgg$date_infected <- as.Date(counties_covAgg$date_infected, "%Y.%m.%d")
counties_covAgg <- counties_covAgg %>% group_by(week_infected = week(date_infected), county_fips) %>% 
  summarize(weekly_cases=sum(cases))

p <- ggplot(counties_covAgg) +
  geom_sf(mapping = aes(fill = weekly_cases, geometry = geometry), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "black", size = 0.25) +
  coord_sf(datum = NA) +   
  scale_fill_gradient(name = "Cases", trans = "log", low='yellow', high='red', 
                      na.value="white", breaks=c(1, max(counties_covAgg$weekly_cases))) +
  theme_bw() + theme(legend.position="bottom", panel.border = element_blank()) + ggtitle("Cumulative COVID-19 cases by county over time")

p <- p + transition_time(week_infected) +
  labs(caption = ('Week: {round(frame_time, 1)}'))
str(counties_covSmall)
View(counties_covSmall)

animate(p, height = 715, width = 600)
anim_save("county3.gif")

View(Covid)


##regression and DID------- #ASK
psrcovdt3 <- psrcovdt[, -3]
psrcovdt4 <- psrcovdt3
View(psrcovdt4)
psrcovdt4$PCE <- pcescatt1[, 4]
View(psrcovdt5)
cor(pcescatt1$PCE, psrcovdt$PSR)
View(psrcovdt5)
psrcovdt5 <- psrcovdt4[, -1]
View(pcescatt2)
View(pcepercent)
regT <- lm(PCE ~ PSR * positiveInc, data = psrcovdt5)
summary(regT)
View(psrcovdt)
View(pcescatt1)
View(psrcovdt5)

psrcovdt5 <- psrcovdt4[, -1]
library(olsrr)
reg2 <- lm(PCE ~., data = psrcovdt5) 
stepall <- ols_step_all_possible(reg2)
ols_step_best_subset(reg2)
summary(stepall)
stepall

regCP <- lm(log(PCE) ~ PSR + positiveInc, data = psrcovdt5)
summary(regCP)

anova(regT, regCP)

#DID
#pre-stimmy data: 
pce08 <- fredr(
  series_id = "PCEC96", #REALPCE
  observation_start = as.Date("2007-12-01"),
  observation_end = as.Date("2009-02-01")
)
pce20 <-  fredr(
  series_id = "PCEC96", #REALPCE
  observation_start = as.Date("2019-12-01"),
  observation_end = as.Date("2021-02-01")
)
View(pce20)
pce

counties_covSmall <- as.data.frame(counties_cov[1:1000, ])
View(counties_covSmall)

counties_covSmall <- apply(counties_covSmall,2,as.character)
write.csv(counties_covSmall, "/Users/rubysamim/testcdc", row.names = FALSE)



