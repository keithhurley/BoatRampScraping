### web scrape reservoir levels
#http://www.nwd-mr.usace.army.mil/rcc/nwk/kcbull3.txt - corp of engineers
#https://www.usbr.gov/gp/boat/index.html
#https://www.usbr.gov/gp-bin/boatweb_spec.pl?main%20-%20east

options(stringsAsFactors=FALSE, digits=1)

#load libraries -----
library("tidyverse")
library("maps")
library("rvest")
library("lubridate")
library("broom")
library("readxl")
library("png")
library("grid")

#call this function to get data from bureau sites -----
getData.b<-function(url){
temp<-read_html(url)
nodes<-temp %>%
  html_nodes("p")

m<-list()
m$date<-mdy(substr(nodes[1], 47, 56))
m$ele<-as.numeric(substr(nodes[2], 22, 27))
return(m)
}

#call this function to loop through bureau sites and get all data -----
getData.b.loop<-function(sites.b){
  op<-data.frame(date=character(),ele=numeric(), wb=character())
  for(intX in 1:nrow(sites.b)) {
    tmp<-data.frame(getData.b(sites.b$url[intX]))
    tmp$wb<-sites.b$wb[intX]
    tmp$date=as.character(tmp$date)
    op<-rbind(op,tmp)
  }
  return(op)
}

#this function creates a ribbon to use to create water polygon -----
getPoly<-function(ele) {
  myPoly<-data.frame(x=seq(0, ele), ymin=seq(0, ele), ymax=rep(ele, n=ele))
  return(myPoly)
}

#this function creates a faceted plot for the data passed (a waterbody) -----
plotRamp<-function(myData){

p<-ggplot(data=myData) +
  geom_ribbon(data=getPoly(myData$ele[1]), aes(x=x, ymin=ymin, ymax=ymax), color="lightblue", fill="lightblue") +
  geom_segment(aes(x=r.min, y=r.min, xend=r.bottom, yend=r.bottom), size=0.5, color="black") +
  geom_segment(aes(x=r.top, y=r.top, xend=r.top+2, yend=r.top+2), size=0.5, color="black") +
  geom_segment(aes(x=r.top+2, y=r.top+2, xend=r.max, yend=r.top+2), size=0.5, color="black") +
  geom_segment(aes(x=r.min,y=r.cp, xend=r.cp, yend=r.cp), color="gray60") +
  geom_segment(aes(x=r.bottom, y=r.bottom, xend=r.top, yend=r.top), size=3, color="black") +
  geom_text(aes(x=r.top, y=r.top, label=paste("Top Of Ramp", "(", format(round(r.top,1), nsmall=1)," feet)", sep="")), nudge_x=20, size=2) +
  geom_text(aes(x=r.bottom, y=r.bottom, label=paste("Bottom Of Ramp", " (", format(round(r.bottom,1), nsmall=1)," feet)", sep="")), nudge_x=20, size=2) +
  geom_text(aes(x=r.cp, y=r.cp, label=paste("Conservation Pool", " (", format(round(r.cp,1), nsmall=1)," feet)", sep="")), nudge_x=20, size=2) +
  scale_x_continuous(limits=c(myData$r.min[1], myData$r.max[1])) + scale_y_continuous(limits=c(myData$r.min[1],myData$r.max[1]))+
  labs(title=myData$wb[1]) +
  theme_void() +
  theme(title=element_text(hjust=0.5, face="bold", size=24),
        panel.border=element_rect(color="black", fill=NA),
        strip.background = element_rect(fill="gainsboro", color="black")) +
  facet_wrap(~r.name, ncol=2)

return(p)
}
plotRamp(myData[myData$wb=="Calamus" & !is.na(myData$ele),])

#run code to create plots -----
#build frame of bureau sites
sites.b<-read_excel("./ramps.xlsx", sheet="wb")

#build frame of launch sites
sites.launch<-read_excel("./ramps.xlsx", sheet="ramps")

#get data
myData<-getData.b.loop(sites.b)

#wrangle data
myData<-myData %>%
  right_join(sites.launch, by="wb") %>%
  filter(!is.na(r.name)) %>%
  filter(!is.na(ele)) %>%
  mutate(r.title=paste(r.name, " (", format(ymd(date), format="%d/%m/%Y"), ")", sep=""))

myData$r.max=as.numeric(myData$r.max)
myData$r.min=as.numeric(myData$r.min)

#create plots
for (intX in 1:6) {
  print(plotRamp(myData[myData$wb==sites.b$wb[intX],])
)
}






