require("dplyr")
require("stringr")
require("rvest")
require("lubridate")
require("readxl")


#main function to get BOR data
getBorData<-function(myUrls=c("https://www.usbr.gov/gp-bin/arcweb_cane.pl")){
  
  #get data
  op<-data.frame("name"= character(), "elevation"=numeric(), "updatedDate"=Date())
  for(intX in 1:length(myUrls)) {
    temp<-read_html(myUrls[[intX]])
    
    m<-list()
    m$name<-temp %>% html_nodes("div h3") %>% html_text() %>% str_split_i(pattern="for ", i=2) %>% str_split_i(pattern=",", i=1)
    m$elevation<-m$elevation <- as.numeric((temp %>% html_nodes("p"))[[2]] %>% html_text() %>% substr(19, 24))
    m$updatedDate<-mdy(temp %>% html_node(".t_title") %>% html_text() %>% str_split_i("as of ", i=2)) 
    #control for different page layouts
    if(is.na(m$updatedDate)){
      m$updatedDate<- mdy(temp %>% html_nodes("div h3") %>% html_text() %>% str_split_i(pattern="as of ", i=2) %>% str_split_i(pattern=",", i=1))
    }
    
    #control for missing date
    # if (is.na(m$updatedDate) || length(m$updatedDate)==0) {
    #   m$date<-"Date Unknown"
    # }
    op<-bind_rows(op,data.frame(m))
  }
  
  return(op)

}

#scrape CPPD data
getCppdData<-function(url="https://cnppid.com/lake-river-data"){
  m_html<-read_html(url)
  myDate<-lubridate::as_date(m_html %>% html_node(".subtitle") %>% html_text() %>% gsub("Readings for ", "", .) %>% parse_date_time(orders="%A, %B %d, %Y"))
  elevations<-data.frame(c(name=NULL, elevation=NULL))
  
  for(intX in 1:length(m_html %>%
                       html_node("table") %>%
                       html_nodes(., "tr"))-1)  {
  elevations<-rbind(elevations,data.frame(
      name=m_html %>%
             html_node(paste("tr:nth-child(",intX,") td:nth-child(1)", sep="")) %>%
             html_text(),
      elevation=as.numeric(m_html %>%
        html_node(paste("tr:nth-child(",intX,") td:nth-child(2)", sep="")) %>%
        html_text %>%
             substr(1,7) %>% gsub(",", "", .))
  )
  )
  }
  
  return(elevations %>%
           mutate(updatedDate=myDate) %>%
           filter(!is.na(name)))
}


getAllData<-function(){
  
  myData_Cppd<-getCppdData()
  myData_Bor<-getBorData(wb$url[wb$source==1 & !is.na(wb$url)])
  allData<-bind_rows(myData_Cppd, myData_Bor)
}


#this function creates a ribbon to use to create water polygon -----
getPoly<-function(ele) {
  myPoly<-data.frame(x=seq(0, ele), ymin=seq(0, ele), ymax=rep(ele, n=ele))
  return(myPoly)
}

#this function creates a faceted plot for the data passed (a waterbody) -----
plotRamp<-function(myData){
  
  #get standard bottom and top
  myBottom<-min(myData$r.bottom) - 3
  myTop<-max(myData$r.top)+2
  
  #get factor to use in placing labels
  #first adjust for different scales
  myLabelNudgeScaleFactor<-myBottom/myTop 
  
  
  # #now adjust for number of facets
  # if(nrow(myData) > 1) {
  #   myLabelNudgeScaleFactor<-1 * myLabelNudgeScaleFactor
  # } else {
  #   myLabelNudgeScaleFactor<-0.5  * myLabelNudgeScaleFactor
  # }
  
  myData$oos_flag<-factor(as.numeric(myData$oos_flag))
  
  p<-ggplot(data=myData) +
    geom_ribbon(data=getPoly(myData$ele[1]), aes(x=x, ymin=ymin, ymax=ymax), color="lightblue", fill="lightblue") +
    geom_segment(aes(x=myBottom, y=myBottom, xend=r.bottom, yend=r.bottom), size=0.5, color="black") +
    geom_segment(aes(x=r.top, y=r.top, xend=r.top+2, yend=r.top+2), size=0.5, color="black") +
    geom_segment(aes(x=r.top+2, y=r.top+2, xend=myTop+20, yend=r.top+2), size=0.5, color="black") +
    geom_segment(aes(x=myBottom,y=r.cp, xend=r.cp, yend=r.cp), color="gray60") +
    geom_segment(aes(x=r.bottom, y=r.bottom, xend=r.top, yend=r.top), size=3, color="black") +
    geom_text(aes(x=r.top, y=r.top, label=paste("Top Of Ramp", "(", format(round(r.top,1), nsmall=1)," feet)", sep="")), nudge_y=myLabelNudgeScaleFactor*6, nudge_x=myLabelNudgeScaleFactor*14, size=4) +
    geom_text(aes(x=r.bottom, y=r.bottom, label=paste("Bottom Of Ramp", " (", format(round(r.bottom,1), nsmall=1)," feet)", sep="")), nudge_x=myLabelNudgeScaleFactor*18, size=4) +
    geom_text(aes(x=r.cp, y=r.cp, label=paste("Conservation Pool", " (", format(round(r.cp,1), nsmall=1)," feet)", sep="")), nudge_x=myLabelNudgeScaleFactor*5, nudge_y=myLabelNudgeScaleFactor*-1, size=4, hjust=0, color="gray60") +
    geom_text(aes(x=myBottom, y=ele, label= paste(format(round(ele,1), nsmall=1)," feet", sep="")), nudge_x=myLabelNudgeScaleFactor*2, hjust=0, size=4, color="navy") +
    geom_text(aes(x=((myTop+22+myBottom)/2),y=((myTop + 6 + myBottom)/2), label=oos), size=10, hjust=0.5, vjust=0.5, color="red", angle=-18) +
    geom_rect(aes(xmin=myBottom, xmax=myTop+22, ymin=myBottom, ymax=myTop+6, alpha=oos_flag), fill="gray20") +
    scale_x_continuous(limits=c(myBottom, myTop +22)) + scale_y_continuous(limits=c(myBottom, myTop +6))+
    #scale_alpha_manual(limits=c(0,1), values=c(0,0.3), guide=FALSE) +
    scale_alpha_manual(limits = factor(c(0, 1)), values = c(0, 0.3), guide = FALSE)+
    theme_void() +
    labs(title=paste(myData$name[[1]], " - ", myData$r.name[[1]])) +
    theme(panel.background=element_rect(color="NA", fill=NA),
          strip.background = element_rect(fill="gainsboro", color="black"),
          strip.text=element_text(size=24),
          plot.title=element_text(size=20, face="bold", hjust=0.5)) #+
    #facet_wrap(~r.name, ncol=2)
  
  return(p)
}


wb<-read_excel("./ramps.xlsx", sheet="wb")
ramps<-read_excel("./ramps.xlsx", sheet="ramps") %>%
  filter(!is.na(r.bottom))
sources<-read_excel("./ramps.xlsx", sheet="sources")

#this gets rid of any waterbodys with no ramps
wb<-wb %>%
  right_join(ramps, by="wb") %>%
  select(wb, url, source, con.pool, max.ele, min.ele, pic, wbCode) %>%
  filter(!is.na(wb)) %>%
  unique() %>%
  arrange(wb)%>% 
  arrange(wb) %>%
  filter(!is.na(url)) 
#gets rid of ramps with no source data for elevation
ramps<-ramps %>% 
  left_join(wb[,c("wb","url")], by="wb") %>%
  filter(!is.na(url)) %>%
  select(-url)

#get all data
myData<-getAllData()
#myData$updatedDate[is.na(myData$updatedDate)]<-"Date Unknown"

myData<-myData %>%
  right_join(ramps, by=c("name"="wb")) %>%
  left_join(wb, by=c("name"="wb")) %>%
  filter(!is.na(r.name)) %>%
  filter(!is.na(elevation)) %>%
  mutate(r.title=paste(r.name, " (", updatedDate, ")", sep="")) %>%
  mutate(ele=as.numeric(elevation),
         r.max=as.numeric(max.ele),
         r.min=as.numeric(min.ele)) %>%
  rename(r.cp=con.pool) %>%
  mutate(oos_flag=(r.bottom + 2 > elevation) | (r.top<elevation)) %>%
  mutate(oos="") %>%
  mutate(oos=replace(oos, oos_flag, "Out Of Service")) %>%
  left_join(sources, by=c("source"="Code")) %>%
  select(-source)





