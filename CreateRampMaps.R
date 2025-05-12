#script to use to create ramp maps for ramp scraping report
#01/11/2018

options(stringsAsFactors = FALSE, digits=8)

###### load libraries ----
library(tidyverse)
library(sf)
library(stringr)
library(readxl)
library(ggrepel)

####### get shapefile ----
#myBounds=data.frame(minX=params$boundsUL_long, maxX=params$boundsLR_long, minY=params$boundsLR_lat, maxY=params$boundsUL_lat)
contours<-read_sf("contours/Lake_Shoreline_Contours.shp")
contours2<-read_sf("contours/Public_Fishing_Areas.shp")
ramps<-read_excel("./ramps.xlsx", sheet="ramps", col_types = c("guess", "guess", "numeric", "numeric", "text", "text")) 
ramps$r.lat<-as.numeric(ramps$r.lat) #needed otherwise r rounds down decimals to 1 place
ramps$r.long<-as.numeric(ramps$r.long)


# Lake Mac ----------------------------------------------------------------
op<-list()
op$contour<-contours %>% 
  filter(str_detect(Name, "McConaughy"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "McCon"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
             nudge_x=c(-0.01,0.01,-0.015,0.012,
                       0.015,0,-0.05,-0.01,0,-0.01,0,0.07,-0.04,-0.02),
             nudge_y=c(0.01,0.01,0.01,0.01,
                       0.008,0.03,-0.9,-0.01,0.01,-0.3,0.01,0.01,0.02,0.02)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/mcconaughy.png")

# Calamus ----------------------------------------------------------------
op<-list()
op$contour<-contours %>% 
  filter(str_detect(Name, "Calamus"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "Calamus"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
                   nudge_x=c(-0.02,-0.02,0.005,0.015),
                   nudge_y=c(0,0,-0.0075,0)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/calamus.png")

# Merrit ----------------------------------------------------------------
op<-list()
op$contour<-contours %>% 
  filter(str_detect(Name, "Merrit"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "Merrit"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
                   nudge_x=c(0.02,0.015,0.0,0.0, 0.0),
                   nudge_y=c(0,0,-0.02,0.0075,-0.01)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/merritt.png")


# Sherman ----------------------------------------------------------------
op<-list()
op$contour<-contours %>% 
  filter(str_detect(Name, "Sherman"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "Sherman"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
                   nudge_x=c(0.02,0.019,0.01,0.01, 0.01,0.006,-0.002),
                   nudge_y=c(-0.001,0.003,-0.006,-0.0045,0.007,0.005,0.005)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/sherman.png")


# Harlan ----------------------------------------------------------------
op<-list()
op$contour<-contours %>% 
  filter(str_detect(Name, "Harlan"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "Harlan"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
                   nudge_x=c(0.0,0.01,-0.015,-0.01,
                             0.0,-0.02,0.0, 0.0),
                   nudge_y=c(0.005,0.01,0.0,-0.0075,
                             0.008,0.004,-0.01,-0.01)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/harlan.png")


# Winters Creek ----------------------------------------------------------------
op<-list()
op$contour<-contours2 %>% 
  filter(str_detect(GIS_WEBS_3, "Winter"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "Winters"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
                   nudge_x=c(-0.01),
                   nudge_y=c(-0.002)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/winters.png")


#Minatare Creek ----------------------------------------------------------------
op<-list()
op$contour<-contours %>% 
  filter(str_detect(Name, "Minatare"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "Minatare"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
                     nudge_x=c(-0.0005,0.01,0.005,-0.005),
                     nudge_y=c(-0.0015,0.0,-0.0025,0.0)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/minatare.png")

#Elwood ----------------------------------------------------------------
op<-list()
op$contour<-contours2 %>% 
  filter(str_detect(GIS_WEBS_3, "Elwood"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "Elwood"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
                   nudge_x=c(0.00),
                   nudge_y=c(0.005)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/elwood.png")

#Jeffrey ----------------------------------------------------------------
op<-list()
op$contour<-contours2 %>% 
  filter(str_detect(GIS_WEBS_3, "Jeffrey"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "Jeffrey"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
                   nudge_x=c(0.005),
                   nudge_y=c(-0.0055)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/jeffrey.png")


#Johnson ----------------------------------------------------------------
op<-list()
op$contour<-contours %>% 
  filter(str_detect(Name, "Johnson"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "Johnson"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
                     nudge_x=c(-0.005),
                     nudge_y=c(-0.002)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/johnson.png")

#Box Butte ----------------------------------------------------------------
op<-list()
op$contour<-contours %>% 
  filter(str_detect(Name, "Butte"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "Butte"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
                   nudge_x=c(0.005),
                   nudge_y=c(0.002)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/butte.png")

#Enders ----------------------------------------------------------------
op<-list()
op$contour<-contours %>% 
  filter(str_detect(Name, "Enders"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "Enders"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
                   nudge_x=c(0.005),
                   nudge_y=c(0.002)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/enders.png")


#Medicine Creek ----------------------------------------------------------------
op<-list()
op$contour<-contours %>% 
  filter(str_detect(Name, "Medicine"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "Medicine"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
                   nudge_x=c(-0.008, -0.007, 0.001),
                   nudge_y=c(0.002, 0, 0.005)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/medicine.png")


#Red Willow ----------------------------------------------------------------
op<-list()
op$contour<-contours %>% 
  filter(str_detect(Name, "Red Willow"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "Willow"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
                   nudge_x=c(0.0075, 0.0075, 0.002),
                   nudge_y=c(-0.0025, 0.002, -0.0075)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/willow.png")


#Swanson ----------------------------------------------------------------
op<-list()
op$contour<-contours %>% 
  filter(str_detect(Name, "Swanson"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "Swanson"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
                   nudge_x=c(0.0, 0.0075, 0.002, -0.015),
                   nudge_y=c(0.0025, 0.002, -0.0075, -0.0025)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/swanson.png")


#Davis Creel ----------------------------------------------------------------
op<-list()
op$contour<-contours2 %>% 
  filter(str_detect(GIS_WEBS_3, "Davis Creek"))
op$ramps<-ramps %>%
  filter(str_detect(wb, "Davis Creek"))

ggplot() +
  geom_sf(data=op$contour, fill="steelblue2", alpha=.4) +
  geom_point(data=op$ramps, aes(x=r.long, y=r.lat), color="red", size=5) +  
  geom_label_repel(data=op$ramps, aes(x=r.long, y=r.lat, label=r.name), size=4,
                   nudge_x=c(-0.006),
                   nudge_y=c(0.0025)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_sf()
ggsave("./pics/davis.png")
