#Libraries
library(geojsonio)  
library(rgdal) 
library(ggplot2)
library(dplyr)
library(sf)
library(showtext)
library(ggsflabel)
library(colorspace)

#Load data
museums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv')

#Remove row with faulty coordinates
museums <- museums[!(museums$Latitude >90),]

uk <- readOGR("https://github.com/paulinebaudry/tidy_tuesday/blob/main/2022-11-29/NUTS1_Jan_2018_SGCB_in_the_UK.shp")
uk_sf <- st_as_sf(uk)

#Clean year column
museums$Year_opened <- substr(museums$Year_opened, 1,4)
museums$Year_opened <- as.numeric(museums$Year_opened)

#Keep only regions
museums['cleaned']<- sub('.', '',museums$Admin_area)
museums['cleaned']<- gsub("\\(English Region)/.*","",museums$cleaned)
museums['cleaned']<- gsub(".*England/","",museums$cleaned)
museums['cleaned']<- gsub("\\/.*","",museums$cleaned)

#Change columns to match names of shapefile
museums[which(museums$cleaned == "North East "),"cleaned"] <- "North East (England)"
museums[which(museums$cleaned == "North West "),"cleaned"] <- "North West (England)"
museums[which(museums$cleaned == "East Midlands "),"cleaned"] <- "East Midlands (England)"
museums[which(museums$cleaned == "West Midlands "),"cleaned"] <- "West Midlands (England)"
museums[which(museums$cleaned == "South East "),"cleaned"] <- "South East (England)"
museums[which(museums$cleaned == "South West "),"cleaned"] <- "South West (England)"
museums[which(museums$cleaned == "Yorkshire and The Humber "),"cleaned"] <- "Yorkshire and The Humber"
museums[which(museums$cleaned == "London "),"cleaned"] <- "London"
museums[which(museums$cleaned == "East of England "),"cleaned"] <- "East of England"

#Create a new dataframe where each row is a region, with columns for total counts, and counts of museums opened before 1900
new_museums <- museums %>% 
  group_by(cleaned) %>% 
  summarise(total_count = n(), count_old = sum(Year_opened < 1900))

#Merge two dataframes
merged_uk <- left_join(uk_sf,
                       new_museums,
                       by = c("nuts118nm" = "cleaned"),keep = TRUE) 

#Calculate percentage of museums that opened before 1900 out of total number of museums per region
merged_uk['old_pct'] <- ( merged_uk$count_old * 100 ) / merged_uk$total_count

#Add column names for labels
merged_uk['regions']<- gsub(" \\(England).*","",merged_uk$nuts118nm)

#Import font
font_add_google(name = "Source Sans Pro")
showtext_auto()

#Set color for map
base_color <- "#785f44"

color_points<- c(
  lighten(base_color, 0.5),
  base_color,
  darken(base_color, 0.8)
)

color_palette <-colorRampPalette(color_points)
colors_map <-color_palette(20)

#Plot the map
merged_uk%>% 
  ggplot() +
  aes(fill = `old_pct`)+
  geom_sf(color = '#ded6c2', size = 0.1)+
  ggtitle("Museum ages in the United Kingdom")+
  #geom_sf_label(data=merged_uk, mapping = aes(x = long, y = lat, label = `regions`), colour = "white",size=5,  label.size = 0.25,  label.padding = unit(0.25, "lines"))+
   geom_sf_label_repel(aes(label = `regions`),
                     force = 20, nudge_y = 1,seed=10,colour = "white",size=6,label.padding = 0.15,box.padding=0.15)+
  theme(
    text=element_text(size =20,  family="Source Sans Pro", colour = "#18191A", lineheight = 0.3),
    plot.title = element_text(hjust = -0.05, vjust = -2, size = 48,face = "bold"),
    plot.subtitle = element_text(hjust = -0.1, vjust = -5.1, size = 24),
    axis.title = element_blank(), 
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#E8DED1"),
    panel.background = element_rect(fill = "#E8DED1"),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_blank(),
    legend.title.align=-0.2,
    legend.text = element_text(size=18),
    plot.caption = element_text(hjust = 2.3),
    plot.margin = margin(0.2,0.2,0.2,0.2, "cm")) +
    labs(subtitle = "Proportion of museums in each region that opened before the 20th century",caption = "Source: Data from the Mapping Museums project")+
    scale_fill_gradientn(colours = colors_map, name = "Percentage of museums\nthat opened before 1900")

#Save map
ggsave("uk_museums.png", 
       plot = last_plot(),
       device = "png",
       width = 3.78,
       height = 5.2, dpi=300)
