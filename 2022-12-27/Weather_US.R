##Libraries
library(dplyr)
library(tidyverse)
library(usmap)
library(ggrepel)
library(stringr)
library(colorspace)
library(showtext)
library(scales)

##Import data
weather_forecasts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/weather_forecasts.csv')
cities <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/cities.csv')
outlook_meanings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/outlook_meanings.csv')

##Clean data
#Delete empty values
weather_forecast <- weather_forecasts %>% drop_na()

#Find the date with coldest temperatures on average
coldest <- weather_forecast %>% 
  group_by(date) %>% 
  summarise(Mean = mean(observed_temp))

coldest_day <- coldest[which(coldest$Mean == min(coldest$Mean)),"date"]

vday <- weather_forecast[which(weather_forecast$date == coldest_day$date),]

#Create columns for high and low temperatures
vday1  <- pivot_wider(vday , names_from = high_or_low, values_from = observed_temp)

#Assign corresponding values of temperatures to them
vday2 <- vday1 %>% 
  group_by(city)  %>% 
  summarise(low = mean(low, na.rm = TRUE), high = mean(high, na.rm = TRUE)) 

#Identify the most common forecast outlook per city
vday3 <- vday1 %>% 
  group_by(city) %>% 
  count(city, forecast_outlook) %>%
  slice(which.max(n), na.rm=FALSE) 

#Merge temperatures and forecast outlook into one dataframe
vday4 <- merge(vday2, vday3, by="city")

#merge with actual outlook meanings
vday_final <-merge(vday4, outlook_meanings, by="forecast_outlook")

#merge with location
cities_longlat <- cities[,c('city','lon','lat')]
vday_final <-merge(vday_final, cities_longlat, by="city")

#Format data to map
usmap_data<- usmap_transform(vday_final)

#Remove insular areas (St Thomas and San Juan)
usmap_data <- usmap_data[!(usmap_data$city == "ST_THOMAS"|usmap_data$city == "SAN_JUAN"),]

#Format text
usmap_data$city <- gsub("_", " ", usmap_data$city)
usmap_data$city <- str_to_title(usmap_data$city)

#Group values to simplify categories
usmap_data[which(usmap_data$meaning == "Mostly Cloudy"),"meaning"] <- "Cloudy"
usmap_data[which(usmap_data$meaning == "Partly Cloudy"),"meaning"] <- "Cloudy"
usmap_data[which(usmap_data$meaning == "Rain Showers"),"meaning"] <- "Rain"
usmap_data[which(usmap_data$meaning == "Freezing Rain"),"meaning"] <- "Rain"
usmap_data[which(usmap_data$meaning == "Snow Flurries"),"meaning"] <- "Snow"
usmap_data[which(usmap_data$meaning == "Snow Showers"),"meaning"] <- "Snow"

##Map the data

#Import font
font_add_google(name = "Roboto")
showtext_auto()

#Create text label for coldest city
min_city <- usmap_data[which(usmap_data$low == min(usmap_data$low)),]
min_city['label'] <- paste0(min_city$city,", ", min_city$low, "°F")

#Create text label for warmest city
max_city <- usmap_data[which(usmap_data$low == max(usmap_data$low)),]
max_city['label'] <- paste0(max_city$city,", ", max_city$low, "°F")

#Plot the map
plot_usmap("states",color = "#E6EAEF",fill = "#4e5f6e",size=0.2) +
  geom_point(data = usmap_data,aes(x, y,shape = meaning, colour = low),size=2)+
  geom_label_repel(data= min_city, aes(x, y,label = label,fill=low),colour = "#4e5f6e",label.size = NA,label.padding = 0.3,label.r =0,size=14,segment.size=0.2,segment.color="white",nudge_x = -2000000,nudge_y = -1000000)+
  geom_label_repel(data= max_city, aes(x, y,label = label,fill=low),colour = "white",label.size = NA,label.padding = 0.3,label.r =0,size=14,segment.size=0.2,segment.color="#ff9d5c",nudge_x = 350000, nudge_y = -245000)+
  scale_colour_gradient2(low = "white",mid = "#5F84A2",high = "#ff9d5c", midpoint = 30, name = "Lowest recorded temperature (°F)") + 
  scale_fill_gradient2(low = "white", mid = "#5F84A2",high = "#ff9d5c",midpoint = 30, name = "Lowest recorded temperature (°F)")+ 
  scale_shape_manual(values = c(21, 17,15,18,8,16,25,4),name = "Weather condition")+ 
  theme(plot.background = element_rect(fill = "#E6EAEF",colour="#E6EAEF"),
        text=element_text(family="Roboto",size =24),
        plot.title = element_text(hjust = 0.25, vjust=5.5,size = 86,face = "bold",colour = "#3e4c58"),
        plot.subtitle = element_text(hjust = -2.5, vjust = 5.5,size = 54, colour = "#3e4c58",lineheight = 0.2),
        legend.position=c(-0.3,-0.1),
        legend.text = element_text(size=38, colour = "#3e4c58"),
        legend.title = element_text(size=38, colour = "#3e4c58", face = "bold",lineheight = 0.2),
        legend.background = element_blank(),
        legend.key=element_rect(fill="#E6EAEF"),
        legend.spacing.x = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.5, 'cm'),
        plot.caption = element_text(colour = "#3e4c58",size=32,hjust=0.9,vjust = -10),
        plot.margin=unit(c(1,0,0.5,5),"cm"))+
  labs(subtitle = "Weather conditions in US cities on February 14, 2021, the average coldest day of that year",caption = "Source: Data from the USA National Weather Service")+
  ggtitle("Cold Valentine")

##Save the map
ggsave("snowy_vday.png", 
       plot = last_plot(),
       device = "png",
       width = 8,
       height = 6, dpi=500)
