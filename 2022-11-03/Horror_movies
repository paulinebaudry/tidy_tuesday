#Load libraries
install.packages("spiralize")
library("spiralize")
library("tidyverse")
library(lubridate)
library(showtext)

#Read in the TidyTuesday package from CRAN
install.packages('tidytuesdayR')

#Load TidyTuesday data for week 44 on Horror Movies
tuesdata <- tidytuesdayR::tt_load('2022-11-01')
horror_movies <- tuesdata$horror_movies

##CLEAN DATA 
#Keep only data from 1980 onwards
horror_new <- with(horror_movies, horror_movies[(release_date >= "1980-01-01"),])

#Count number of movies released per month of each year
horror_new['count'] <- 1
horror_new_counts <- horror_new %>% 
  group_by(month = lubridate::floor_date(release_date, 'month')) %>%
  summarize(counts = sum(count))


##PLOT THE GRAPH
#Create file to save the plot in
png("Horror_movies.png",width = 1300,height = 1000,res=200)

font_add_google(name = "Poppins") #Import font
showtext_auto()

#Add the title
grid.newpage()
pushViewport(viewport(y = 1, height = unit(0.08, "npc"), just = "top"))
grid.text("Monthly horror movie releases since 1980", gp = gpar(fontsize = 14,fontfamily="Poppins",fontface = "bold"))
popViewport()

#Add the bottom text for data source
pushViewport(viewport(x=0.8,y = 0, height = unit(0.05, "npc"), just = "bottom"))
grid.text("Data source : Horror movies dataset extracted by\nTanya Shapiro from The Movie Database (TMDB)", gp = gpar(fontsize = 5,fontfamily="Poppins"), just = "bottom")
popViewport()

#Make the spiral plot
x <- sort((1:nrow(horror_new_counts))) #create list of number of values to plot
years_labels <-seq(1980,2022, length = 2023-1980) #create list of years for labels

months <- rep(month.abb,43)
months_letter <-  substr(months,1,1) #create list of first letter of each month for labels

pushViewport(viewport(y = 0.99, height = unit(0.85, "npc"), just = "top"))
spiral_initialize(flip = "horizontal", start = 450, scale_by = "curve_length", xlim = c(1,nrow(horror_new_counts)),newpage = FALSE) #Initialize the spiral
spiral_track(height = 0.7, ylim = c(0,110),background=FALSE) #Set the track and hide it
spiral_lines(x,horror_new_counts$counts, area = TRUE,gp = gpar(fill = "#C41E3A",col = NA)) #Plot the line
spiral_axis(h = "bottom", major_at = seq(1,516, by = 12),labels = years_labels,minor_ticks = 0,major_ticks_length = unit(0.25, "cm"),labels_gp = gpar(fontsize = 8,fontfamily="Poppins"),ticks_gp = gpar(col= "grey")) #Add year ticks and labels
spiral_axis(h = "bottom", major_at = seq(1,516), labels = months_letter,minor_ticks =0,major_ticks_length = unit(0.05, "cm"),labels_gp = gpar(fontsize = 4,fontfamily="Poppins"),ticks_gp = gpar(col= "grey")) #Add month ticks and labels
spiral_yaxis(side = "end",labels_gp = gpar(fontsize = 6,fontfamily="Poppins")) #Add y axis label
popViewport()

#Add annotation text
pushViewport(viewport(x=0.18,y = 0.04, height = unit(0.25, "npc"), just = "top"))
grid.text("Halloween 2020 saw the most\nnew releases with 455 horror movies\ncounted for the month of october", gp = gpar(fontsize = 7,fontfamily="Poppins"), just = "bottom")
popViewport()

#Add annotation arrow
pushViewport(viewport(x=0.2,y = 0.05, height = unit(0.25, "npc"), just = "top"))
grid.lines(x = unit(c(0.43, 0.5), "npc"),
           y = unit(c(0.82, 1.08), "npc"),
           gp = gpar(fill="black"),
           arrow = arrow(length = unit(0.1, "inches"), 
                         ends="last", type="open"))
popViewport()

dev.off() #Saves the plot


