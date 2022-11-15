#Load packages
library(ggforce)
library('showtext')

#Load data
ally_scores <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/ally_scores.csv')

#Clean data
ally_scores$date <- as.Date(ally_scores$date, "%Y_%m_%d") #Convert to date format

#Create lower and higher date value to create space to plot labels on side
position_text <- as.Date("2022-11-01")
min_plot <- as.Date("2017-04-01")
max_plot <- as.Date("2022-12-01")

#Create a data frame with the annotation labels for categories Low - Medium - High
annotation <- data.frame(
  x = c(min_plot,min_plot,min_plot),
  y = c(25,50,73),
  label = c("LOW\n(0-33%)", "MEDIUM\n(34-66%)", "HIGH\n(67-99%)"))

#Import font
font_add_google(name = "Roboto")
showtext_auto()

#Plot the graph
ggplot(data = ally_scores, aes(x=date, y =p50)) +
  #Add the lines for percentiles
  geom_line(aes( y = p10),color = NA) + 
  geom_line(aes( y = p25),color = NA) + 
  geom_line(aes( y = p75),color = NA) +
  geom_line(aes( y = p90),color = NA) + 
  #Fill areas for percentile
  geom_ribbon(aes(ymin=p10,ymax=p90), fill="#367892", alpha=0.4) + 
  geom_ribbon(aes(ymin=p25,ymax=p75), fill="#367892", alpha=0.5) + 
  #Add the median 
  geom_line( color="#1A3A47", alpha =1,size=0.5)+
  #Add titles
  ggtitle("Distribution of web page accessibility scores", subtitle = 'Lighthouse accessibility scores per month (2017-2022)') + 
  #Add axes, axes titles and source caption
  scale_y_continuous(limits = c(20, 100)) +
  scale_x_date(date_breaks = "years",expand=c(0, .9),date_labels = "%Y")+
  labs(x= "\n ", y = "Accessibility score (%)\n", caption = "Source: Data from httparchive.org. Score categories (Low to High) as suggested by blackboard.com.")+
  #Add two horizontal lines for score categories
  geom_hline(yintercept=66,color = "#5d7079",size = 0.8, alpha=1,linetype="dashed")+
  geom_hline(yintercept=33,color = "#5d7079",size = 0.8, alpha=1,linetype="dashed")+ 
  #Add annotations for percentiles
  geom_text(data = annotation, aes(x=position_text,y=84,label = "Median"),color = "#1A3A47",size=5,hjust = 0.1,family = "Roboto")+
  geom_text(data = annotation, aes(x=position_text,y=63,label = "10th percentile"),color = "#9bbbcb",size=5,hjust = 0.1,family = "Roboto")+
  geom_text(data = annotation, aes(x=position_text,y=74,label = "25th percentile"),color = "#367892",size=5,hjust = 0.1,family = "Roboto")+
  geom_text(data = annotation, aes(x=position_text,y=92,label = "75th percentile"),color = "#367892",size=5,hjust = 0.1,family = "Roboto")+
  geom_text(data = annotation, aes(x=position_text,y=96,label = "90th percentile"),color = "#9bbbcb",size=5,hjust = 0.1,family = "Roboto")+
  #Add annotations for categories
  geom_text(data = annotation, aes(x=x,y=y,label = label),color = "#5d7079",fontface="bold",size=6,hjust=-0.1,family = "Roboto", lineheight=0.3)+
  coord_cartesian(xlim = c(min_plot, max_plot), # This focuses the x-axis on the range of interest
                  clip = 'off') +
  #Style
  theme(panel.background = element_rect(fill = "#E3EAED"),
      plot.background = element_rect(fill = "#E3EAED"),
      text=element_text(size=26,  family="Roboto", colour = "#1A3A47",lineheight = 0),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title.x=element_text(colour = "#1A3A47",size = 20,family = "Roboto"),
      axis.title.y=element_text(colour = "#1A3A47",size = 20,family = "Roboto"),
      axis.text.x = element_text(size=14),
      axis.text.y = element_text(size=14),
      axis.line = element_line(size = 0.1, colour = "#5d7079"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5,size=20),
      plot.caption = element_text(size=12),
      plot.margin = margin(1, 1.8, 0.5, 0.8, "cm")#Customized grids and axes 
  )


#Save the graph
ggsave("website_accessibility_scores.png",
       plot = last_plot(),
       device = "png",
       width = 6,
       height = 4, dpi=300)
