#Load libraries
library('tidyverse')
library('showtext')

#Read in the TidyTuesday package from CRAN
install.packages('tidytuesdayR')

#Load TidyTuesday data for week 43 of the Great British Bakeoff
tuesdata <- tidytuesdayR::tt_load('2022-10-25')
gbbo_challenges <-tuesdata$challenges

##CLEAN DATA : Extract top 10 most used words in recipes of contestants

#Create one column containing all the words from both signature and showstopper dishes
gbbo_challenges['recipe']<- paste(gbbo_challenges$signature, gbbo_challenges$showstopper)

#Remove words “and”, "&", "in" and “with” from columns
gbbo_challenges['recipe'] <- sub('and |& |in |with ',' ',gbbo_challenges$recipe)

#Remove characters“,”, "'" and “ " ” from columns
gbbo_challenges['recipe'] <- sub(", |' ",' ',gbbo_challenges$recipe)
gbbo_challenges['recipe'] <- sub('"',' ',gbbo_challenges$recipe)

#Remove NAs in result
gbbo_challenges <- gbbo_challenges %>% drop_na(result)

#Extract top 10 most used ingredients overall
freq <- sort(table(unlist(strsplit(gbbo_challenges$recipe, " "))), decreasing = TRUE)
top_freq <- data.frame(freq)
top_freq <- top_freq[c(4,5,7,9,10,11,12,15,17,19),]

#Group “IN” and “OUT” recipes
gbbo_IN <- gbbo_challenges[-c(which(gbbo_challenges$result == 'OUT')),]
gbbo_OUT <- gbbo_challenges[gbbo_challenges$result == 'OUT',]

#Create a frequency table of all words used in recipes
freq_IN <- sort(table(unlist(strsplit(gbbo_IN$recipe, " "))), decreasing = TRUE)
freq_OUT <- sort(table(unlist(strsplit(gbbo_OUT$recipe, " "))), decreasing = TRUE)

#Keep 10 first variables that correspond to ingredients in IN and OUT data
top_IN <- data.frame(freq_IN)
top_OUT <- data.frame(freq_OUT)

#Merge tables together
top_freq <- merge(top_freq,top_IN,by="Var1")
top_freq <- merge(top_freq,top_OUT,by="Var1")

#Rename columns
colnames(top_freq)[2] <- "Freq_total"
colnames(top_freq)[3] <- "Freq_in"
colnames(top_freq)[4] <- "Freq_out"

#Add levels to ensure values from high to low in bar chart
top_freq$Var1 <- factor(top_freq$Var1, levels = top_freq$Var1[order(top_freq$Freq_total, decreasing = TRUE)])


##PLOT THE GRAPH : Make a horizontal barplot

#Add column and rows to get "in" and "out" variables
top_freq <- pivot_longer(top_freq,Freq_in:Freq_out,names_to = "Result", values_to = "Frequency")
top_freq['Pctg'] <- (top_freq$Frequency)

#Count number of bakes associated for "in" and "out" contestants
in_recipes_count <- colSums(!is.na(gbbo_IN))['signature'][[1]] + colSums(!is.na(gbbo_IN))['showstopper'][[1]]
out_recipes_count <- colSums(!is.na(gbbo_OUT))['signature'][[1]] + colSums(!is.na(gbbo_OUT))['showstopper'][[1]]

#Make the "out" values negative to get a bidirectional bar plot and calculate percentages of ingredients appearing in a bake 
top_freq <- top_freq %>% mutate(Pctg = ifelse(Result == 'Freq_out',
                                              Frequency/out_recipes_count,
                                              -1*Frequency/in_recipes_count))

top_freq <- top_freq %>% mutate(Frequency = ifelse(Result == 'Freq_in',
                                              Frequency/in_recipes_count*100,
                                              Frequency/out_recipes_count*100))

#Remove decimals and add % sign
top_freq$Frequency <- paste0(as.matrix(round(top_freq$Frequency,0)), '%')

#Import fonts
font_add_google(name = "Montserrat")
font_add_google(name = "DM Serif Display")
font_add_google(name = "Merriweather")
showtext_auto()

#Plot the graph    
ggplot(top_freq, aes(x = Pctg, y = reorder(Var1,Freq_total), fill = Result)) + 
  geom_bar(stat = "identity", position = 'stack')+
  geom_text(aes(label = paste0(" ",Frequency, " ")), hjust = "inward", size = 6,colour = 'white', family = "Montserrat")+
  ggtitle('Recipe for success \nin the Great British Bake Off',subtitle = 'Proportion of baked goods containing the top 10 \nmost used ingredients')+
  xlab("")+
  ylab("")+
  scale_fill_manual(name = "",values=c("#A9D39E","#EADDCA"),labels=c("Baked goods of contestants who won \nor made it to the next round","Baked goods of disqualified contestants"))+
  labs(caption = "Source: bakeoff R Package")+
  theme(text=element_text(size=24,  family="DM Serif Display", colour = "#5A5A5A", lineheight = 0.3), #Font
        panel.background = element_rect(fill = '#FFFBF1', color = '#FFFBF1'), plot.background = element_rect(fill = '#FFFBF1'), #Background color
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.major.y = element_line(size=0.5, color="grey"),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(), #Customized grids and axes 
        plot.margin = margin(1, 1.8, 0.5, 1, "cm"),
        legend.text=element_text(size=16,family = "Montserrat", margin = margin(0,0,0,0)),legend.position = 'bottom', legend.spacing.x = unit(0.2, 'cm'),legend.background = element_rect(fill = '#FFFBF1'), #Legend
        plot.caption.position = "plot", plot.caption = element_text(hjust = 1,family = "Montserrat",size = 12,margin = margin(30,0,0,0)),#Bottom caption
        plot.title.position = "plot",plot.title = element_text(hjust = 0.55, family = "DM Serif Display",size = 48, color = "black",margin = margin(0,0,10,0)),plot.subtitle = element_text(hjust = 0.55,family = "Montserrat", color = 'black',margin = margin(0,0,10,0))) #Title

#Save the graph
ggsave("bakeoff3.png",
       plot = last_plot(),
       device = "png",
       width = 5,
       height = 4, dpi=300)
