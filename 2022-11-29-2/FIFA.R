#Libraries
library(ggforce)
library(showtext)


#Load data
wcmatches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv')

wcmatches[which(wcmatches$stage == "Final Round"),"stage"] <-"Final"
wcmatches[which(wcmatches$stage == "Final"),"stage"] <-"Finals"
wcmatches[which(wcmatches$winning_team == "West Germany"),"winning_team"] <-"Germany"
wcmatches[which(wcmatches$winning_team == "Czechoslovakia"),"winning_team"] <-"Czechoslovakia (former)"
wcmatches[which(wcmatches$winning_team == "Yugoslavia"),"winning_team"] <-"Yugoslavia (former)"
wcmatches[which(wcmatches$winning_team == "Soviet Union"),"winning_team"] <-"Soviet Union (former)"


quarterfinals_matches <- wcmatches[which(wcmatches$stage == "Quarterfinals"),]
semifinals_matches <- wcmatches[which(wcmatches$stage == "Semifinals"),]
finals_matches <- wcmatches[which(wcmatches$stage == "Finals"),]

quarter_semi_final <- rbind(quarterfinals_matches,semifinals_matches,finals_matches)

#keep only relevant columns
quarter_semi_final <- quarter_semi_final[,c("winning_team","stage")]

#Clean rows
quarter_semi_final[which(quarter_semi_final$winning_team == "Portagul"),"winning_team"] <- "Portugal"
quarter_semi_final<- na.omit(quarter_semi_final) 


quarter_semi_final <- quarter_semi_final %>% 
  group_by(winning_team,stage) %>% 
  summarise(count = n())


quarter_semi_final <- gather_set_data(quarter_semi_final, c(2,1))

#Change names to ensure correct order when plotted
quarter_semi_final[which(quarter_semi_final$y == "Finals"),"y"] <- "  Finals"
quarter_semi_final[which(quarter_semi_final$y == "Semifinals"),"y"] <- " Semifinals"
#Convert to factor
quarter_semi_final$y <- as.factor(quarter_semi_final$y)

font_add_google(name = "Montserrat")

total_final <- nrow(quarter_semi_final[quarter_semi_final$stage == "Finals",])
total_semi <- nrow(quarter_semi_final[quarter_semi_final$stage == "Semifinals",])
total_quarter <- nrow(quarter_semi_final[quarter_semi_final$stage == "Quarterfinals",])


ggplot(quarter_semi_final, aes(x, id = id, split = y,value =count))+
  geom_parallel_sets(aes(fill = stage), alpha = 0.7,axis.width = 0.02,show.legend = FALSE,sep=0.02) +
  geom_parallel_sets_axes(axis.width =0.01,sep=0.02,fill = "white") +
  geom_parallel_sets_labels(colour= "white", angle = 0, hjust = 1,nudge_x = -0.01,nudge_y = 0.3,sep=0.02,size=5,family = "Montserrat")+
  scale_fill_manual(values=c("#00254d", "grey", "#daaa04"))+
  ggtitle("Winning football teams of final matches \nin FIFA world cups (1934-2018)")+
  labs(caption="Source: Data from FIFA World Cup")+
  theme(plot.background = element_rect(fill = "#567d46"),
        panel.background = element_rect(fill = "#567d46"),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        text=element_text(size =12),
        plot.title = element_text(hjust = 0.5,vjust = -1,family = "Montserrat",face = "bold",size=32,color = "white", lineheight = 0.3),
        plot.caption = element_text(hjust = 1,color = "white",family = "Montserrat",size=16))+ 
  expand_limits(x = c(0.8,2.1))+
  annotate("text", x = 2.03, y = 109, label = total_final,color= "white",family = "Montserrat",size=5)+
  annotate("text", x = 2.03, y = 73, label = total_semi,color= "white",family = "Montserrat",size=5)+
  annotate("text", x = 2.03, y = 8, label = total_quarter,color= "white",family = "Montserrat",size=5)

#Save map
ggsave("fifa.png", 
       plot = last_plot(),
       device = "png",
       width = 5,
       height =4, dpi=300)
