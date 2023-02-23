##Import libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

##Import TidyTuesday data
age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')

##Clean data

#Get heterosexual couples
hetero_couples <- age_gaps[age_gaps$character_1_gender != age_gaps$character_2_gender,]

#Get men in heterosexual couples 
hetero_men_old <- hetero_couples[hetero_couples$character_1_gender == "man",]
hetero_men_old['Age'] <- hetero_men_old$actor_1_age
hetero_men_young <- hetero_couples[hetero_couples$character_2_gender == "man",]
hetero_men_young['Age'] <- hetero_men_young$actor_2_age
hetero_men <- rbind(hetero_men_old, hetero_men_young)

#Get women in heterosexual couples 
hetero_women_old <- hetero_couples[hetero_couples$character_1_gender == "woman",]
hetero_women_old['Age'] <- hetero_women_old$actor_1_age
hetero_women_young <- hetero_couples[hetero_couples$character_2_gender == "woman",]
hetero_women_young['Age'] <- hetero_women_young$actor_2_age
hetero_women <- rbind(hetero_women_old, hetero_women_young)

#Calculate average age of men and women in heterosexual couples per year
hetero_men_avg <- hetero_men %>% group_by(release_year) %>% summarise(avg_age_men = mean(Age))
hetero_men_avg$avg_age_men <- round(hetero_men_avg$avg_age_men,digit=0)
hetero_women_avg <- hetero_women %>% group_by(release_year) %>% summarise(avg_age_women = mean(Age))
hetero_women_avg$avg_age_women <- round(hetero_women_avg$avg_age_women,digit=0)
hetero_avg <- merge(hetero_men_avg , hetero_women_avg, by="release_year", all.x=TRUE)

#Calculate average age of men who are the oldest in same-sex couples per year
gay_couples <- age_gaps[age_gaps$character_1_gender == age_gaps$character_2_gender,]
gay_men_old <- gay_couples[gay_couples$character_1_gender == "man",]
gay_men_old['Age'] <- gay_men_old$actor_1_age
gay_men_old_avg <- gay_men_old %>% group_by(release_year) %>% summarise(avg_age_men_old = mean(Age))
gay_men_old_avg$avg_age_men_old <- round(gay_men_old_avg$avg_age_men_old, digit=0)

#Calculate average age of men who are the youngest in same-sex couples per year
gay_men_young <- gay_couples[gay_couples$character_2_gender == "man",]
gay_men_young['Age'] <- gay_men_young$actor_2_age
gay_men_young_avg <- gay_men_young %>% group_by(release_year) %>% summarise(avg_age_men_young = mean(Age))
gay_men_young_avg$avg_age_men_young <- round(gay_men_young_avg$avg_age_men_young, digit=0)

gay_men_avg <- merge(gay_men_old_avg , gay_men_young_avg, by="release_year", all.x=TRUE)

#Calculate average age of women who are the oldest in same-sex couples per year
gay_women_old <- gay_couples[gay_couples$character_1_gender == "woman",]
gay_women_old['Age'] <- gay_women_old$actor_1_age
gay_women_old_avg <- gay_women_old %>% group_by(release_year) %>% summarise(avg_age_women_old = mean(Age))
gay_women_old_avg$avg_age_women_old <- round(gay_women_old_avg$avg_age_women_old, digit=0)

#Calculate average age of women who are the youngest in same-sex couples per year
gay_women_young <- gay_couples[gay_couples$character_2_gender == "woman",]
gay_women_young['Age'] <- gay_women_young$actor_2_age
gay_women_young_avg <- gay_women_young %>% group_by(release_year) %>% summarise(avg_age_women_young = mean(Age))
gay_women_young_avg$avg_age_women_young <- round(gay_women_young_avg$avg_age_women_young, digit=0)

gay_women_avg <- merge(gay_women_old_avg , gay_women_young_avg, by="release_year", all.x=TRUE)

library('showtext')
font_add_google(name = "Limelight")
font_add_google(name = "Roboto")
showtext_auto()

##Visualize data
colors <- c("Men" = "#CD663D", "Women" = "#eeb400")

hetero_plot<- ggplot()+
  geom_segment(data = hetero_avg, aes(x = release_year, y = avg_age_men, xend = release_year, yend = avg_age_women), colour = "#3a3a3c",size=0.3)+ #Plot line between each partner
   geom_point(data = hetero_men_avg, aes(x = release_year, y = avg_age_men,colour="Men"),size=2)+ #Plot points for heterosexual men
  geom_point(data = hetero_women_avg, aes(x = release_year, y = avg_age_women,colour="Women"),size=2)+ #Plot points for heterosexual women
            labs(x = "Year of movie release", y ="Age of actors", subtitle = "Heterosexual couples",colour=NULL)+
      scale_color_manual(values = colors)+
  scale_x_continuous(breaks = seq(1935,2022,10))+
     ylim(20,81)+
            theme(text=element_text(size=12,  family="Roboto", colour = "#3a3a3c"),
              panel.background = element_rect(fill = "#d1d2d4", colour = "#d1d2d4",size = 0.5, linetype = "solid"),
                                       panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
                                       panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
                    plot.background = element_rect(fill = "#d1d2d4",colour = "white", size=3),
              legend.key = element_rect(fill = "#d1d2d4"),
              legend.background = element_rect(fill = "#d1d2d4"),
                    axis.title.x = element_text(color="#3a3a3c", size = 12,vjust = -0.2),
                    axis.title.y = element_text(color="#3a3a3c", size = 12,vjust = 0.5),
                plot.subtitle= element_text(color="#3a3a3c", size = 14, family = "Roboto",face = "bold",vjust = 0.5))
                                                                               

men_plot <- ggplot()+
  geom_segment(data = gay_men_avg, aes(x = release_year+0.5 , y = avg_age_men_old, xend = release_year+0.5, yend = avg_age_men_young), colour = "#3a3a3c",size=0.3)+ #Plot line between each partner
   geom_point(data = gay_men_old_avg, aes(x = release_year +0.5 , y = avg_age_men_old),colour="#CD663D",size=2)+ #Plot points for homosexual men (oldest)
  geom_point(data = gay_men_young_avg , aes(x = release_year +0.5 , y = avg_age_men_young),colour="#CD663D",size=2)+ #Plot points for homosexual men (youngest)
  labs(x = "Year of movie release", y ="Age of actors",subtitle = "Men in same-sex couples")+
  theme(text=element_text(size=12,  family="Roboto", colour = "#3a3a3c"),
        panel.background = element_rect(fill = "#d1d2d4", colour = "#d1d2d4",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        plot.background = element_rect(fill = "#d1d2d4",colour = "white", size=3),
        axis.title.x = element_text(color="#3a3a3c", size = 12,vjust = -0.2),
        axis.title.y = element_text(color="#3a3a3c", size = 12,vjust = 0.5),
        plot.subtitle= element_text(color="#3a3a3c", size = 14, family = "Roboto",face= "bold",vjust = 0.5)
  )

women_plot <- ggplot()+
  geom_segment(data = gay_women_avg, aes(x = release_year+0.5, y = avg_age_women_old, xend = release_year+0.5, yend = avg_age_women_young), colour = "#3a3a3c",size=0.3)+ #Plot line between each partner
   geom_point(data = gay_women_old_avg, aes(x = release_year+0.5, y = avg_age_women_old),colour="#eeb400",size=2)+ #Plot points for homosexual women (oldest)
  geom_point(data = gay_women_young_avg , aes(x = release_year+0.5, y = avg_age_women_young),colour="#eeb400",size=2)+ #Plot points for homosexual women (youngest)
  labs(x = "Year of movie release", y ="Age of actors", subtitle = "Women in same-sex couples")+
  ylim(20,81)+
  theme(text=element_text(size=12,  family="Roboto", colour = "#3a3a3c"),
        panel.background = element_rect(fill = "#d1d2d4", colour = "#d1d2d4",size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        plot.background = element_rect(fill = "#d1d2d4",colour = "white", size=3),
        axis.title.x = element_text(color="#3a3a3c", size = 12,vjust = -0.2),
        axis.title.y = element_text(color="#3a3a3c", size = 12,vjust = 0.5),
        plot.subtitle= element_text(color="#3a3a3c", size = 14, family = "Roboto",face = "bold",vjust = 0.5)
  )

title <- ggplot()+
  ggtitle("Age gaps of couples in Hollywood movies, 1935 to 2022")+
  theme_void()+
  theme(text=element_text(color="#3a3a3c", size = 20, family = "Limelight"),
        plot.title = element_text(hjust = 0.5, vjust =1.9),
        plot.margin = margin(0.5, 0, 0, 0, "cm"))

caption <- ggplot()+
  labs(caption = "Source: Data from Hollywood Age Gap from Data Is Plural")+
  theme_void()+
  theme(text=element_text(color="#3a3a3c", size = 12, family = "Roboto"),
        plot.caption = element_text(hjust = 0.99),
        plot.margin = margin(1, 0, 1, 1, "cm"))

figure <- grid.arrange(title,hetero_plot, men_plot, women_plot, caption, layout_matrix = cbind(c(1,2,3,5), c(1,2,4,5)),heights=c(0.3,2,2,0.3))

#Save the graph
ggsave("age_gaps_hollywood.png",
       plot = figure,
       device = "png",
       width = 12,
       height = 8, dpi=300)
