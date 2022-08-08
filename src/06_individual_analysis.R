#stolen bases and caught stealing

#steal df
steal <- kbobattingdata %>% 
  group_by(year,team) %>% 
  select(year,team,stolen_bases,caught_stealing) %>% 
  drop_na()
save(steal,file="steal.rda")

#make data long so i can plot 
steal_long <- pivot_longer(steal,3:4,names_to = "type",
                           values_to = "total")
save(steal_long,file="steal_long.rda")

#plot bar graph 
ggplot(steal_long,aes(x=year,y=total,fill=type))+
  geom_col()+
  scale_fill_viridis(discrete = T,option = "E",guide=guide_legend(title="Type"))+
  labs(title = "Total Stolen Bases and Total Times Caught Stealing",
       subtitle = "by Year",
       x="Year",y="Total")
ggsave("steals_bar.png")

#plot bar graph facet by team
ggplot(steal_long,aes(x=year,y=total,fill=type))+
  geom_col()+
  scale_fill_viridis(discrete = T,option = "E",guide=guide_legend(title="Type"))+
  labs(title = "Total Stolen Bases and Total Times Caught Stealing",
       subtitle = "by Year, Team",
       x="Year",y="Total")+
  facet_wrap(~team)
ggsave("steals_facet_bar_team.png")

#batting avg, strikeout , walks(bases on balls)
batting <- kbobattingdata %>% 
  group_by(year,team) %>% 
  select(year,team,batting_average,strikeouts,bases_on_balls,plate_appearances) %>% 
  mutate(strikeout_percentage=round(strikeouts/plate_appearances,3),
         walk_percentage=round(bases_on_balls/plate_appearances,3)) %>% 
  select(year,team,batting_average,strikeout_percentage,walk_percentage) %>% 
  drop_na()
save(batting,file="batting.rda")

#pivot data long
batting_long <- pivot_longer(batting,3:5,names_to = "type",values_to = "percentage")
save(batting_long,file="batting_long.rda")


#plot bar graph with these percentages
ggplot(batting_long,aes(x=year,y=percentage,fill=type))+
  geom_col()+
  scale_fill_viridis(discrete = T,option = "E",guide=guide_legend(title="Type"))+
  labs(title = "Batting Average, Strikeout Percentage, Walk Percentage",
       subtitle = "by Year",
       x="Year",y="Percentage")
ggsave("batting_bar.png")
#plot bar graph with facet
ggplot(batting_long,aes(x=year,y=percentage,fill=type))+
  geom_col()+
  scale_fill_viridis(discrete = T,option = "E",guide=guide_legend(title="Type"))+
  labs(title = "Batting Average, Strikeout Percentage, Walk Percentage",
       subtitle = "by Year",
       x="Year",y="Percentage")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)
ggsave("batting_facet_bar.png")