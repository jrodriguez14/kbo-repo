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