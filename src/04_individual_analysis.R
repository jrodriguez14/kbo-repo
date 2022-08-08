#individual stat analysis

#runs per game df
runs_game<- kbobattingdata %>% 
  group_by(year,team) %>% 
  select(year,team,runs_per_game)
save(runs_game,file="runs_game.rda")

#scatter plot for runs per game
ggplot(runs_game,aes(x=year,y=runs_per_game))+
  geom_point(aes(color=runs_per_game))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="Runs per Game"))+
  labs(title = "Runs per game",
       subtitle = "by Year",
       x="Year",y="Runs")
ggsave("runs_game_scatter.png")

#plot facet wrap
ggplot(runs_game,aes(x=year,y=runs_per_game))+
  geom_point(aes(color=runs_per_game))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="Runs per Game"))+
  labs(title = "Runs Per Game",
       subtitle = "by Year, by Team",
       x="Year",y="Runs")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)
ggsave("runs_game_scatter_facet_teams.png")

#plot bar graph
ggplot(runs_game,aes(x=year,y=runs_per_game,fill=runs_per_game))+
  geom_col()+
  scale_fill_viridis(discrete = F,option = "A",guide=guide_legend(title="Runs per Game"))+
  labs(title = "Runs Per Game",
       subtitle = "by Year",
       x="Year",y="Runs")
ggsave("runs_game_bar.png")