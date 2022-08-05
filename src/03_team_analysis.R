#analysis on league teams


#get the number of years each team has been in the league
#create dataframe that stores the number of years each has played in the league
years_played <- kbobattingdata %>% 
  group_by(team) %>% 
  summarise(years_total=n())
save(years_played,file = "years_played.rda")

#plot the number of years played by each team
ggplot(years_played,aes(x=team,y=years_total,fill=team))+
  geom_col()+
  scale_fill_viridis(discrete = T,alpha=.8,option="A")+
  coord_flip()+
  labs(title = "Years each team has played in the league",
       x="Team",y="Number of Years")+
  guides(fill="none")
ggsave("years_played.png")

#total number of games played in the kbo by year
#create df 
games_played <- kbobattingdata %>% 
  group_by(year) %>% 
  tally(games) %>% 
  mutate(games_total=n) %>% 
  select(year,games_total)
save(games_played,file = "games_played.rda")

#plot the number of games played in the kbo by year
ggplot(games_played,aes(x=year,y=games_total,fill=games_total))+
  geom_col()+
  scale_fill_viridis(discrete = F,alpha=.8,option="A")+
  labs(title = "Total Number of Games Played in KBO",
       subtitle = "by Year",
       x="Year",y="Number of Games")+
  guides(fill=guide_legend(title="Scale"))
ggsave("games_played.png")

