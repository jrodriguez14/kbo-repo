#this will be some R analyis using the korean baseball league dataset

head(kbobattingdata)

skim(kbobattingdata)

#get the number of years each team has been in the league
years_played <- kbobattingdata %>% 
  group_by(team) %>% 
  summarise(years_in_league=n())

#plot the number of years played by each team
ggplot(years_played,aes(x=team,y=years_in_league,fill=team))+
  geom_col()+
  scale_fill_viridis(discrete = T,alpha=.8,option="A")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Years each team has played in the league",
       x="Team",y="Number of Years")+
  guides(fill="none")
  
#total number of games played in kbo by year
games_played <- kbobattingdata %>% 
  group_by(year) %>% 
  tally(games) %>% 
  mutate(games_total=n) %>% 
  select(year,games_total) %>% 
  ggplot(aes(x=year,y=games_total,fill=games_total))+
  geom_col()+
  scale_fill_viridis(discrete = F,alpha=.8,option="A")+
  labs(title = "Total Number of Games Played in KBO",
       subtitle = "by Year",
       x="Year",y="Number of Games")