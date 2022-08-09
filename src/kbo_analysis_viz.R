#' ---
#' title: "KBO Visualizations"
#' author: Jesus Rodriguez
#' output: github_document
#' 
#' ---
#' We use the *kbobattingdata.csv* file from Kaggle
#+ load packages, include=FALSE
library(tidyverse)
library(readr)
library(skimr)
library(janitor)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(viridis)
library(lubridate)


#+ import data, include=FALSE
kbobattingdata <- read_csv("data/kbobattingdata.csv")
head(kbobattingdata)

#+ years_played df, include=FALSE
years_played <- kbobattingdata %>% 
  group_by(team) %>% 
  summarise(years_total=n())
#' **League Wide**
#+ years played plot, echo=FALSE 
ggplot(years_played,aes(x=team,y=years_total,fill=team))+
  geom_col()+
  scale_fill_viridis(discrete = T,alpha=.8,option="A")+
  coord_flip()+
  labs(title = "Years each team has played in the league",
       x="Team",y="Number of Years")+
  guides(fill="none")

#+ games played df, include=FALSE
games_played <- kbobattingdata %>% 
  group_by(year) %>% 
  tally(games) %>% 
  mutate(games_total=n) %>% 
  select(year,games_total)

#+ games played plot , echo=FALSE 
ggplot(games_played,aes(x=year,y=games_total,fill=games_total))+
  geom_col()+
  scale_fill_viridis(discrete = F,alpha=.8,option="A")+
  labs(title = "Total Number of Games Played in KBO",
       subtitle = "by Year",
       x="Year",y="Number of Games")+
  guides(fill=guide_legend(title="Scale"))

#+ batter age df, include=FALSE
batter_age <- kbobattingdata %>% 
  group_by(year,team) %>% 
  select(year,team,average_batter_age)


#+ batter age scatter, echo=FALSE
ggplot(batter_age,aes(x=year,y=average_batter_age))+
  geom_point(aes(color=average_batter_age))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="Average Batter Age"))+
  labs(title = "Average Batter Age",
       subtitle = "by Year",
       x="Year",y="Average Age")


#+ facet batter age, echo=FALSE
ggplot(batter_age,aes(x=year,y=average_batter_age))+
  geom_point(aes(color=average_batter_age))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="Average Batter Age"))+
  labs(title = "Average Batter Age",
       subtitle = "by Year, by Team",
       x="Year",y="Average Age")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)


#+ bar batter age, echo=FALSE
ggplot(batter_age,aes(x=year,y=average_batter_age,fill=average_batter_age))+
  geom_col()+
  scale_fill_viridis(discrete = F,option = "A",guide=guide_legend(title="Average Batter Age"))+
  labs(title = "Average Batter Age",
       subtitle = "by Year",
       x="Year",y="Average Age")

#' **Indivual Stats**
#' **Runs**

#+ runs per game df, include=FALSE
runs_game<- kbobattingdata %>% 
  group_by(year,team) %>% 
  select(year,team,runs_per_game)


#+ scatter plot for runs per game, echo=FALSE
ggplot(runs_game,aes(x=year,y=runs_per_game))+
  geom_point(aes(color=runs_per_game))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="Runs per Game"))+
  labs(title = "Runs per game",
       subtitle = "by Year",
       x="Year",y="Runs")


#+ facet wrap runs, echo=FALSE
ggplot(runs_game,aes(x=year,y=runs_per_game))+
  geom_point(aes(color=runs_per_game))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="Runs per Game"))+
  labs(title = "Runs Per Game",
       subtitle = "by Year, by Team",
       x="Year",y="Runs")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)

#+ bar graph runs, echo=FALSE
ggplot(runs_game,aes(x=year,y=runs_per_game,fill=runs_per_game))+
  geom_col()+
  scale_fill_viridis(discrete = F,option = "A",guide=guide_legend(title="Runs per Game"))+
  labs(title = "Runs Per Game",
       subtitle = "by Year",
       x="Year",y="Runs")

#' **Run Types**
#' **Doubles, Triples, HR, RBI**

#+ run type df, include=FALSE
run_type<- kbobattingdata %>% 
  group_by(year,team) %>% 
  select(year,team,doubles,triples,homeruns,RBI)

#+ plot for doubles, echo=FALSE
ggplot(run_type,aes(x=year,y=doubles))+
  geom_point(aes(color=doubles))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="Doubles"))+
  labs(title = "Total Doubles",
       subtitle = "by Year",
       x="Year",y="Doubles")


#+ plot bar graph for doubles, echo=FALSE
ggplot(run_type,aes(x=year,y=doubles,fill=doubles))+
  geom_col()+
  scale_fill_viridis(discrete = F,option = "A",guide=guide_legend(title="Doubles"))+
  labs(title = "Total Doubles",
       subtitle = "by Year",
       x="Year",y="Doubles")

#+ facet wrap plot for doubles, echo=FALSE
ggplot(run_type,aes(x=year,y=doubles))+
  geom_point(aes(color=doubles))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="doubles"))+
  labs(title = "Doubles",
       subtitle = "by Year, by Team",
       x="Year",y="Doubles")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)


#+ plot for triples, echo=FALSE
ggplot(run_type,aes(x=year,y=triples))+
  geom_point(aes(color=triples))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="Triples"))+
  labs(title = "Total Triples",
       subtitle = "by Year",
       x="Year",y="Triples")


#+ plot bar graph for triples, echo=FALSE
ggplot(run_type,aes(x=year,y=triples,fill=triples))+
  geom_col()+
  scale_fill_viridis(discrete = F,option = "A",guide=guide_legend(title="Triples"))+
  labs(title = "Total Triples",
       subtitle = "by Year",
       x="Year",y="Triples")


#+ plot facet wrap for triples, echo=FALSE
ggplot(run_type,aes(x=year,y=triples))+
  geom_point(aes(color=triples))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="Triples"))+
  labs(title = "Triples",
       subtitle = "by Year, by Team",
       x="Year",y="Triples")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)


#+ plot for homeruns, echo=FALSE
ggplot(run_type,aes(x=year,y=homeruns))+
  geom_point(aes(color=homeruns))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="Homeruns"))+
  labs(title = "Total Homeruns",
       subtitle = "by Year",
       x="Year",y="Homeruns")


#+ plot bar graph for homeruns, echo=FALSE
ggplot(run_type,aes(x=year,y=homeruns,fill=homeruns))+
  geom_col()+
  scale_fill_viridis(discrete = F,option = "A",guide=guide_legend(title="Homeruns"))+
  labs(title = "Total Homeruns",
       subtitle = "by Year",
       x="Year",y="Homeruns")


#+ plot facet wrap for homeruns, echo=FALSE
ggplot(run_type,aes(x=year,y=homeruns))+
  geom_point(aes(color=homeruns))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="Homeruns"))+
  labs(title = "Homeruns",
       subtitle = "by Year, by Team",
       x="Year",y="Homeruns")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)


#+ plot for RBIs, echo=FALSE
ggplot(run_type,aes(x=year,y=RBI))+
  geom_point(aes(color=homeruns))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="RBIs"))+
  labs(title = "Total RBIs",
       subtitle = "by Year",
       x="Year",y="RBIs")


#+ plot bar graph for RBIs, echo=FALSE
ggplot(run_type,aes(x=year,y=RBI,fill=RBI))+
  geom_col()+
  scale_fill_viridis(discrete = F,option = "A",guide=guide_legend(title="RBIs"))+
  labs(title = "Total RBIs",
       subtitle = "by Year",
       x="Year",y="RBIs")


#+ facet wrap plot for RBIs, echo=FALSE
ggplot(run_type,aes(x=year,y=RBI))+
  geom_point(aes(color=RBI))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="RBI"))+
  labs(title = "RBIs",
       subtitle = "by Year, by Team",
       x="Year",y="RBI")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)
#' **Steals**
#'

#+ steal df, include=FALSE
steal <- kbobattingdata %>% 
  group_by(year,team) %>% 
  select(year,team,stolen_bases,caught_stealing) %>% 
  drop_na()


#+ steal long, include=FALSE
steal_long <- pivot_longer(steal,3:4,names_to = "type",
                           values_to = "total")


#+ plot bar graph steals, echo=FALSE
ggplot(steal_long,aes(x=year,y=total,fill=type))+
  geom_col()+
  scale_fill_viridis(discrete = T,option = "E",guide=guide_legend(title="Type"))+
  labs(title = "Total Stolen Bases and Total Times Caught Stealing",
       subtitle = "by Year",
       x="Year",y="Total")


#+ plot bar graph facet by team steals, echo=FALSE
ggplot(steal_long,aes(x=year,y=total,fill=type))+
  geom_col()+
  scale_fill_viridis(discrete = T,option = "E",guide=guide_legend(title="Type"))+
  labs(title = "Total Stolen Bases and Total Times Caught Stealing",
       subtitle = "by Year, Team",
       x="Year",y="Total")+
  facet_wrap(~team)

#' **Batting**
#+ batting avg, strikeout , walks(bases on balls) df, include=FALSE
batting <- kbobattingdata %>% 
  group_by(year,team) %>% 
  select(year,team,batting_average,strikeouts,bases_on_balls,plate_appearances) %>% 
  mutate(strikeout_percentage=round(strikeouts/plate_appearances,3),
         walk_percentage=round(bases_on_balls/plate_appearances,3)) %>% 
  select(year,team,batting_average,strikeout_percentage,walk_percentage) %>% 
  drop_na()


#+ pivot data long batting, include=FALSE
batting_long <- pivot_longer(batting,3:5,names_to = "type",values_to = "percentage")



#+ bar batting, echo=FALSE
ggplot(batting_long,aes(x=year,y=percentage,fill=type))+
  geom_col()+
  scale_fill_viridis(discrete = T,option = "E",guide=guide_legend(title="Type"))+
  labs(title = "Batting Average, Strikeout Percentage, Walk Percentage",
       subtitle = "by Year",
       x="Year",y="Percentage")

#+ plot bar graph with facet batting, echo=FALSE
ggplot(batting_long,aes(x=year,y=percentage,fill=type))+
  geom_col()+
  scale_fill_viridis(discrete = T,option = "E",guide=guide_legend(title="Type"))+
  labs(title = "Batting Average, Strikeout Percentage, Walk Percentage",
       subtitle = "by Year",
       x="Year",y="Percentage")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)
#' **More Batting Stats**
#' on base percentage, slugging percentage, on base plus slugging

#+ another batting df, include=FALSE
batting2 <- kbobattingdata %>% 
  group_by(year,team) %>% 
  select(year,team,OBP,SLG,OPS) %>% 
  drop_na()

#+ scatter plot for obp, echo=FALSE
ggplot(batting2,aes(x=year,y=OBP))+
  geom_point(aes(color=OBP))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="OBP"))+
  labs(title = "OBP",
       subtitle = "by Year",
       x="Year",y="OBP")

#scatter plot for obp by team, echo=FALSE
ggplot(batting2,aes(x=year,y=OBP))+
  geom_point(aes(color=OBP))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="OBP"))+
  labs(title = "OBP",
       subtitle = "by Year, by Team",
       x="Year",y="OBP")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)


#+ scatter plot for slg, echo=FALSE
ggplot(batting2,aes(x=year,y=SLG))+
  geom_point(aes(color=SLG))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="SLG"))+
  labs(title = "SLG",
       subtitle = "by Year",
       x="Year",y="SLG")

#+ scatter plot for obp by team, echo=FALSE
ggplot(batting2,aes(x=year,y=SLG))+
  geom_point(aes(color=SLG))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="SLG"))+
  labs(title = "SLG",
       subtitle = "by Year, by Team",
       x="Year",y="SLG")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)


#+ scatter plot for ops, echo=FALSE
ggplot(batting2,aes(x=year,y=OPS))+
  geom_point(aes(color=OPS))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="OPS"))+
  labs(title = "OPS",
       subtitle = "by Year",
       x="Year",y="OPS")

#+ scatter plot for ops by team, echo=FALSE
ggplot(batting2,aes(x=year,y=OPS))+
  geom_point(aes(color=OPS))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="OPS"))+
  labs(title = "OPS",
       subtitle = "by Year, by Team",
       x="Year",y="OPS")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)
