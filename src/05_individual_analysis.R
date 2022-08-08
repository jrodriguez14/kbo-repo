#run types
#Doubles, Triples, HR, RBI

#run type df
run_type<- kbobattingdata %>% 
  group_by(year,team) %>% 
  select(year,team,doubles,triples,homeruns,RBI)
save(run_type,file="run_type.rda")

#plot for doubles
ggplot(run_type,aes(x=year,y=doubles))+
  geom_point(aes(color=doubles))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="Doubles"))+
  labs(title = "Total Doubles",
       subtitle = "by Year",
       x="Year",y="Doubles")
ggsave("doubles_scatter.png")

#plot bar graph for doubles
ggplot(run_type,aes(x=year,y=doubles,fill=doubles))+
  geom_col()+
  scale_fill_viridis(discrete = F,option = "A",guide=guide_legend(title="Doubles"))+
  labs(title = "Total Doubles",
       subtitle = "by Year",
       x="Year",y="Doubles")
ggsave("doubles_bar.png")

#facet wrap plot for doubles
ggplot(run_type,aes(x=year,y=doubles))+
  geom_point(aes(color=doubles))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="doubles"))+
  labs(title = "Doubles",
       subtitle = "by Year, by Team",
       x="Year",y="Doubles")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)
ggsave("doubles_scatter_facet_teams.png")

#plot for triples
ggplot(run_type,aes(x=year,y=triples))+
  geom_point(aes(color=triples))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="Triples"))+
  labs(title = "Total Triples",
       subtitle = "by Year",
       x="Year",y="Triples")
ggsave("triples_scatter.png")

#plot bar graph for triples
ggplot(run_type,aes(x=year,y=triples,fill=triples))+
  geom_col()+
  scale_fill_viridis(discrete = F,option = "A",guide=guide_legend(title="Triples"))+
  labs(title = "Total Triples",
       subtitle = "by Year",
       x="Year",y="Triples")
ggsave("triples_bar.png")

#plot facet wrap for triples
ggplot(run_type,aes(x=year,y=triples))+
  geom_point(aes(color=triples))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="Triples"))+
  labs(title = "Triples",
       subtitle = "by Year, by Team",
       x="Year",y="Triples")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)
ggsave("triples_scatter_facet_teams.png")

#plot for homeruns
ggplot(run_type,aes(x=year,y=homeruns))+
  geom_point(aes(color=homeruns))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="Homeruns"))+
  labs(title = "Total Homeruns",
       subtitle = "by Year",
       x="Year",y="Homeruns")
ggsave("homeruns_scatter.png")

#plot bar graph for homeruns
ggplot(run_type,aes(x=year,y=homeruns,fill=homeruns))+
  geom_col()+
  scale_fill_viridis(discrete = F,option = "A",guide=guide_legend(title="Homeruns"))+
  labs(title = "Total Homeruns",
       subtitle = "by Year",
       x="Year",y="Homeruns")
ggsave("homeruns_bar.png")

#plot facet wrap for homeruns
ggplot(run_type,aes(x=year,y=homeruns))+
  geom_point(aes(color=homeruns))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="Homeruns"))+
  labs(title = "Homeruns",
       subtitle = "by Year, by Team",
       x="Year",y="Homeruns")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)
ggsave("homeruns_scatter_facet_teams.png")

#plot for RBIs
ggplot(run_type,aes(x=year,y=RBI))+
  geom_point(aes(color=homeruns))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="RBIs"))+
  labs(title = "Total RBIs",
       subtitle = "by Year",
       x="Year",y="RBIs")
ggsave("rbis_scatter.png")

#plot bar graph for RBIs
ggplot(run_type,aes(x=year,y=RBI,fill=RBI))+
  geom_col()+
  scale_fill_viridis(discrete = F,option = "A",guide=guide_legend(title="RBIs"))+
  labs(title = "Total RBIs",
       subtitle = "by Year",
       x="Year",y="RBIs")
ggsave("rbis_bar.png")

#facet wrap plot for RBIs
ggplot(run_type,aes(x=year,y=RBI))+
  geom_point(aes(color=RBI))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="RBI"))+
  labs(title = "RBIs",
       subtitle = "by Year, by Team",
       x="Year",y="RBI")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)
ggsave("rbis_scatter_facet_teams.png")
