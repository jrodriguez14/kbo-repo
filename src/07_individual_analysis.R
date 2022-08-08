#on base percentage, sluggking percentage, on base plus slugging

#another batting df
batting2 <- kbobattingdata %>% 
  group_by(year,team) %>% 
  select(year,team,OBP,SLG,OPS) %>% 
  drop_na()
save(batting2,file="batting2.rda")
#scatter plot for obp
ggplot(batting2,aes(x=year,y=OBP))+
  geom_point(aes(color=OBP))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="OBP"))+
  labs(title = "OBP",
       subtitle = "by Year",
       x="Year",y="OBP")
ggsave("obp_scatter.png")
#scatter plot for obp by team
ggplot(batting2,aes(x=year,y=OBP))+
  geom_point(aes(color=OBP))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="OBP"))+
  labs(title = "OBP",
       subtitle = "by Year, by Team",
       x="Year",y="OBP")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)
ggsave("obp_facet_scatter.png")

#scatter plot for slg
ggplot(batting2,aes(x=year,y=SLG))+
  geom_point(aes(color=SLG))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="SLG"))+
  labs(title = "SLG",
       subtitle = "by Year",
       x="Year",y="SLG")
ggsave("slg_scatter.png")
#scatter plot for obp by team
ggplot(batting2,aes(x=year,y=SLG))+
  geom_point(aes(color=SLG))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="SLG"))+
  labs(title = "SLG",
       subtitle = "by Year, by Team",
       x="Year",y="SLG")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)
ggsave("slg_facet_scatter.png")

#scatter plot for ops
ggplot(batting2,aes(x=year,y=OPS))+
  geom_point(aes(color=OPS))+
  scale_color_viridis(discrete = F,option = "C",guide=guide_legend(title="OPS"))+
  labs(title = "OPS",
       subtitle = "by Year",
       x="Year",y="OPS")
ggsave("ops_scatter.png")
#scatter plot for ops by team
ggplot(batting2,aes(x=year,y=OPS))+
  geom_point(aes(color=OPS))+
  scale_color_viridis(discrete = F,option = "F",guide=guide_legend(title="OPS"))+
  labs(title = "OPS",
       subtitle = "by Year, by Team",
       x="Year",y="OPS")+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size=8))+
  facet_wrap(~team)
ggsave("ops_facet_scatter.png")