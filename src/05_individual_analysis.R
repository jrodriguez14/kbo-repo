#run types
#Doubles, Triples, HR, RBI

#run type df
run_type<- kbobattingdata %>% 
  group_by(year,team) %>% 
  select(year,team,doubles,triples,homeruns,RBI)
save(run_type,file="run_type.rda")

