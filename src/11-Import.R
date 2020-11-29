source("src/01-config.R")
source("src/02-helpers.R")

## Read data from google sheets
gs4_deauth()
arbre <- read_sheet("https://docs.google.com/spreadsheets/d/1s1IawUBVc88SyZKsSXa8jo440yqJu3sQvrtB-mqdWUk/edit?usp=sharing") 
names(arbre) <- str_replace_all(names(arbre),"é|è","e") %>% 
  str_replace_all(" ","_") %>% 
  tolower()

arbre <- ungroup(arbre) %>% 
  mutate(annee_naissance = lubridate::year(date_de_naissance),
         child=paste(nom,prenom,sep=" "))


### On définit les différentes relations familiales à partir du fichier de base
### On dispose de la forme dataframe et graphe

# Filiations
filiations_df <- select(arbre,child,pere,mere,annee_naissance) %>% 
  pivot_longer(-c(child,annee_naissance),values_to = "parent",names_to = "type") %>% 
  rename(from=parent,to=child) %>% 
  mutate(type="filiation")
graph_filiations <- select(filiations_df,from,to) %>% 
  na.omit() %>% 
  graph_from_data_frame()

#  Mariages
mariages_df <- filter(arbre,!is.na(marie_e_avec)) %>% 
  select(from=child,annee_naissance,to=marie_e_avec) %>% 
  mutate(type="mariage")
graph_mariage <- select(mariages_df,from,to) %>% 
  graph_from_data_frame()

# Fratries
fratries <- ego(graph_filiations,order = ,mode="in",mindist = 1)
fratries_df <- lapply(fratries,function(xx) t(combn(names(xx),pmin(2,length(xx)))))
fratries_df <- fratries_df[which(lapply(fratries_df,length)>1)] %>% 
  do.call(rbind,.)  %>% 
  as.data.frame() %>% 
  rename(from=V1,to=V2) %>% 
  distinct()

# Parents
parents <- ego(graph_filiations,order = ,mode="out",mindist = 1)
parents_df <- lapply(parents,function(xx) t(combn(names(xx),pmin(2,length(xx)))))
parents_df <- parents_df[which(lapply(parents_df,length)>1)] %>% 
  do.call(rbind,.)  %>% 
  as.data.frame() %>% 
  rename(from=V1,to=V2) %>% 
  distinct()



# plot(family_graph,edge.arrow.size=.2)


