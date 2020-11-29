source("src/11-Import.R")

### Toutes les edges dans la même table
liens <- bind_rows(filiations_df,mariages_df) %>% 
  select(from,to,annee_naissance,type) %>% 
  mutate(across(c(from,to),
                function(xx) coalesce(xx,"Inconnu"))) %>% 
  filter(from!="Inconnu" & to!="Inconnu") %>% 
  mutate(lty=ifelse(type=="filiation","dashed","solid"))

# Tous les individus
tout_le_monde <- c(liens$from,liens$to) %>% 
  unique() %>% 
  setdiff("Inconnu")

gen_ref <- filter(arbre,generation_reference==1) # Génération de référence (la mienne)

### Calculer la distance de chaque individu par rapport à la génération de référence
### (nécessaire pour la représentation graphique)
dist_enfants <- shortest.paths(graph_filiations,mode = "out")[,gen_ref$child] %>% 
  apply(1,min)
dist_ancetres <- shortest.paths(graph_filiations,mode = "in")[,gen_ref$child] %>% 
  apply(1,min)

generations <- c(-1*dist_enfants[!is.infinite(dist_enfants)],
                 dist_ancetres[!is.infinite(dist_ancetres)])
generations <- data.frame(nom=names(generations),generation=generations) %>% 
  distinct()

non_trouv <- setdiff(tout_le_monde,generations$nom)
for (ii in non_trouv)
{
  gen <- lapply(c("parents","mariages","fratries"),
                function(xx) get_generation_from_liens(ii,type_lien = xx))
  gen <- gen[which(lapply(gen,nrow)>0)][[1]]
  if (nrow(gen)>0) generations <- bind_rows(generations,gen)
  else print(paste0("Attention : impossible de détemriner la génération de ",ii))
}

generations <- mutate(generations,generation=max(generation)-generation-min(generation))

lieux <- paste(str_sub(arbre$lieu_de_vie_principal,1,2),
               str_extract(arbre$lieu_de_vie_principal,"[,|\\s]([A-Za-z]+?[\\s\\w+]*)"),
               "france")
geo <- tidygeocoder::geo_osm(lieux)
arbre <- bind_cols(arbre,geo) %>% 
  mutate(arbre,nom_complet=paste(nom,prenom))



# Construction de la table des individus
individus <- select(arbre,nom=nom_complet,genre,annee_naissance,famille=nom) %>% 
  right_join(generations,by="nom") %>%  
  mutate(nom2 = unlist(purrr::map(str_split(nom," |-"),1)),
         famille=as.factor(ifelse(is.na(famille),nom2,famille)),
         shape=ifelse(genre=="M","square","circle"),
         shape = ifelse(is.na(shape),"crectangle",shape)) %>% 
  select(-nom2)

# couleurs
pal <- n_distinct(individus$famille) %>% 
  rainbow()
individus$color <- pal[individus$famille]

# Construction du graphe d'ensemble
family_graph <- graph_from_data_frame(liens,directed = T,vertices = individus)


