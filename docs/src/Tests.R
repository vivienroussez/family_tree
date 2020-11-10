################################
### igraph model + basic viz ###
################################

gg <- filiations %>% 
  select(from,to) %>% 
  na.omit() %>% 
  igraph::graph_from_data_frame()
plot.igraph(gg,edge.arrow.size=.2,label.cex=2 )

gg <- filiations %>% 
  filter(str_detect(to,"Bruneel|Douai")) %>% 
  select(from,to) %>% 
  igraph::graph_from_data_frame()
plot(gg)
plot(gg,layout= layout.reingold.tilford(gg, root = 1, flip.y = FALSE, circular = FALSE))

#####################
# Test visNetwork ###
#####################

personnes <- data.frame(label=c(prep$child)) %>% 
  distinct() %>%  
  mutate(label=as.character(label),
         id=row_number())

elist <- data.frame(from=match(prep$parent,personnes$label),to=match(prep$child,personnes$label))
visNetwork(personnes,elist,label=personnes$nodes) %>% 
  visEdges(arrows = "to") %>% 
  visHierarchicalLayout()

#########################################################################
### Determination des relations (parents, fratries, grands parent...) ###
#########################################################################

parents <- ego(gg,order=1,mode="in",mindist = 1)
grands_parents <- ego(gg,order = 2,mode = "in",mindist = 2)
enfants <- ego(gg,order=1,mode="out",mindist = 1)

who_has_common <- function(xx,yy){
  # xx : a list in which to check presence of yy
  # yy : a single element to be matched
  mm <- lapply(xx,function(xx) intersect(xx,yy))
  res <- sapply(mm,function(xx) length(xx)!=0) %>%  which()
  return(res)
}
who_has_common(enfants,enfants[[1]])

couples <- lapply(enfants,function(xx) who_has_common(enfants,xx)) %>%  unique()
fratries <- lapply(parents,function(xx) who_has_common(parents,xx))
cousins <- lapply(grands_parents,function(xx) who_has_common(grands_parents,xx)) %>% 
  map2(fratries,function(xx,yy) setdiff(xx,yy))

################################################################
### calcul des generations à partir de la génération magique ###
################################################################


