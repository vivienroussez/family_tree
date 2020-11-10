## Pour ceux dont la génération n'a pas pu être trouvée par les liens de filiation
## Fonction qui checke les autres liens pour la déterminer

# qui <- ii;type_lien <- "fratries";gene_df=generations
get_generation_from_liens <- function(qui,
                                      gene_df=generations,
                                      type_lien=c("parents","fratries","mariages")){
  lien <- get(paste0(type_lien,"_df")) %>% 
    filter(from==qui | to==qui) %>% 
    unlist() %>% 
    setdiff(qui) %>% 
    first()
  gen <- filter(gene_df,nom==lien) %>% 
    mutate(nom=qui)
  return(gen)
}
