#######################
# Explore & Viz Data   
######################


nrow(edges_list[[1]])

#longer tenure = more connections?
#closeness between states
#closeness within states
# between parties (betweeness measures)
# within parties ()
# networks in senate and house 
# govtrack.us to see likelihood of supporting bills together 
# map connections from different states to see their relationship
# who is the bridge between each chamber, each party
# 2018 midterms: more competitive seat breed more bipartisanship (following)


edges_df <- data.frame()
  
  for(i in seq_along(edges_list)) {
edges_df <- dplyr::bind_rows(edges_df,edges_list[[i]])  
  }

edges_df %>% 
  dplyr::filter(stringr::str_detect(source,'Massie')) %>% 
  head(10)
