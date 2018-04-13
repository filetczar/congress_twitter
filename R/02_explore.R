#######################
# Explore & Viz Data   
######################


library(ggrepel)
library(ggiraph)
library(ggnetwork)
library(igraph)
library(statnet)
library(tidyverse)
library(stringr)

setwd('/Users/acazar/Desktop/blog/projects/congress_twitter')
edges_df <- readRDS('./data/edges_df.rds')
politicians <- read_csv('./data/politicians.csv')
pvi <- read_csv('./data/cook_pvi.csv')
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

######################
#  LIST -> DF 
######################

edges_df <- data.frame()
  
  for(i in seq_along(edges_list)) {
edges_df <- dplyr::bind_rows(edges_df,edges_list[[i]])  
  }

######################
# Break into House and Senate
# Break into Parties
# Within States and Across States
######################

politicians$name <- str_c(politicians$first_name, politicians$last_name, sep= " ")

head(edges_df)

edges_df %>% 
  dplyr::filter(source == 'Mike Bost') %>% 
  head()
# add to edges DF for target + source:
# party, state, S or H, PVI? 

# main network 
# source party, type
# connection: dotted if cross party, solid if inter party 
# size = centrality 

main_df <- edges_df %>% 
  inner_join(., politicians, by = c('source'='name')) %>% 
  select(source, target, source_type =type, source_party =party) %>% 
  inner_join(.,politicians, by=c('target'='name')) %>% 
  select(source, target, source_type, source_party,target_type =type,target_party=party) %>% 
  mutate(line = ifelse(source_party == target_party, "solid", 'dotted')) %>% 
  select(source, target, type=source_type, party=source_party, line)

senate_df <- edges_df %>% 
  inner_join(., politicians, by = c('source'='name')) %>% 
  select(source, target, source_type =type, source_party =party) %>% 
  inner_join(.,politicians, by=c('target'='name')) %>% 
  select(source, target, source_type, source_party,target_type =type,target_party=party) %>%
  dplyr::filter(source_type== 'sen' & target_type =='sen') %>% 
  mutate(line = ifelse(source_party == target_party, "solid", 'dotted')) %>% 
  select(source, target, type=source_type, party=source_party, line)
##### igraph to ggnetwork
View(senate_df)
senate_df <- main_df %>% 
            dplyr::filter(type=='sen')

graph_df <- graph_from_data_frame(main_df)
senate_graph_obj <- graph_from_data_frame(senate_df,directed = FALSE)

head(main_df)
n <- network::network(edges_df)
summary.network(n)
network.vertex.names(n)

set.network.attribute(n,'type', main_df[,'type'])
set.network.attribute(n, 'party', main_df[,'party'])
set.network.attribute(n, 'line', main_df[,'line'])

network_df <- ggnetwork(graph_df, layout= 'fruchtermanreingold', cell.jitter=.5 )
senate_network <- ggnetwork(senate_graph_obj, layout= 'fruchtermanreingold', cell.jitter=.5)
View(network_df)
ggplot(network_df, aes(x=x, y=y, xend=xend, yend=yend, fill=party))+
  geom_edges(aes(linetype=line),color = 'grey50') +
  theme_minimal() +
  geom_nodetext(aes(label =vertex.names)) +
  geom_point_interactive(size =2)

ggplot(senate_network, aes(x=x, y=y, xend=xend, yend=yend))+
  geom_edges(aes(linetype=line),color = 'grey50') +
  theme_blank() +
  geom_nodetext(aes(label =vertex.names), size = 3) +
  geom_node_point(aes(fill=party)) +
  scale_fill_manual(party, c("Democrat" = 'blue', "Republican" = "red", "Independent"="green"))
  
######################
#  TRY AGAIN FOR SENATE
######################
# GOOD 
edges_df %>% 
  filter(source =='Benjamin Sasse')

senate_graph <- network(senate_df[,c('source','target')])
count <- network.edgecount(senate_graph)
set.edge.attribute(senate_graph, "type", senate_df[,'type'])
set.edge.attribute(senate_graph, 'line', senate_df[,'line'])
set.edge.attribute(senate_graph, 'party', senate_df[,'party'])
View(senate_gg)
senate_gg <- ggnetwork(senate_graph,layout = "fruchtermanreingold", cell.jitter = .5)
senate_gg$betweenness <- round(betweenness(senate_graph)[senate_gg$vertex.names],2)


senate_gg <- readRDS('./data/senate_gg.rds')
# attempt interactive
# geom_point_interactive vs. geom_text_interactive
# plot sizing 
# minimal legend (maybe just arrows and pointing to examples)

plot <- ggplot(senate_gg, aes(x = x, y = y, xend=xend, yend=yend, color = party)) +
  geom_edges(aes(linetype = line), curvature = .4, alpha=.5, size=.1) +
  geom_nodes(alpha= .6, aes(fill =party, size =betweenness), show.legend = FALSE) +
  scale_color_manual(values=c('Democrat'='dodgerblue', 'Republican'='red', 'Independent'='forestgreen', 'NA'='grey'))+
  theme_blank() +
  scale_linetype_manual(values = c('solid'='solid', 'dotted'='dotted')) +
  geom_nodetext(aes(label=vertex.names), fontface='bold', size =3) +
  geom_text_repel(aes(label=vertex.names))
  


ggiraph(code={print(plot)})

# senate's centraility 
centr_degree(senate_graph_obj, mode='total')
# centralization .578

# get closeness 
senate_closeness <- igraph::closeness(senate_graph_obj,mode='total') %>% 
                    as.data.frame() %>% 
                    tibble::rownames_to_column() 
names(senate_closeness) <- c('Senator', 'Closeness')

senate_closeness %>% 
  arrange(desc(Closeness)) %>%
  head()
# closeness could be interpreted as bipartisanship 
# betweenness could be intepreted as connections 
View(senate_gg)
senate_btw <- senate_gg %>% 
  distinct(vertex.names, betweenness)

