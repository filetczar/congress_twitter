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
library(ggrepel)

######################
#  READ DATA IN 
#  EDITED FOR OUBLIC GITHUB
######################


######################
#  LIST -> DF 
######################

edges_df <- data.frame()
  
  for(i in seq_along(edges_list)) {
edges_df <- dplyr::bind_rows(edges_df,edges_list[[i]])  
  }


politicians$name <- str_c(politicians$first_name, politicians$last_name, sep= " ")


edges_df %>% 
  dplyr::filter(source == 'Mike Bost') %>% 
  head()

senate_df <- edges_df %>% 
  inner_join(., politicians, by = c('source'='name')) %>% 
  select(source, target, source_type =type, source_party =party) %>% 
  inner_join(.,politicians, by=c('target'='name')) %>% 
  select(source, target, source_type, source_party,target_type =type,target_party=party) %>%
  dplyr::filter(source_type== 'sen' & target_type =='sen') %>% 
  mutate(line = ifelse(source_party == target_party, "solid", 'dotted')) %>% 
  select(source, target, type=source_type, party=source_party, line)


######################
#  CREATE NETWORK
######################

senate_graph_obj <- graph_from_data_frame(senate_df,directed = TRUE)
senate_network <- ggnetwork(senate_graph_obj, layout= 'fruchtermanreingold', cell.jitter=.5)
senate_graph <- network(senate_df[,c('source','target')])

set.edge.attribute(senate_graph, "type", senate_df[,'type'])
set.edge.attribute(senate_graph, 'line', senate_df[,'line'])
set.edge.attribute(senate_graph, 'party', senate_df[,'party'])


senate_gg <- ggnetwork(senate_graph,layout = "fruchtermanreingold", cell.jitter = .5)
senate_gg$Betweenness <- round(betweenness(senate_graph)[senate_gg$vertex.names],2)

#### RAND PAULS NETWORK
rand_graph <- senate_df %>% 
            filter(source == 'Rand Paul' | target== 'Rand Paul') %>% 
            select(source,target) %>% 
            network()
rand_df <- senate_df %>% 
  filter(source == 'Rand Paul' | target== 'Rand Paul')
set.edge.attribute(rand_graph, "type", rand_df[,'type'])
set.edge.attribute(rand_graph, 'line', rand_df[,'line'])
set.edge.attribute(rand_graph, 'party', rand_df[,'party'])

senate_gg_rand <- ggnetwork(rand_graph,layout = "fruchtermanreingold", cell.jitter = .5, directed=TRUE)

senate_network$Betweenness <- round(betweenness(senate_graph)[senate_network$vertex.names],2)


senate_gg <- dplyr::rename(senate_gg, Betweenness = betweenness)
plot <- ggplot(senate_gg, aes(x = x, y = y, xend=xend, yend=yend, color = party, 
                              tooltip=vertex.names)) +
  geom_edges(aes(linetype = line), curvature=.4, alpha=.3, size=.1, show.legend = FALSE, angle =90, ncp=3) +
  geom_nodes(alpha= .4, aes(fill =party, size =Betweenness*.25)) +
  scale_color_manual(values=c('Democrat'='dodgerblue', 'Republican'='red', 'Independent'='forestgreen', 'NA'='grey')) +
  theme_blank() +
  scale_linetype_manual(values = c('solid'='solid', 'dotted'='dotted')) +
  geom_nodetext(aes(label=vertex.names, x=x,y=y), fontface='bold', size =2.5, nudge_y=.025) +
  guides(color="none", fill='none') +
  theme(legend.position = 'bottom')
ggiraph(code={print(plot)}, width =1, height_svg = 7)


plot_rand <- ggplot(senate_gg_rand, aes(x = x, y = y, xend=xend, yend=yend, color = party)) +
  geom_edges(aes(linetype = line), curvature=.3, alpha=.3, size=1, show.legend = FALSE, arrow=arrow(length=unit(6,'pt'), type='closed')) +
  geom_nodes(alpha= .4, aes(fill =party)) +
  scale_color_manual(values=c('Republican'='red')) +
  theme_blank() +
  scale_linetype_manual(values = c('solid'='solid', 'dotted'='dotted')) +
  geom_nodetext(aes(label=vertex.names, x=x,y=y), fontface='bold', size =2.5, nudge_y=.03, nudge_x = .03) +
  geom_text(aes(label ='Ron Johnson', x = 0.72694616, y = 0), fontface='bold', size=2.5, alpha=.6) +
  guides(color="none", fill='none') +
  theme(legend.position = 'bottom')
ggiraph(code={print(plot_rand)})

# senate's centraility 
network_meta <- centr_degree(senate_graph_obj, mode='total')

######################
#  Betweeness 
#  Centrality- Closeness
#  Matrix Plot
######################

senate_closeness <- igraph::closeness(senate_graph_obj,mode='total') %>% 
                    as.data.frame() %>% 
                    tibble::rownames_to_column() 
names(senate_closeness) <- c('Senator', 'Closeness')

senate_closeness %>% 
  arrange(desc(Closeness)) %>%
  head()


senate_btw <- senate_gg %>% 
  distinct(vertex.names, Betweenness)
mean <- mean(senate_btw$Betweenness)
sd <- sd(senate_btw$Betweenness)
senate_btw$btw_std <- (senate_btw$Betweenness - mean)/sd


cls_mean <- mean(senate_closeness$Closeness)
cls_sd <- sd(senate_closeness$Closeness)
senate_closeness$cls_std <- (senate_closeness$Closeness - cls_mean)/cls_sd
summary(senate_btw$btw_std)
matrix_plot <- senate_closeness %>% 
  inner_join(.,senate_btw, by=c('Senator'='vertex.names')) %>% 
  inner_join(.,politicians, by=c('Senator'='name')) %>% 
  arrange(desc(cls_std)) %>% 
  dplyr::mutate(cls_rank = row_number()) %>% 
  arrange(desc(btw_std)) %>% 
  dplyr::mutate(btw_rank = row_number()) %>% 
  ggplot(., aes(x=cls_std, y=btw_std, color=party, 
                tooltip=paste0(Senator, " - ", state, '\n Bipartisan Rank: ', cls_rank, '\n Connections Rank: ', btw_rank))) +
  geom_point(size = 1.5, alpha=.8) +
  theme_blank() +
  scale_color_manual(values=c('Democrat'='dodgerblue', 'Republican'='red', 'Independent'='forestgreen', 'NA'='grey')) +
  xlim(-5,5) +
  ylim(-5,5) +
  geom_vline(xintercept = 0, color='grey50', size=1, alpha= .7) +
  geom_hline(yintercept = 0, color='grey50', size=1, alpha=.7) +
  guides(color='none') +
  geom_text(x=-4,y=.5, label="More Partisanship", color ='grey50', alpha=.5, size = 3.5) +
  geom_text(x=4,y=.5, label ='Less Partisanship', color ='grey50', alpha=.5, size =3.5) +
  geom_text(y=4,x=-1.1, label ='More Connected', color ='grey50', alpha=.5, size =3.5)+
  geom_text(y=-4, x=-1.1, label = 'Less Connected', color ='grey50', alpha=.5, size=3.5) +
  geom_text(x=4, y=-4, label ='Hover Over Points', color= 'lightgray', alpha=.3, size=2.5) +
  geom_point_interactive(alpha=.8, size =1.5)

ggiraph(code={print(matrix_plot)})



######################
#  Explore Tenure
######################

ranks <- read_delim('./data/ranks.csv', col_names=FALSE, delim = '\t')
names(politicians)
install.packages('fuzzyjoin')
library(fuzzyjoin)
ranks <- ranks %>% 
  mutate(rank = row_number()) %>% 
  select(rank, 'name' = X3) %>% 
  mutate(name = stringr::str_replace_all(name, '.[:digit:].', "")) %>% 
  mutate(name = ifelse(name=='Chuck Grassley', 'Charles Grassley', 
                       ifelse(name=='Jim Inhofe', 'James Inhofe', 
                              ifelse(name=='Dick Durbin', 'Richard Durbin', 
                                     ifelse(name=='Jack Reed', 'John Reed', 
                                            ifelse(name =='Mike Enzi', 'Michael Enzi', 
                                                   ifelse(name=='Chuck Schumer', 'Charles Schumer', 
                                                          ifelse(name=='Mike Crapo', 'Michael Crapo' ,name)))))))) %>% 
 mutate(name =ifelse(name =='Tom Carper', 'Thomas Carper', 
                     ifelse(name=='Johnny Isakson', 'John Isakson', 
                            ifelse(name =='Bob Menendez', 'Robert Menéndez', 
                                   ifelse(name=='Bernie Sanders', 'Bernard Sanders', 
                                          ifelse(name=='Ben Cardin', 'Benjamin Cardin', 
                                                 ifelse(name =='Bob Casey','Robert Casey',name ))))))) %>% 
  mutate(name=ifelse(name=='Jim Risch', 'James Risch', 
                     ifelse(name=='Rob Portman', 'Robert Portman', 
                            ifelse(name =='Pat Toomey', 'Patrick Toomey', 
                                   ifelse(name=='Chris Murphy', 'Christopher Murphy', 
                                          ifelse(name=='Shelley Moore Capito', 'Shelley Capito', 
                                                 ifelse(name=='Ben Sasse', 'Benjamin Sasse', 
                                                 ifelse(name=='Maggie Hassan','Margaret Hassan', 
                                                        ifelse(name=='Tina Smith]', 'Tina Smith', 
                                                               ifelse(name=='John Neely Kennedy', 'John Kennedy',name))))))))))
  ranks %>% 
  inner_join(senate_closeness, by=c('name'='Senator')) %>% 
  inner_join(.,senate_btw, by=c('name'='vertex.names')) %>% 
  arrange(desc(cls_std)) %>% 
  dplyr::mutate(cls_rank = row_number()) %>% 
  arrange(desc(btw_std)) %>% 
  dplyr::mutate(btw_rank = row_number()) %>% 
  select(name, rank, btw_rank, cls_rank) %>% 
  filter(btw_rank==1)
  
cor(rank_data$rank, rank_data$btw_rank) #.09
cor(rank_data$rank, rank_data$cls_rank) #.14 

######################
#  Cook PVI
######################


toss_up <- c('Claire McCaskill', 'Bill Nelson', 'Joe Manchin','Joe Donnelly','Heidi Heitkamp')
lean_d <- c('Tina Smith', 'Sherrod Brown')
likely_d <- c('Debbie Stebnow', 'Jon Tester', 'Robert Menéndez','Robert Casey', 'Tammy Baldwin')
solid_d <- c('Dianne Feinstein', 'Christopher Murphy', 'Thomas Carper', 'Mazie Hirono', 
             'Elizabeth Warren', 'Bernard Sanders', 'Benjamin Cardin', 'Angus King', 'Amy Klobuchar', 
             'Martin Heinrich', 'Kirsten Gillibrand', 'Sheldon Whitehouse', 'Timothy Kaine', 'Maria Cantwell')
library(RColorBrewer)
senate_closeness %>% 
  dplyr::mutate(group = ifelse(Senator %in% toss_up, 'Toss Up', 
                        ifelse(Senator %in% lean_d, 'Lean Democrat', 
                               ifelse(Senator %in% likely_d, 'Likely Democrat', 
                                      ifelse(Senator %in% solid_d, 'Solid Democrat', NA))))) %>% 
  filter(!is.na(group)) %>% 
  group_by(group) %>% 
  summarise(bip_rank = median(rank)) %>% 
  mutate(group = factor(group, levels =c('Toss Up', 'Lean Democrat', 'Likely Democrat', 'Solid Democrat'))) %>% 
  ggplot(aes(x=group,y=bip_rank, fill = group, label =bip_rank)) + 
  geom_col(width = .75) +
  geom_text(nudge_y = 2, size = 4, color='grey50', alpha=.9) +
  theme_minimal() +
  ylab('Average Bipartisanship Rank') +
  xlab('') +
  scale_fill_brewer(palette = 'Blues') +
  guides(fill='none') +
  scale_y_continuous(minor_breaks = NULL)






