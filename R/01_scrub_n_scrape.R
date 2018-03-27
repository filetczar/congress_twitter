#################################
#  Project: Polticians & Twitter
#  Date: March 2018
###############################
rm(list=ls())
install.packages('rtweet')
library(rtweet)
library(tidyverse)
library(stringr)
library(readr)

#######################
#  LOAD DATA
######################

politicians <- read_csv('/Users/acazar/Desktop/blog/projects/congress_twitter/congress.csv')


#######################
#  MISSING 
######################

politicians <- politicians %>% 
  dplyr::filter(is.na(twitter) == FALSE)
# 13 missing 
# impute missing 
impute_twitter <- function(first, last, twitter) { 
  politicians[which(politicians$first_name == first & politicians$last_name == last), 'twitter'] <<- twitter
}

impute_twitter(first='Amy', last = 'Klobuchar', twitter='amyklobuchar')
impute_twitter(first='Justin', last='Amash', twitter='justinamash')
impute_twitter(first='Bill', last='Cassidy', twitter ='BillCassidy')
impute_twitter(first = 'Wm.', 'Clay', twitter = 'LacyClayMO1')
impute_twitter(first='Rand', last='Paul', twitter = 'DrRandPaul')
impute_twitter(first='Timothy', last='Kaine', twitter='timkaine')
impute_twitter(first='James', last='Comer', twitter='KYComer')
impute_twitter(first='Greg', last='Gianforte', twitter='gianforte')
impute_twitter(first='Karen', last ='Handel', twitter='RepKHandel')
impute_twitter(first='John', last = 'Curtis', twitter = 'CurtisUT')

# 3 No Twitter Accounts: Madeleine Bordallo, Collin Peterson, Gregorio Sablan 

# Look for campaign specific accounts
politicians %>% 
  dplyr::filter(str_detect(twitter, '.for.')==TRUE |
                  str_detect(twitter, '.campaign.') == TRUE |
                  str_detect(twitter,'.official.') == TRUE) %>% 
  dplyr::select(first_name, last_name, party, twitter)

######################
#  Get Twitter IDs
######################

for(i in 1:nrow(politicians)) { 
  if(!is.na(politicians[i,'twitter'])) {
    politicians[i,'twitter_id'] = rtweet::lookup_users(users = as.character(politicians[i,'twitter']))$user_id
  }
}

######################
# QA
######################

check <- lists_members(slug='members-of-congress', owner_user='CSPAN')

check %>% 
  group_by(name) %>% 
  summarise(rws=n()) %>% 
  arrange(desc(rws))

table(check$user_id %in% politicians$twitter_id)

qa <- politicians %>% 
      dplyr::left_join(.,check, by=c('twitter_id'='user_id')) %>% 
      dplyr::select(first_name, last_name, twitter, name) %>% 
      dplyr::filter(is.na(name))
View(qa)

######################
#  IMPUTE MIS ID's
######################
impute_twitter(first='Christopher', last = 'Murphy', twitter='ChrisMurphyCT')
impute_twitter(first='Marco', last='Rubio', twitter='marcorubio')
impute_twitter(first='Earl', last='Blumenauer', twitter='repblumenauer')
impute_twitter(first='Claire', last='McCaskill', twitter = 'clairecmc')
impute_twitter(first='Brian', last='Schatz', twitter='brainschatz')
impute_twitter(first='Seth', last='Moulton', twitter='sethmoulton')

######################
#  RECOLLECT IDs
######################

for(i in 1:nrow(politicians)) { 
  if(!is.na(politicians[i,'twitter'])) {
    politicians[i,'twitter_id'] = rtweet::lookup_users(users = as.character(politicians[i,'twitter']))$user_id
  }
}




######################
# SCRAPE 
######################


scraper <- function(twitter_id, pol_df = politicians) {
  following <- rtweet::get_friends(twitter_id, parse=TRUE, n =10000) %>% 
  dplyr::inner_join(., pol_df, by =c('user_id'='twitter_id')) %>% 
  dplyr::mutate(target = stringr::str_c(first_name, last_name, sep = " ")) %>% 
  dplyr::select(user,target) %>% 
  dplyr::inner_join(.,pol_df, by=c('user'='twitter_id')) %>% 
  dplyr::mutate(source = stringr::str_c(first_name, last_name, sep = " ")) %>% 
  dplyr::select(source,target)
return(following)
}

check %>% 
  filter(friends_count==0) %>% 
  select(user_id)
# hard code users who follow zero peaple

  Sys.time()
  z =0
  edges_list <- list()
  for(i in 1:nrow(politicians)){
    twitter_id <- as.character(politicians[i,'twitter_id'])
    if(twitter_id == '50152441' || twitter_id== '752364246218862592' || is.na(twitter_id)) {next} #follow 0
    edges_list[[i]] <- scraper(twitter_id=twitter_id)
    z = z+1
    if(z %% 15 == 0) {
      print(paste0('Sleeping at ', Sys.time(), ' iteration: ', i)) # Sleep every 15 
      system('sleep 905')
    }
  }
  print(paste0('Loop ended at ',Sys.time(), ' there are ', length(edges_list) , ' observations'))

saveRDS(edges_list,file='/Users/acazar/Desktop/blog/projects/congress_twitter/edges_list.rds')
Sys.time()