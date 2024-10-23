library(dplyr)
library(tidyr)
library(tidyverse)
get_imdb_file <- function(fname){
    BASE_URL <- "https://datasets.imdbws.com/"
    fname_ext <- paste0(fname, ".tsv.gz")
    if(!file.exists(fname_ext)){
        FILE_URL <- paste0(BASE_URL, fname_ext)
        download.file(FILE_URL, 
                      destfile = fname_ext)
    }
    as.data.frame(readr::read_tsv(fname_ext, lazy=FALSE))
}

## loading the imdb files ###
NAME_BASICS  <- get_imdb_file("name.basics")
TITLE_BASICS     <- get_imdb_file("title.basics")
TITLE_EPISODES   <- get_imdb_file("title.episode")
TITLE_RATINGS    <- get_imdb_file("title.ratings")
TITLE_CREW       <- get_imdb_file("title.crew")
TITLE_PRINCIPALS <- get_imdb_file("title.principals")

head(TITLE_PRINCIPALS)
#### Data Sub-Sampling -- bring the size down to allow for workable datasize ####
library(stringr)

NAME_BASICS <- NAME_BASICS |> 
  filter(str_count(knownForTitles, ",") > 1)

TITLE_RATINGS |>
    ggplot(aes(x=numVotes)) + 
    geom_histogram(bins=30) +
    xlab("Number of IMDB Ratings") + 
    ylab("Number of Titles") + 
    ggtitle("Majority of IMDB Titles Have Less than 100 Ratings") + 
    theme_bw() + 
    scale_x_log10(label=scales::comma) + 
    scale_y_continuous(label=scales::comma)
#removing those titles that have less than 100 ratings
TITLE_RATINGS |>
    pull(numVotes) |>
    quantile() 

TITLE_RATINGS <- TITLE_RATINGS |>
    filter(numVotes >= 100)
# now using this to reduced date to filter all the other tables with semi_join 
TITLE_BASICS <- TITLE_BASICS |>
    semi_join(TITLE_RATINGS, 
              join_by(tconst == tconst))

TITLE_CREW <- TITLE_CREW |>
    semi_join(TITLE_RATINGS, 
              join_by(tconst == tconst))

TITLE_EPISODES_1 <- TITLE_EPISODES |>
    semi_join(TITLE_RATINGS, 
              join_by(tconst == tconst))
TITLE_EPISODES_2 <- TITLE_EPISODES |>
    semi_join(TITLE_RATINGS, 
              join_by(parentTconst == tconst))

TITLE_EPISODES <- bind_rows(TITLE_EPISODES_1,
                            TITLE_EPISODES_2) |>
    distinct()

TITLE_PRINCIPALS <- TITLE_PRINCIPALS |>
    semi_join(TITLE_RATINGS, join_by(tconst == tconst))



rm(TITLE_EPISODES_1)
rm(TITLE_EPISODES_2)

### Cleaning the data ####
glimpse(TITLE_BASICS)
glimpse(TITLE_EPISODES)

NAME_BASICS <- NAME_BASICS |>
    mutate(birthYear = as.numeric(birthYear),
           deathYear = as.numeric(deathYear)) # using the as numeric to force non-numeric into actual N/A instead of the \\N.R provides by the data

TITLE_BASICS <- TITLE_BASICS |>
    mutate(startYear = as.numeric(startYear),
           endYear = as.numeric(endYear),
           isAdult = as.logical(isAdult))
TITLE_EPISODES <- TITLE_EPISODES |>
    mutate(seasonNumber = as.numeric(seasonNumber),
           episodeNumber = as.numeric(episodeNumber))
#### Separating the values in the cells by the comma to further clean the data ####
glimpse(NAME_BASICS)
#seperating the knownfor col
NAME_BASICS <- NAME_BASICS |> separate_longer_delim(knownForTitles, ",")
 #### TASK 2 ####
  #How many movies are in our data set? How many TV series? How many TV episodes?
  
  unique(TITLE_BASICS$titleType)
  ### oldest living person in the date
  
  oldies<-NAME_BASICS %>% filter(is.na(deathYear),!is.na(birthYear), 2024-birthYear < 101) %>% arrange(birthYear) %>% head(10)
  
  #There is one TV Episode in this data set with a perfect 10/10 rating and at least 200,000 IMDb ratings. What is it? What series does it belong to?
               
series_name <- TITLE_EPISODES %>% 
  inner_join(TITLE_BASICS, by = c('parentTconst' = 'tconst')) %>% 
  filter(titleType == "tvSeries") %>% 
  select(parentTconst,originalTitle) %>% 
  distinct(parentTconst, .keep_all = TRUE)
  
episodes_ranked <- TITLE_EPISODES %>% 
  left_join(TITLE_BASICS, by = c('tconst' = 'tconst')) %>% 
  inner_join(TITLE_RATINGS, by = c('tconst' = 'tconst')) %>%
  inner_join(series_name, by = "parentTconst") %>% 
  filter(numVotes > 200000 & averageRating == 10) %>% 
  rename(show_name = originalTitle.y)
#What four projects is the actor Mark Hamill most known for?####

  NAME_BASICS |> 
  separate_longer_delim(knownForTitles, ",") %>% 
  filter(primaryName == "Mark Hamill") %>%
  inner_join(TITLE_BASICS,by = c('knownForTitles' = 'tconst')) %>% 
  select(primaryName,primaryTitle)
#What TV series, with more than 12 episodes, has the highest average rating?
 tv_ranked<- TITLE_EPISODES %>%
   inner_join(TITLE_RATINGS, by = c('parentTconst'='tconst')) %>%
   inner_join(TITLE_BASICS, by = c('parentTconst' = 'tconst')) %>% 
   group_by(parentTconst) %>% 
   filter(n()>12) %>% 
   ungroup() %>% 
   select(primaryTitle,averageRating) %>%
   distinct(primaryTitle, .keep_all = TRUE) %>% 
   mutate(rank = dense_rank(desc(averageRating))) %>% 
   arrange(rank)
                 
#Is it true that episodes from later seasons of Happy Days have lower average ratings than the early seasons?
happy_days<- TITLE_EPISODES %>%
  inner_join(TITLE_RATINGS, by = c('tconst'='tconst')) %>%
  inner_join(TITLE_BASICS, by = c('parentTconst' = 'tconst')) %>% 
  filter(primaryTitle == 'Happy Days') %>% 
  mutate(season_episode= str_c(seasonNumber,episodeNumber, sep = ",")) %>%
  select(averageRating,season_episode,seasonNumber,episodeNumber) %>% 
  arrange(seasonNumber,episodeNumber) %>% 
  group_by(seasonNumber) %>% 
  mutate(season_average = mean(averageRating))

ggplot(happy_days, aes(x = season_episode, y = averageRating)) +
  geom_point(size = 2.5,color = "purple") +
  labs(title = "Average Ratings of Happy Days Episodes",
       y = "Average Rating", x = "Show Timeline") +
  theme_bw() +
  theme(axis.text.x = element_blank())

  ggplot(happy_days, aes(x = seasonNumber, y = season_average)) +
  geom_point(size = 2.5, color = "purple") +
  geom_smooth(se = FALSE)+
  labs(title = "Average Ratings of Happy Days by Season",
       y = "Average Rating", x = "Season Number") +
  theme_bw()
#animations
smooth_vals = predict(loess(season_average~seasonNumber,happy_days))

 ggplot(happy_days, aes(x = seasonNumber, y = season_average)) +
  geom_point(size = 4, color = "purple") +
  geom_line(aes(y = smooth_vals), colour = "darkblue",linewidth = 2,linetype = 'dotdash') +
  labs(title = "Average Ratings of Happy Days by Season", y = "Average Rating", x = "Season Number") +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  transition_reveal(seasonNumber) +
  ease_aes('linear')

 TITLE_BASICS <- TITLE_BASICS %>% 
  inner_join(TITLE_RATINGS,by = 'tconst')
 ## creating a metric
mean(TITLE_BASICS$numVotes)
popular_titles <- TITLE_BASICS %>% 
  filter(numVotes > 500)
popular_titles <- popular_titles %>% mutate(vote_bin = ntile(numVotes, n=10))

popular_titles %>% filter(vote_bin == 10) %>% head(10) %>% arrange(numVotes)

popular_titles <- popular_titles %>% 
  mutate(CLI = log10(averageRating)*vote_bin)

NAMES <- NAME_BASICS |> 
  separate_longer_delim(knownForTitles, ",") %>% 
  select(knownForTitles,primaryName)

popular_titles <-popular_titles %>%
  left_join(NAMES,by=c('tconst'='knownForTitles')) %>% 
  select(-c(10,11,12,13))

popular_titles %>%
  filter(primaryName == "Robert De Niro") %>% 
  select(averageRating,CLI,OriginalTitle,primaryName)
#Task 4: Trends in Success Over Time

recent <- popular_titles %>% 
  filter(startYear > 1959) %>%
  mutate(decade = floor(as.numeric(startYear) / 10) * 10) %>% 
  select(-primaryName.y) %>% 
  rename(actor = primaryName.x)

decade <-recent %>% 
 filter(CLI > 8) %>% 
 group_by(genres,decade) %>% 
 group_by(decade, genres) %>%  
 summarise(num_success = n()) %>%
 slice_max(num_success, n = 1) %>%  
 arrange(decade)

  
  
  
  
  
  
  
  
  
 