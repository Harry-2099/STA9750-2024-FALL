library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
setwd("~/R/Project_0/")

# PROFESSORS EXMAMPLE
# get_imdb_file <- function(fname){
#      BASE_URL <- "https://cdmaps.polisci.ucla.edu/"
#     fname_ext <- paste0(fname, ".zip")
#     if(!file.exists(fname_ext)){
#         FILE_URL <- paste0(BASE_URL, fname_ext)
#         download.file(FILE_URL,
#                       destfile = fname_ext)
#     }
#     as.data.frame(readr::read_tsv(fname_ext, lazy=FALSE))
# }

#citation
#MIT Election Data and Science Lab. (2017). U.S. President 1976–2020 [Data set, version 8]. Harvard Dataverse. https://doi.org/10.7910/DVN/42MVDX

### DATA ONE 
house_votes <- read_csv("~/R/Project_0/1976-2022-house (1).csv")
pres <- read_csv("~/R/Project_0/1976-2020-president.csv")
### DATA TWO CONGRESSIONAL BOUNDRIES
library(httr)
for (i in 94:112) {
  BASE_URL <- "https://cdmaps.polisci.ucla.edu/shp/"
  zip_file <- paste0("districts", sprintf("%03d", i), ".zip")  # Adjusts the format to three digits
  # Check if file exists
  if (!file.exists(zip_file)) {
    FILE_URL <- paste0(BASE_URL, zip_file)
    print(FILE_URL)
    # download
    download.file(FILE_URL, destfile = zip_file,mode = "wb")  # Use appropriate method if needed
    
  }
}

### UNZIP FUNCTION- this function reads all the shape files in the directory I believe
library(sf)
unzip_all <- function(filename){
 
   dest_dir <-"~R/Project_0"
  zip_file <- paste0("~/R/Project_0/",filename)
  unzip(zipfile = zip_file,exdir = dest_dir)
  files <- list.files(dest_dir, all.files = TRUE, recursive = TRUE)#recursive allows files to be retrived from the sub directories
  #print(files)
  shp_file <- file.path(dest_dir, files[grepl("\\.shp$",files)])
  dynamic_name <- paste0(filename,"_shp")
  sf <- st_read(shp_file)

  return(sf)
  }

#district_94 <- unzip_shape("districts094.zip")

#### DATA THREE- TIGER DATA CONGRESIONAL BOUNDIRES
#https://www2.census.gov/geo/tiger/TIGER2014/CD/tl_2014_us_cd114.zip

for (i in 2014:2022) {
  BASE_URL <- "https://www2.census.gov/geo/tiger/"
  if (i >= 2018) { #### accounting for changes in the cd#
    file <- paste0("TIGER", sprintf("%d", i), "/CD/tl_", sprintf("%d", i), "_us_cd116.zip")
  } else if (i > 2015) {
    file <- paste0("TIGER", sprintf("%d", i), "/CD/tl_", sprintf("%d", i), "_us_cd115.zip")
  } else {
    file <- paste0("TIGER", sprintf("%d", i), "/CD/tl_", sprintf("%d", i), "_us_cd114.zip")
  }
  download_name <- paste0("TIGER",sprintf("%d",i),".zip")
  # Check if file exists
  if (!file.exists(download_name)) {
    FILE_URL <- paste0(BASE_URL,file)
    print(FILE_URL)
    # download
    download.file(FILE_URL, destfile = download_name,mode = "wb")
  }
}

seats_won <- house_votes %>%
  group_by(state, year,district) %>%
  slice_max(candidatevotes,n = 1) %>% 
  select(year,state,district,party, candidate,candidatevotes)



top_10_parties <-house_votes %>% group_by(party) %>%
  drop_na() %>% 
  summarise(party_count = n()) %>% 
  arrange(desc(party_count)) %>% 
  head(10)
parties <- top_10_parties$party

# house_votes %>% 
#   group_by(year, party) %>% 
#   summarise(party_votes = n())

### TASK 3 PARTY CHANGES AND SEAT CHANGE 
library(ggthemes)
seats_won %>% group_by(year,party) %>%
  drop_na() %>% 
  mutate(party_seats = n()) %>% 
  ggplot(aes(x = year,y = party_seats,color = party))+
  scale_color_manual(
    values= c("DEMOCRAT" = "blue","REPUBLICAN"= "red",
            "DEMOCRATIC-FARMER-LABOR"= "darkgreen","INDEPENDENT"="lightblue"))+
  geom_line(linewidth = 1.2)+
  theme_minimal()+
  theme(legend.position = "bottom",legend.text = element_text(size = 5))+
  labs(title = "Party Seats by Year ", x = "Year", y= "Number of Seats ", color = "Party")

house_votes %>% group_by(year,state) %>% 
  mutate(num_seats = n_distinct(district)) %>%#couting the seats via district
  ungroup() %>% 
  select(year,state,num_seats) %>%
  filter(year== 2022 |year ==1976) %>%
  group_by(state) %>% 
  mutate(diff_seats = num_seats - lag(num_seats)) %>% # subtracting the newer value from the older via lag function
  filter(year==2022) %>% # this gives up the "lagged" values since the older values will not have a diff via the lag method
  distinct() %>% 
  arrange(desc(abs(diff_seats))) %>% #ordering by biggest differece by absoulte value
  head(25) %>% 
  #ploting above
  ggplot(aes(x = state,y = diff_seats,fill = state))+
  geom_bar(stat = "identity",width = 1)+
  theme_clean()+
  theme(legend.position = "FALSE")+
  labs(title= "Changes in Seats of the House of Representatives by State", 
       x= "State", y = "Change in Seats")+
  coord_flip()

#### TASK 3 FUSION SYSTEM

fusion <- house_votes %>% 
  filter(fusion_ticket == TRUE & candidate != "BLANK") %>% 
  select(year,candidate,candidatevotes,district,party,totalvotes) %>% 
  group_by(year,district,candidate) %>% 
  mutate(total_fusion_votes =sum(candidatevotes)) %>% 
  ungroup() %>% 
  group_by(year,district) %>% 
  mutate(highest_single_party_votes = candidate[which.max(candidatevotes)]) %>% 
  ungroup()
 
fusion_winners <- fusion %>%
  group_by(year,district) %>% 
  mutate(district_winner = candidate[which.max(total_fusion_votes)]) %>%
  select(year,district,district_winner,highest_single_party_votes) %>% 
  filter(district_winner!=highest_single_party_votes) %>% 
  distinct() %>% 
  DT::datatable()


### TASK 3 CONGRESSIONAL CANDIDATES VS PRESIDENTIAL 
house_winners <- house_votes %>% 
  group_by(year,district,state,candidate) %>% 
  mutate(cong_votes=sum(candidatevotes)) %>% 
  ungroup %>% group_by(year,district,state) %>% 
  mutate(cong_winner = candidate[which.max(cong_votes)]) %>% ungroup() %>% 
  filter(cong_winner == candidate) %>% 
  select(year,party,state,state_fips,district,cong_votes,cong_winner) %>% 
  ungroup()

 
pres %>% 
  inner_join(house_winners, by = c("state_fips","party_detailed" = "party","state","year")) %>% 
  group_by(state,candidate,year,cong_winner) %>% 
  mutate(vote_diff = candidatevotes - cong_votes) %>% 
  ungroup() %>% group_by(year) %>%  
  mutate(avg_diff = mean(vote_diff)) %>% 
  select(year,state,state_fips,party_detailed,candidate,cong_winner,district,vote_diff,avg_diff) %>% 
  ggplot(aes(x = year, y = avg_diff))+
  geom_line(linewidth = 1.2, color = "red")+
  theme_minimal()+
  labs(title = "Average Difference Between Congressional Candidate Votes & Presidential Votes",
       x = "Year", y = "Average Difference")

####IMPORTING SHAPE FILE TASK 4
library(sf)

if(!file.exists("nyc_borough_boundaries.zip")){
    download.file("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Shapefile", 
              destfile="nyc_borough_boundaries.zip")
}

##-
unzip_shape <- function(filename){
 
  dest_dir <- tempdir()
  file <- unzip(zipfile = filename,exdir = dest_dir)
  shp_file <- file[grepl("\\.shp$",file)]
  sf <- read_sf(shp_file)
  return(sf)
  }

### UNZIPPIING AND PLOTTING
nyc_sf <- unzip_shape("nyc_borough_boundaries.zip")
nyc_sf
# ggplot(nyc_sf, 
#        aes(geometry=geometry, 
#            fill = shape_area)) + 
#     geom_sf()

###TASK 5
library(tools)
bushVgore <- unzip_shape("tl_2023_us_state.zip")

# ggplot(bushVgore, 
#        aes(geometry=geometry)) + 
#     geom_sf()

bushVgore <- bushVgore %>% select(NAME,geometry)

election_2000 <- pres %>% 
  filter(year == 2000 & candidate == "BUSH, GEORGE W."| candidate=="GORE, AL") %>% 
  mutate(state = toTitleCase(tolower(state))) %>% 
  select(state,state_po,candidate,candidatevotes,state_fips,totalvotes) %>% 
  inner_join(bushVgore, by = c("state"="NAME")) %>% 
  group_by(state) %>% 
  mutate(state_winner = candidate[which.max(candidatevotes)])
color <- c("BUSH, GEORGE W." = "red","GORE, AL"="blue")
main_land <- election_2000 %>% 
  filter(!state %in% c("Hawaii", "Alaska")) %>% 
  ggplot(aes(geometry = geometry, fill = state_winner)) +
  scale_fill_manual(values = color) + 
  geom_sf() +
  labs(fill = "Candidate") + 
  geom_sf_text(aes(label = state_po), size = 2.5, color = "white",fontface = "bold")+
  theme_void()
#main_land

hawaii <- election_2000 %>% filter(state == "Hawaii") %>% 
  ggplot(aes(geometry = geometry,fill = state_winner))+
  scale_fill_manual(values = color)+
  geom_sf()+
  labs(fill = "Candidate") +
   coord_sf(crs = st_crs(4326)) +
  theme_void()+
  geom_sf_text(aes(label = state_po), size = 1, color = "white",fontface = "bold")+
  theme(legend.position = "FALSE")
#hawaii

alaska <- election_2000 %>%  filter(state =="Alaska") %>% 
  ggplot(aes(geometry = geometry,fill = state_winner))+
  scale_fill_manual(values = color)+
  geom_sf()+
  labs(fill = "Candidate") +
    coord_sf(crs = st_crs(4326)) +
  theme_void()+
  geom_sf_text(aes(label = state_po), size = 2, color = "white",fontface = "bold")+
  theme(legend.position = "FALSE")
#alaska

# library(parallel)
# options(mc.cores = NULL)

library(cowplot)
library(gganimate)
ggdraw()+
  draw_plot(main_land)+
  draw_plot(hawaii, x= .1,width = 0.15, height = 0.15)+
  draw_plot(alaska,y = -.25)


parties <- pres %>%
  mutate(state = toTitleCase(tolower(state))) %>% 
  filter(state !="Hawaii" & state != "Alaska" & year >= 2008) %>% 
  inner_join(bushVgore, by = c("state"="NAME")) %>% #bushvgore is the shape file
  group_by(state,year) %>%
  mutate(state_winner = party_detailed[which.max(candidatevotes)]) %>%
  ungroup()

### facetmap
library(tmap)
tmap_mode("plot")
parties %>% 
  st_as_sf() %>% 
  tm_shape() +
  tm_polygons("state_winner", title = "Party", 
              palette = c("REPUBLICAN" = "red", 
                          "DEMOCRAT" = "blue",
                          "DEMOCRATIC-FARMER-LABOR" = "lightblue")) +
  tm_facets("year", free.scales = TRUE) +
  tm_text("state_po", size = .5, col = "white", fontface = "bold")+
  tm_layout(main.title = "Statewide Party Preferences During Presidential Elections(2008-2020)")

###ANIMATING THE FACET
anim <- parties %>% 
  filter(year >= 2000) %>% 
  st_as_sf() %>% 
  tm_shape() +
  tm_polygons("state_winner", title = "Party", 
              palette = c("REPUBLICAN" = "red", 
                          "DEMOCRAT" = "blue",
                          "DEMOCRATIC-FARMER-LABOR" = "lightblue")) +
  tm_facets(along = "year", free.coords = FALSE)+
  tm_text("state_po", size = .3, col = "white", fontface = "bold")
 
#anim_map<- tmap_animation(anim,"anim_map.gif") 
 tmap_animation(anim,restart.delay = 75)

#### ELECTORAL TASK
ECV <- election_2000 %>%
  filter(candidate %in%c("BUSH, GEORGE W.","GORE, AL") & state !="District of Columbia") %>% 
  select(state,candidate,candidatevotes,totalvotes) %>%
  group_by(state) %>% 
  mutate(WTall = candidate[which.max(candidatevotes)]) %>% 
  mutate(bush_prop = case_when(
      candidate == "BUSH, GEORGE W." ~ candidatevotes / totalvotes,TRUE ~NA_real_),
      gore_prop = 1-bush_prop) %>%
  ungroup() %>% group_by(WTall) %>% 
  mutate(national_cand_votes = sum(candidatevotes)) %>% 
  ungroup() %>% 
  mutate(total_national_votes = sum(candidatevotes)) %>% 
  group_by(WTall) %>% 
  mutate(national_prop = (national_cand_votes/total_national_votes)) %>% 
  ungroup()

ECV <- ECV %>% 
  select(-candidate) %>% 
  filter(!is.na(bush_prop) &!is.na(gore_prop)) %>% 
  distinct(state, .keep_all = TRUE) %>% 
  mutate(bush_WTall = sum(WTall == "BUSH, GEORGE W."),
         gore_WTall = sum(WTall == "GORE, AL")) %>% 
  mutate(bush_state_prop_win = case_when(bush_prop>gore_prop ~1,is.na(bush_prop) ~ NA_real_, TRUE~0)) %>% 
  mutate(num_SPW_bush =case_when(bush_state_prop_win ==1 ~ sum(bush_state_prop_win ==1),TRUE~NA_real_),#state proportional wins
         num_SPW_gore = case_when(bush_state_prop_win == 0 ~ sum(bush_state_prop_win ==0), TRUE~ NA_real_)) %>% 
  select(-1,-2,-3, -bush_prop, -gore_prop, -bush_state_prop_win) %>% 
  distinct(WTall,.keep_all = TRUE) %>% 
  rename(cand = WTall) %>%
  DT::datatable()
ECV



ECV

data(World, metro)

  
m3 <- lapply(seq(50, 85, by = 5), function(age) {
	World$at_most <- World$life_exp <= age
	World_sel <- World[which((World$life_exp <= age) & (World$life_exp > (age - 5))), ]
	tm_shape(World) +
		tm_polygons("at_most", palette = c("gray95", "gold"), legend.show = FALSE) +
		tm_shape(World_sel) +
		tm_text("name", size = "AREA", root = 5, remove.overlap = TRUE) +
		tm_layout(main.title = paste0("Life expectency at most ", age), frame = FALSE)
})
 
tmap_animation(m3, width = 1200, height = 600, delay = 100)

