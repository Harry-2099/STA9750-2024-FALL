setwd("~/R/Project_0")
#### PACKAGES ######
library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse,ggplot2)
library(janitor)
library(tidygeocoder)
library(tools)
library(leaflet)
library(tmap)
library(sf)
library(RColorBrewer)
library(tidycensus,tidyverse)
census_api_key("37c6251bf49ce45381267b45c7ce1149844ac8ce",install = TRUE)
acs_vars <- load_variables(2022, "acs5", cache = TRUE)
unemployed<- acs_vars %>% filter(grepl(".unemployed.*",label, ignore.case = TRUE))

by_race <- read_csv("Unemployment rate by sex, race and Hispanic ethnicity.csv")
by_education <- read_csv("Unemployment rates for people 25 years and older by educational attainment and sex.csv")
head(by_race)

#fred api unemployment information
install.packages("fredr")
library(fredr)
fredr_set_key(read_lines("FRED_key.txt"))
search_results2 <- fredr_series_search_text("unemployment rate state") 
### Another data set



###FROM MIGUEL ####
# Initialize data frame
ny_male_population_df <- data.frame()

# Loop through years
for (year in 2005:2019) {
  # Retrieve data
  male_population <- get_acs(
    geography = "state",
    state = "NY",
    variables = "B03002_002",
    year = year,
    survey = "acs1"
  )
  
  # Check for errors
  if (nrow(male_population) > 0) {
    # Add year column
    male_population$year <- year
    
    # Bind rows to data frame
    ny_male_population_df <- bind_rows(ny_male_population_df, male_population)
  } else {
    print(paste("No data available for", year))
  }
}

# Initialize data frame
ny_female_population_df <- data.frame()

# Loop through years
for (year in 2005:2019) {
  # Retrieve data
  female_population <- get_acs(
    geography = "state",
    state = "NY",
    variables = "B03002_003",
    year = year,
    survey = "acs1"
  )
  
  # Check for errors
  if (nrow(female_population) > 0) {
    # Add year column
    female_population$year <- year
    
    # Bind rows to data frame
    ny_female_population_df <- bind_rows(ny_female_population_df, female_population)
  } else {
    print(paste("No data available for", year))
  }
}

glimpse(ny_female_population_df)

# Initialize data frame
ny_white_population_df <- data.frame()

# Loop through years
for (year in 2005:2019) {
  # Retrieve data
  white_population <- get_acs(
    geography = "state",
    state = "NY",
    variables = "B02001_002",
    year = year,
    survey = "acs1"
  )
  
  # Check for errors
  if (nrow(white_population) > 0) {
    # Add year column
    white_population$year <- year
    
    # Bind rows to data frame
    ny_white_population_df <- bind_rows(ny_white_population_df, white_population)
  } else {
    print(paste("No data available for", year))
  }
}

# Initialize data frame
ny_black_population_df <- data.frame()

# Loop through years
for (year in 2005:2019) {
  # Retrieve data
  black_population <- get_acs(
    geography = "state",
    state = "NY",
    variables = "B02001_002",
    year = year,
    survey = "acs1"
  )
  
  # Check for errors
  if (nrow(black_population) > 0) {
    # Add year column
    black_population$year <- year
    
    # Bind rows to data frame
    ny_black_population_df <- bind_rows(ny_black_population_df, black_population)
  } else {
    print(paste("No data available for", year))
  }
}

# Initialize data frame
ny_hispanic_population_df <- data.frame()

# Loop through years
for (year in 2005:2019) {
  # Retrieve data
  hispanic_population <- get_acs(
    geography = "state",
    state = "NY",
    variables = "B03001_001",
    year = year,
    survey = "acs1"
  )
  
  # Check for errors
  if (nrow(hispanic_population) > 0) {
    # Add year column
    hispanic_population$year <- year
    
    # Bind rows to data frame
    ny_hispanic_population_df <- bind_rows(ny_hispanic_population_df, hispanic_population)
  } else {
    print(paste("No data available for", year))
  }
}

# Initialize data frame
ny_black_population_df <- data.frame()

# Loop through years
for (year in 2005:2019) {
  # Retrieve data
  black_population <- get_acs(
    geography = "state",
    state = "NY",
    variables = "B02001_002",
    year = year,
    survey = "acs1"
  )
  
  # Check for errors
  if (nrow(black_population) > 0) {
    # Add year column
    black_population$year <- year
    
    # Bind rows to data frame
    ny_black_population_df <- bind_rows(ny_black_population_df, black_population)
  } else {
    print(paste("No data available for", year))
  }
}

# Initialize data frame
ny_asian_population_df <- data.frame()

# Loop through years
for (year in 2005:2019) {
  # Retrieve data
  asian_population <- get_acs(
    geography = "state",
    state = "NY",
    variables = "B01001D_001",
    year = year,
    survey = "acs1"
  )
  
  # Check for errors
  if (nrow(asian_population) > 0) {
    # Add year column
    asian_population$year <- year
    
    # Bind rows to data frame
    ny_asian_population_df <- bind_rows(ny_asian_population_df, asian_population)
  } else {
    print(paste("No data available for", year))
  }
}

# Initialize data frame
less_than_hs_population_df <- data.frame()

# Loop through years
for (year in 2005:2019) {
  # Retrieve data
  less_than_hs_population <- get_acs(
    geography = "state",
    state = "NY",
    variables = "B06009_002",
    year = year,
    survey = "acs1"
  )
  
  # Check for errors
  if (nrow(less_than_hs_population) > 0) {
    # Add year column
    less_than_hs_population$year <- year
    
    # Bind rows to data frame
    less_than_hs_population_df <- bind_rows(less_than_hs_population_df, less_than_hs_population)
  } else {
    print(paste("No data available for", year))
  }
}

# Initialize data frame
hs_population_df <- data.frame()

# Loop through years
for (year in 2005:2019) {
  # Retrieve data
  hs_population <- get_acs(
    geography = "state",
    state = "NY",
    variables = "B06009_003",
    year = year,
    survey = "acs1"
  )
  
  # Check for errors
  if (nrow(hs_population) > 0) {
    # Add year column
    hs_population$year <- year
    
    # Bind rows to data frame
    hs_population_df <- bind_rows(hs_population_df, hs_population)
  } else {
    print(paste("No data available for", year))
  }
}

#some college or associates degree
# Initialize data frame
some_college_population_df <- data.frame()

# Loop through years
for (year in 2005:2019) {
  # Retrieve data
  some_college_population <- get_acs(
    geography = "state",
    state = "NY",
    variables = "B06009_004",
    year = year,
    survey = "acs1"
  )
  
  # Check for errors
  if (nrow(some_college_population) > 0) {
    # Add year column
    some_college_population$year <- year
    
    # Bind rows to data frame
    some_college_population_df <- bind_rows(some_college_population_df, some_college_population)
  } else {
    print(paste("No data available for", year))
  }
}

# Initialize data frame
bachelors_population_df <- data.frame()

# Loop through years
for (year in 2005:2019) {
  # Retrieve data
  bachelors_population <- get_acs(
    geography = "state",
    state = "NY",
    variables = "B06009_005",
    year = year,
    survey = "acs1"
  )
  
  # Check for errors
  if (nrow(bachelors_population) > 0) {
    # Add year column
    bachelors_population$year <- year
    
    # Bind rows to data frame
    bachelors_population_df <- bind_rows(bachelors_population_df, bachelors_population)
  } else {
    print(paste("No data available for", year))
  }
}

#Graduate or professional degree 
# Initialize data frame

graduate_population_df <- data.frame()

# Loop through years
for (year in 2005:2019) {
  # Retrieve data
  graduate_population <- get_acs(
    geography = "state",
    state = "NY",
    variables = "B06009_005",
    year = year,
    survey = "acs1"
  )
  
  # Check for errors
  if (nrow(graduate_population) > 0) {
    # Add year column
    graduate_population$year <- year
    
    # Bind rows to data frame
    graduate_population_df <- bind_rows(graduate_population_df, graduate_population)
  } else {
    print(paste("No data available for", year))
  }
}
#Creating a table for the regression model
fredr_set_key(read_lines("FRED_key.txt"))

ny_unrate <- fredr(
  series_id = "NYUR",
  observation_start = as.Date("2005-01-01"),
  observation_end = as.Date("2019-12-31"),
  frequency = "a"  # monthly
)

ny_unrate$year <- as.integer(format(ny_unrate$date, "%Y"))


regression_model_table <- ny_unrate |>
  left_join(ny_female_population_df |>
              select(female_pop = estimate, year), join_by(year == year)) |>
  select(-date)



regression_model_table <- ny_unrate |>
  select(year, unemployment_rate = value) |>
  left_join(ny_female_population_df |>
              select(female_pop = estimate, year), 
            by = "year") |>
  left_join(ny_male_population_df |>
              select(male_pop = estimate, year),
            by = "year") |>
  left_join(less_than_hs_population_df |>
              select(less_than_hs_pop = estimate, year),
            by = "year") |>
  left_join(hs_population_df |>
              select(hs_pop = estimate, year),
            by = "year") |>
  left_join(some_college_population_df |>
              select(some_college_pop = estimate, year),
            by = "year") |>
  left_join(bachelors_population_df |>
              select(bachelors_pop = estimate, year),
            by = "year") |>
  left_join(graduate_population_df |>
              select(graduate_pop = estimate, year),
            by = "year") |>
  left_join(ny_asian_population_df |>
              select(ny_asian_pop = estimate, year),
            by = "year") |>
  left_join(ny_white_population_df |>
              select(ny_white_pop = estimate, year),
            by = "year") |>
  left_join(ny_black_population_df |>
              select(ny_black_pop = estimate, year),
            by = "year") |>
  left_join(ny_hispanic_population_df |>
              select(ny_hispanic_pop = estimate, year),
            by = "year")



#### REGRESSION TABLE ANALYSIS#####
regression_model_table %>% 
  select(-year) %>%
  ggplot(aes(x = graduate_pop,y = unemployment_rate))+
  geom_point()
### creating a proportions for the data instead of actual estimates
regression_table_prop <- regression_model_table %>%
  group_by(year) %>%
  mutate(
    total_pop_yr = rowSums(across(c(female_pop, male_pop, less_than_hs_pop, 
                                    hs_pop, some_college_pop, bachelors_pop, 
                                    graduate_pop, ny_asian_pop, ny_white_pop, 
                                    ny_black_pop, ny_hispanic_pop))),
    across(c(female_pop, male_pop, less_than_hs_pop, hs_pop, 
             some_college_pop, bachelors_pop, graduate_pop, 
             ny_asian_pop, ny_white_pop, ny_black_pop, 
             ny_hispanic_pop), ~ .x / total_pop_yr, .names = "{.col}_prop")
  ) %>%
  select(year, unemployment_rate, ends_with("_prop")) %>%
  ungroup()

regression_table_prop %>%
  ggplot(aes(x = year,y =  unemployment_rate))+
  geom_point()

## seems like there is a correlation between the years due to econmic downtown in those years so we will remove them and see what happens
prop_df <- regression_table_prop %>% 
  arrange(desc(unemployment_rate)) %>% 
    slice(-(1:5))

prop_df%>%
  ggplot(aes(x = graduate_pop_prop,y =  unemployment_rate))+
  geom_point()
### doesnt really work
### trying detrending

model <- lm(unemployment_rate ~ year, data = regression_model_table)
#using the residuals as a form of detrending 

detrend_df <- regression_table_prop %>%
  mutate(detrended_unemployment_rate = exp(residuals(model)))

detrend_df %>% 
  ggplot(aes(x = less_than_hs_pop_prop,y =  detrended_unemployment_rate))+
  geom_point()+
   geom_smooth(method = "loess", se = TRUE, color = "blue")


summary(lm(detrended_unemployment_rate ~ 
             less_than_hs_pop_prop + 
             I(less_than_hs_pop_prop^2) + 
             graduate_pop_prop+
             I(graduate_pop_prop^2),
             data = detrend_df))

### KAGGLE DATA WORK #####
library(lubridate)
library(gganimate)
by_demo <- read_csv("unemployment_data_us.csv") %>% 
  mutate(Date = my(Date)) %>% 
  select(-Month,-Year) %>% 
  drop_na()

demo_long <- by_demo %>%
  pivot_longer(cols = -Date,
               names_to = "demographic",
               values_to = "unemployment_rate"
               )

race <- c("Asian","Black","Hispanic","White")
gender <-c("Men","Women")
education <- c("High_School","Primary_School","Professional_Degree","Associates_Degree")

race_anim<-demo_long %>%
  filter(demographic %in% race) %>%
  ggplot(aes(x = Date, y = unemployment_rate, color = demographic)) +
  geom_point()+
  theme_minimal()+
  transition_time(Date)+
  shadow_mark()+
  labs(y = "Unemploment Rate",x = "Date", color = "Race")+
  ggtitle("Unemployment by Race: {frame_time}")

gender_anim <-demo_long %>%
  filter(demographic %in% gender) %>%
  ggplot(aes(x = Date, y = unemployment_rate, color = demographic)) +
  geom_point()+
  theme_minimal()+
  transition_time(Date)+
  shadow_mark()+# shadow mark allows the dots to stay on the plot in stead of disappering and looking stupid
  labs(y = "Unemploment Rate",x = "Date", color = "Gender")+
  ggtitle("Unemployment by Gender: {frame_time}")

education_anim <- demo_long %>%
  filter(demographic %in% education) %>%
  ggplot(aes(x = Date, y = unemployment_rate, color = demographic)) +
  geom_point()+
  theme_minimal()+
  transition_time(Date)+
  shadow_mark()+
  labs(y = "Unemploment Rate",x = "Date", color = "Education")+
  ggtitle("Unemployment by Education: {frame_time}")
  
animate(education_anim, fps = 12, duration = 20)

animate(race_anim, fps = 12, duration = 20)

my_theme <- theme_minimal()+
   theme(axis.title.x = element_text(face = "bold.italic", size = 12),  
    axis.title.y = element_text(face = "bold.italic", size = 12))



avg_rate <- demo_long %>% group_by(demographic) %>% reframe(Average = mean(unemployment_rate))
avg_rate %>% 
  ggplot(aes(x = demographic, y = Average))+
  geom_bar(stat = "identity",color = "blue",fill = "lightblue")+
  my_theme+
  theme(axis.text.x = element_text(angle = 90))
######## TRYING THE TIDY CENSUS
library(tidycensus)


acs_vars <- load_variables(2022, "acs1", cache = TRUE)
acs_vars %>% filter(grepl(".*unemployed.*",label, ignore.case = TRUE)) %>% print(n = Inf)


filter_vars <- acs_vars %>% 
  filter(grepl(".*Education.*",concept, ignore.case = TRUE) & grepl("United States", concept, ignore.case = TRUE))

edu_race <- read_csv("eduacation_by_race_census.csv")

rename_col <- function(data) {
  new_colnames <- as.character(data[1, ])
  colnames(data) <- new_colnames
  data <- data[-1, ]
  return(data)
}

edu_race <- rename_col(edu_race)

edu_race<- edu_race %>%
  mutate(across(-Education, as.numeric))
edu_race <-edu_race %>% drop_na()
#### SEEING THE GEOGRAPHIC EFFECT OF THESE HIDDEN VARIABLES
#unemployed less than highschool education
year = "2023"

less_hs_by_geo <- get_acs(
  geography = "county",
  state = "NY",
  variables = "B06009_002",
  year =  year ,
  survey = "acs1")
# init borroughs by their county name
borroughs <-c(
"Kings County",
"Bronx County",
"New York County",
"Queens County",
'Richmond County')

# number of less than highschool level education by geography
less_HS_GEO <-less_hs_by_geo %>%
  mutate(County= sub(", .*","",NAME)) %>% 
  select(-NAME) %>% 
  filter(grepl(paste(borroughs, collapse = "|"),County))
### get the shape file and join for plotting
library(sf)
borrough_shape <- read_sf("nyc_borough_boundaries/geo_export_1b7449ee-3559-48e8-b506-c4147ebf3d4d.shx") %>% arrange(boro_name)
less_HS_GEO <- less_HS_GEO %>% bind_cols(borrough_shape)

# get the total population so that we can do a proportion, and not have the population size effect our results
total_population <- get_acs(
  geography = "county",
  state = "NY",
  variables = "B01003_001",
  year = year,
  survey = "acs1")
# apply the pipeline to get our counties to the population data
less_HS_PROP <- total_population %>%
  mutate(County= sub(", .*","",NAME)) %>% 
  rename(total_pop = estimate) %>% 
  select(-NAME,-GEOID,-moe,-variable) %>% 
  inner_join(less_HS_GEO,by = "County") %>%
  mutate(prop = estimate/total_pop)
  


ggplot(less_HS_PROP, aes(geometry = geometry, fill = prop))+
  geom_sf()+
  geom_sf_text(aes(label = boro_name), size = 2, color = "black",fontface = "bold")+
  scale_fill_gradient(low = "white", high = "red") +
  labs(fill = "Proportion of Less then HS Education",title = "High School Degree Attainment by Borrough")+
  theme_void()+
  theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5,face = "bold.italic",size = 12))
  
