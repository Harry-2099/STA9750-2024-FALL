if(!require("tidyverse")) install.packages("tidyverse")

# Let's start with Fare Revenue
library(tidyverse)

  FARES <- readxl::read_xlsx("2022_fare_revenue.xlsx") |>
  select(-`State/Parent NTD ID`, 
         -`Reporter Type`,
         -`Reporting Module`,
         -`TOS`,
         -`Passenger Paid Fares`,
         -`Organization Paid Fares`) |>
  filter(`Expense Type` == "Funds Earned During Period") |>
  select(-`Expense Type`) |>
  group_by(`NTD ID`,       # Sum over different `TOS` for the same `Mode`
           `Agency Name`,  # These are direct operated and sub-contracted 
           `Mode`) |>      # of the same transit modality
  # Not a big effect in most munis (significant DO
  # tends to get rid of sub-contractors), but we'll sum
  # to unify different passenger experiences
  summarize(`Total Fares` = sum(`Total Fares`)) |>
  ungroup()

# Next, expenses
if(!file.exists("2022_expenses.csv")){
  # This should work _in theory_ but in practice it's still a bit finicky
  # If it doesn't work for you, download this file 'by hand' in your
  # browser and save it as "2022_expenses.csv" in your project
  # directory.
  download.file("https://data.transportation.gov/api/views/dkxx-zjd6/rows.csv?date=20231102&accessType=DOWNLOAD&bom=true&format=true", 
                destfile="2022_expenses.csv", 
                quiet=FALSE)
}
EXPENSES <- readr::read_csv("2022_expenses.csv") |>
  select(`NTD ID`, 
         `Agency`,
         `Total`, 
         `Mode`) |>
  mutate(`NTD ID` = as.integer(`NTD ID`)) |>
  rename(Expenses = Total) |>
  group_by(`NTD ID`, `Mode`) |>
  summarize(Expenses = sum(Expenses)) |>
  ungroup()

Financials <- inner_join(FARES, EXPENSES, join_by(`NTD ID`, `Mode`))

# Monthly Transit Numbers
library(tidyverse)
if(!file.exists("ridership.xlsx")){
  # This should work _in theory_ but in practice it's still a bit finicky
  # If it doesn't work for you, download this file 'by hand' in your
  # browser and save it as "ridership.xlsx" in your project
  # directory.
  download.file("https://www.transit.dot.gov/sites/fta.dot.gov/files/2024-09/July%202024%20Complete%20Monthly%20Ridership%20%28with%20adjustments%20and%20estimates%29_240903.xlsx", 
                destfile="ridership.xlsx", 
                quiet=FALSE,
                method= "curl")}

TRIPS <- readxl::read_xlsx("ridership.xlsx", sheet="UPT") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`), 
               names_to="month", 
               values_to="UPT") |>
  drop_na() |>
  mutate(month=my(month)) # Parse _m_onth _y_ear date specs
MILES <- readxl::read_xlsx("ridership.xlsx", sheet="VRM") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`), 
               names_to="month", 
               values_to="VRM") |>
  drop_na() |>
  group_by(`NTD ID`, `Agency`, `UZA Name`, 
           `Mode`, `3 Mode`, month) |>
  summarize(VRM = sum(VRM)) |>
  ungroup() |>
  mutate(month=my(month)) # Parse _m_onth _y_ear date specs

#### Task One ####

  USAGE <- inner_join(TRIPS, MILES) |>
    mutate(`NTD ID` = as.integer(`NTD ID`))
    sample_n(USAGE, 1000) |> 
    rename("metro_area" = "UZA Name") %>% #renaming UZA
    mutate(month=as.character(month))
  
  USAGE <- USAGE %>% # running again because view doesn't change the name
    rename("metro_area" = "UZA Name")
  
  Use_table <- DT::datatable(USAGE)

#### Task 2 Distinct Modes ####
  distinct_modes<- USAGE %>% distinct(Mode)

  USAGE <- USAGE |>
     mutate(Mode=case_when(
          Mode == "HR" ~ "Heavy Rail", 
          Mode == "DR"~"Demand Response",
          Mode == "FB"~"Ferryboat",
          Mode == "MB"~"Bus",
          Mode == "SR"~"Streetcar Rail",
          Mode == "TB"~"Trolleybus",
          Mode == "VP"~"Vanpool",
          Mode == "CB"~"Commuter Bus",
          Mode == "RB"~"Bus Rapid Transit",
          Mode == "LR"~"Light Rail",
          Mode == "YR"~"Hybrid Rail",
          Mode == "MG"~"Monorail Automated Guideway",
          Mode == "CR"~"Commuter Rail",
          Mode == "AR"~"Alaska Railroad",
          Mode == "TR"~"Aerial Tramway",
          Mode == "IP"~"Inclined Plane",
          Mode == "PB"~"Publico",
          Mode == "CC"~"Cable Car",
          TRUE~"Unknown"))

  Use_table <- DT::datatable(USAGE)
  Use_table
#### TASK 3 #### 
#What transit agency had the most total VRM in our data set?

  Max_VRM <- USAGE %>% 
    select(VRM,Agency) %>%
    arrange(desc(VRM)) %>% 
    head(1)
    print(Max_VRM)

# What transit mode had the most total VRM in our data set?

  vrm_by_mode <- USAGE %>% 
    select(VRM,Mode) %>%
    group_by(Mode) %>% 
    summarise(Total = sum(VRM)) %>% 
    arrange(desc(Total))
    print(vrm_by_mode) # The answer is BUS

#How many trips were taken on the NYC Subway (Heavy Rail) in May 2024?

typeof(USAGE$month)#checking the type of column month is

library(lubridate)
USAGE$month <- ymd(USAGE$month)

#creating new cols with all the years/months, this come in handy for further analysis later
  USAGE <- USAGE %>% 
    rename("trips"=UPT) %>% #renaming the UPT(Unlinked passenger trips)- to trips
    rename("date"=month) %>%  
    mutate("year"= year(date)) %>%
    mutate('month'= month(date))
  
  nyc_trips <- USAGE %>% 
    filter(Agency == "MTA New York City Transit" & Mode =="Heavy Rail")%>% 
    filter(year==2024 & month == 5) %>% 
    summarise(sum(trips))
    print(nyc_trips) #180 mil 

#How much did NYC subway ridership fall between April 2019 and April 2020?

  nyc_monthly <- USAGE %>%
    filter(Agency == 'MTA New York City Transit' & Mode == 'Heavy Rail') %>% 
    group_by(year, month) %>%                       
    mutate(total_vrm = sum(VRM)) %>% 
    mutate(total_trips = sum(trips)) %>%
    select(year,month,total_trips,total_vrm)
  
  vrm_2019 <- nyc_monthly %>%
    filter(year == 2019 & month == 4) %>% 
    pull(total_vrm)
  
  vrm_2020 <- nyc_monthly %>%
    filter(year == 2020 & month == 4) %>% 
    pull(total_vrm)
  
  vrm_2019-vrm_2020 # 12million difference in Vehicle Revenue Miles
  
  trips_2019 <- nyc_monthly %>%
    filter(year == 2019 & month == 4) %>% 
    pull(total_trips)
  
  trips_2020 <- nyc_monthly %>%
    filter(year == 2020 & month == 4) %>% 
    pull(total_trips)
  
  trips_2019-trips_2020 #211 million decrease in trips

#### TASK 4: Find three more interesting transit facts in this data other than those above ####
  #1) What month had the highest VRM in 2023
     USAGE %>% 
     filter(year == 2023) %>% 
     select(year,month,VRM) %>% 
     group_by(month) %>% 
     summarise(total_vrm = sum(VRM)) %>% 
     arrange(desc(total_vrm))

   #2) What year had the most trips?
     yearly_trips <- USAGE %>% 
     group_by(year) %>% 
     mutate(total_trips = sum(trips)) %>%
     mutate(avg_trip = mean(trips)) %>% 
     select(year,total_trips,avg_trip) %>% 
     arrange(desc(total_trips)) %>% 
     distinct(year,total_trips,avg_trip)
     #plotting
     ggplot(yearly_trips,aes(x= year,y=avg_trip))+
       geom_line(color = 'blue')+
       labs(title = "Avg Trips Per Year")+
       theme_minimal()
       
   #  and then a plummet in 2020, with 2024 rebounding in avg trips
   
   #3) What area has the highest/lowest trips, perhaps excluding the NYC area
      trips_by_area <- USAGE %>% 
        select(year,metro_area,trips) %>% 
        group_by(metro_area) %>% 
        summarise(avg_trips = mean(trips)) %>% 
        arrange(desc(avg_trips)) %>% 
        head(10)
        # excluding the NYC area, Chicago was the highest followed by Las Vegas area, the lowest was Cheyenne,WY
        ggplot(trips_by_area,aes(x = metro_area,y = avg_trips))+
          geom_bar(stat = 'identity',aes(fill = metro_area),color = "red",show.legend = FALSE)+
          theme_minimal()+
          theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust = 1))+
          labs(title = "Average Yearly Trips by City",y= "Unlinked Passenger Trips", x= "Metro Area")
          

#### Task 5) Table Summarization ####
      
      USAGE_2022_ANNUAL <- USAGE %>% 
        filter(year == 2022) %>% 
        select('NTD ID',Mode,Agency,metro_area,trips,VRM) %>% 
        group_by(`NTD ID`,Mode) %>% 
        summarise(annual_trips = sum(trips),annual_vrm = sum(VRM)) %>% 
        ungroup()
        
      Financials <- Financials |>
     mutate(Mode=case_when(
          Mode == "HR" ~ "Heavy Rail", 
          Mode == "DR"~"Demand Response",
          Mode == "FB"~"Ferryboat",
          Mode == "MB"~"Bus",
          Mode == "SR"~"Streetcar Rail",
          Mode == "TB"~"Trolleybus",
          Mode == "VP"~"Vanpool",
          Mode == "CB"~"Commuter Bus",
          Mode == "RB"~"Bus Rapid Transit",
          Mode == "LR"~"Light Rail",
          Mode == "YR"~"Hybrid Rail",
          Mode == "MG"~"Monorail Automated Guideway",
          Mode == "CR"~"Commuter Rail",
          Mode == "AR"~"Alaska Railroad",
          Mode == "TR"~"Aerial Tramway",
          Mode == "IP"~"Inclined Plane",
          Mode == "PB"~"Publico",
          Mode == "CC"~"Cable Car",
          TRUE~"Unknown"))
      
      USAGE_AND_FINANCIALS <- left_join(USAGE_2022_ANNUAL, 
           Financials, 
           join_by(`NTD ID`, Mode)) |>
           drop_na()
#### Task 6: Farebox Recovery Among Major Systems ####
      # Which transit system (agency and mode) had the most UPT in 2022?
      USAGE_AND_FINANCIALS %>% 
        group_by(`Agency Name`,`Mode`) %>% 
        summarise(max_trips = max(annual_trips)) %>% 
        arrange(desc(max_trips)) %>% 
        head(1)
      #Which transit system (agency and mode) had the highest fare-box recovery,-Ratio of Total Fares to Expenses?
      USAGE_AND_FINANCIALS<-USAGE_AND_FINANCIALS %>% 
         group_by(`Agency Name`,`Mode`) %>% 
         mutate(fare_box_recovery = `Total Fares`/Expenses) %>% 
         arrange(desc(fare_box_recovery))
         print(USAGE_AND_FINANCIALS) #Vanpool kentucky
      #Which transit system (agency and mode) has the lowest expenses per UPT?
       USAGE_AND_FINANCIALS %>% 
           group_by(`Agency Name`,`Mode`) %>% 
           mutate(expenses_per_trip = Expenses/annual_trips) %>% 
           select(`Agency Name`,Mode,expenses_per_trip) %>% 
           arrange(expenses_per_trip)
      #Which transit system (agency and mode) has the highest total fares per UPT?
         library(stringr)
       fare_per_trip <- USAGE_AND_FINANCIALS %>% 
           group_by(`Agency Name`,`Mode`) %>% 
           mutate(fares_per_trip = `Total Fares`/annual_trips) %>% 
           mutate(Agent_Mode = str_c(`Agency Name`,Mode, sep = ", ")) %>% 
           arrange(desc(fares_per_trip)) %>% 
           select(`Agency Name`,fares_per_trip,Mode,Agent_Mode) %>% 
           head(10)
        #Which transit system (agency and mode) has the lowest expenses per VRM?
         USAGE_AND_FINANCIALS %>% 
           mutate(expenses_per_vrm = Expenses/annual_vrm) %>% 
           arrange(expenses_per_vrm) %>% 
           select(`Agency Name`,Mode,expenses_per_vrm) %>% 
           head(5)
         #Which transit system (agency and mode) has the highest total fares per VRM?
      
         fares_vrm<-USAGE_AND_FINANCIALS %>% 
           mutate(fares_per_vrm = `Total Fares`/annual_vrm) %>%
           mutate(Agent_Mode = str_c(`Agency Name`,Mode, sep = ", ")) %>% 
           arrange(desc(fares_per_vrm)) %>% 
           select(Agent_Mode,fares_per_vrm) %>% 
           head(10)
#### Visualizing some of the above ####
         USAGE_AND_FINANCIALS <- USAGE_AND_FINANCIALS %>% 
           group_by(`Agency Name`,Mode) %>% 
           mutate(Agent_Mode = str_c(`Agency Name`,Mode, sep = ", ")) %>% 
           ungroup()
      ggplot(fare_per_trip,aes(x = Agent_Mode,y = fares_per_trip))+
           geom_bar(stat = "identity",aes(fill= Agent_Mode),color= 'purple',show.legend = FALSE)+
           theme_minimal()+
           theme(axis.text.x = element_text(angle = 45,vjust =1,hjust = 1))+
           labs(title = "Fairs Per UPT(Unlinked Passenger Trips)", x = "Agency & Mode",y = "Fares Per Passenger Trips")
         
       ggplot(fares_vrm,aes(x = Agent_Mode,y = fares_per_vrm))+
           geom_bar(stat = "identity",aes(fill= Agent_Mode),color= 'lightblue',show.legend = FALSE)+
           theme_bw()+
           theme(axis.text.x = element_text(angle = 45,vjust =1,hjust = 1))+
           labs(title = "Fairs Per VRM(Vehicle Revenue Miles)", x = "Agency & Mode",y = "Fares Per VRM")
             