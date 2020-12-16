# Un-comment and install packages if not already installed
#install.packages("RCurl")
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("gsheet")
#install.packages("raster")
#install.packages("dplyr")
#install.packages("spData")
#install.packages("ggplot2")
#install.packages("stats")
#install.packages("Hmisc")

# Open needed libraries
library(RCurl)
library(tidyverse)
library(lubridate)
library(gsheet)
library(raster)
library(dplyr)
library(spData)
library(ggplot2)
library(stats)
library(Hmisc)

##########################################################################
# Download dataframes

#Source: fatalencounters.org, D. Brian Burghart
df_fatalities_2000to2019 <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE/edit#gid=0")

#Source: United States Census Bureau
df_population_2010to2019 <- read.csv(text = getURL("https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/asrh/sc-est2019-alldata6.csv"))
df_population_2000to2010 <- read.csv(text = getURL("https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/co-est00int-sexracehisp.csv"))

##########################################################################
# Data Manipulation - Fatality Data Frame
df_fatalities_2000to2019_edited <- df_fatalities_2000to2019 %>%
  rename("Date" = "Date of injury resulting in death (month/day/year)") %>%
  mutate(Date = mdy(Date), Year = year(Date)) %>%
  filter(Year < 2020) %>%
  mutate(Race = dplyr::recode(Race,		
                              "European-American/White" = "White",		
                              "Hispanic/Latino" = "Hispanic",		
                              "African-American/Black" = "African-American",		
                              "Native American/Alaskan" = "Native American",		
                              "Asian/Pacific Islander" = "Asian")) %>%
  mutate(State = dplyr::recode(State,
                               "AL" = "Alabama",
                               "AK" = "Alaska",
                               "AZ" = "Arizona",
                               "AR" = "Arkansas",
                               "CA" = "California",
                               "CO" = "Colorado",
                               "CT" = "Connecticut",
                               "DE" = "Delaware",
                               "DC" = "District of Columbia",
                               "FL" = "Florida",
                               "GA" = "Georgia",
                               "HI" = "Hawaii",
                               "ID" = "Idaho",
                               "IL" = "Illinois",
                               "IN" = "Indiana",
                               "IA" = "Iowa",
                               "KS" = "Kansas",
                               "KY" = "Kentucky",
                               "LA" = "Louisiana",
                               "ME" = "Maine",
                               "MD" = "Maryland",
                               "MA" = "Massachusetts",
                               "MI" = "Michigan",
                               "MN" = "Minnesota",
                               "MS" = "Mississippi",
                               "MO" = "Missouri",
                               "MT" = "Montana",
                               "NE" = "Nebraska",
                               "NV" = "Nevada",
                               "NH" = "New Hampshire",
                               "NJ" = "New Jersey",
                               "NM" = "New Mexico",
                               "NY" = "New York",
                               "NC" = "North Carolina",
                               "ND" = "North Dakota",
                               "OH" = "Ohio",
                               "OK" = "Oklahoma",
                               "OR" = "Oregon",
                               "PA" = "Pennsylvania",
                               "RI" = "Rhode Island",
                               "SC" = "South Carolina",
                               "SD" = "South Dakota",
                               "TN" = "Tennessee",
                               "TX" = "Texas",
                               "UT" = "Utah",
                               "VT" = "Vermont",
                               "VA" = "Virginia",
                               "WA" = "Washington",
                               "WV" = "West Virginia",
                               "WI" = "Wisconsin",
                               "WY" = "Wyoming"))

# Population
df_population_2000to2010_edited <- df_population_2000to2010 %>%
  filter(SEX == 0, ORIGIN == 0) %>%
  group_by(STATE, RACE) %>% 
  rename("State" = "STATE", "Race" = "RACE") %>%
  dplyr::summarize(pop2000 = sum(POPESTIMATE2000), 
                   pop2001=sum(POPESTIMATE2001),
                   pop2002=sum(POPESTIMATE2002),
                   pop2003=sum(POPESTIMATE2003),
                   pop2004=sum(POPESTIMATE2004),
                   pop2005=sum(POPESTIMATE2005),
                   pop2006=sum(POPESTIMATE2006),
                   pop2007=sum(POPESTIMATE2007),
                   pop2008=sum(POPESTIMATE2008),
                   pop2009=sum(POPESTIMATE2009))

df_population_2010to2019_edited <- df_population_2010to2019 %>%
  filter(SEX == 0, ORIGIN == 0) %>%
  group_by(STATE, RACE) %>%
  rename("State" = "STATE", "Race" = "RACE") %>%
  dplyr::summarize(pop2010 = sum(CENSUS2010POP),
                   pop2011=sum(POPESTIMATE2011),
                   pop2012=sum(POPESTIMATE2012),
                   pop2013=sum(POPESTIMATE2013),
                   pop2014=sum(POPESTIMATE2014),
                   pop2015=sum(POPESTIMATE2015),
                   pop2016=sum(POPESTIMATE2016),
                   pop2017=sum(POPESTIMATE2017),
                   pop2018=sum(POPESTIMATE2018),
                   pop2019=sum(POPESTIMATE2019))

#Merge to one population date frame
df_population_2000to2019 <- merge(df_population_2000to2010_edited, df_population_2010to2019_edited) %>%
  mutate(Race = dplyr::recode(Race,		
                              "1" = "White",
                              "2" = "African-American",
                              "3" = "Native American",
                              "4" = "Asian",
                              "5" = "Pacific Islander",
                              "6" = "Two or more races")) %>%
  mutate(State = dplyr::recode(State,
                               "1" = "Alabama",
                               "2" = "Alaska",
                               "4" = "Arizona",
                               "5" = "Arkansas",
                               "6" = "California",
                               "8" = "Colorado",
                               "9" = "Connecticut",
                               "10" = "Delaware",
                               "11" = "District of Columbia",
                               "12" = "Florida",
                               "13" = "Georgia",
                               "15" = "Hawaii",
                               "16" = "Idaho",
                               "17" = "Illinois",
                               "18" = "Indiana",
                               "19" = "Iowa",
                               "20" = "Kansas",
                               "21" = "Kentucky",
                               "22" = "Louisiana",
                               "23" = "Maine",
                               "24" = "Maryland",
                               "25" = "Massachusetts",
                               "26" = "Michigan",
                               "27" = "Minnesota",
                               "28" = "Mississippi",
                               "29" = "Missouri",
                               "30" = "Montana",
                               "31" = "Nebraska",
                               "32" = "Nevada",
                               "33" = "New Hampshire",
                               "34" = "New Jersey",
                               "35" = "New Mexico",
                               "36" = "New York",
                               "37" = "North Carolina",
                               "38" = "North Dakota",
                               "39" = "Ohio",
                               "40" = "Oklahoma",
                               "41" = "Oregon",
                               "42" = "Pennsylvania",
                               "44" = "Rhode Island",
                               "45" = "South Carolina",
                               "46" = "South Dakota",
                               "47" = "Tennessee",
                               "48" = "Texas",
                               "49" = "Utah",
                               "50" = "Vermont",
                               "51" = "Virginia",
                               "53" = "Washington",
                               "54" = "West Virginia",
                               "55" = "Wisconsin",
                               "56" = "Wyoming"))

####################################################################################
# Create a data frame for each year with the proportion of number of fatalities 
# over the number of people of that race per year
races <- list("White", "African-American", "Asian", "Native American")
df_propfatbystate <- data_frame()

for(yr in 2000:2019){
  for(race in races){
    df <- make_dataframe_by_year_race_state(yr, race)
    
    df_propfatbystate <- rbind(df_propfatbystate, df)
  }
}
# Function to create a data frame with the proportion of fatalities grouped by year, race, and state
make_dataframe_by_year_race_state <- function(){
  races <- list("White", "African-American", "Asian", "Native American")
  new_df <- data_frame()
  
  for(year in 2000:2019){
    popyear <- paste("pop", year, sep = "")
    
    for(race in races){
      pop_df <- df_population_2000to2019 %>%
        filter(Race == race) %>%
        mutate(Year = year) %>%
        dplyr::select(State, Race, popyear, Year) %>%
        rename("Population" = popyear)
  
  
    fat_df <- df_fatalities_2000to2019_edited %>%
      group_by(State, Year, Race) %>%
      dplyr::summarize(Fatalities = n()) %>%
      filter(Year == year, Race == race)
    
  # Number of fatalities per 1,000,000 people
  # Formula: (# fatalities by race/Population by race) * 1000000
  tmp <- merge(pop_df, fat_df) %>%
    mutate(Proportion = (Fatalities/Population)*1000000)
  
  new_df <- rbind(new_df, tmp)
    }
  }
  return(new_df)
}
test <- make_dataframe_by_year_race_state()
# Function to create a data frame with the proportion of fatalities grouped by year and state
make_dataframe_by_year_state <- function(){
  pop_df <- data_frame()
  
  for(year in 2000:2019){
    popyear <- paste("pop", year, sep = "")
    
    tmp <- df_population_2000to2019 %>%
      rename("Pop" = popyear) %>%
      group_by(State) %>%
      mutate(Year = year, Population = sum(Pop)) %>%
      dplyr::select(State, Population, Year) %>%
      unique()
    
    pop_df <- rbind(pop_df, tmp)
  }
  
  fat_df <- df_fatalities_2000to2019_edited %>%
    group_by(State, Year) %>%
    dplyr::summarize(Fatalities = n()) %>%
    dplyr::select(State, Year, Fatalities) %>%
    unique()
  
  new_df <- merge(pop_df, fat_df) %>%
    mutate(Proportion = (Fatalities/Population)*1000000)
  
  return(new_df)
  
}

# Function to create a data frame with the proportion of total fatalities to the total population for each year
make_dataframe_by_year <- function(){
  pop_df <- data_frame()
  
  for(year in 2000:2019){
    popyear <- paste("pop", year, sep = "")
    
    tmp <- df_population_2000to2019 %>%
      dplyr::select(Race, popyear) %>%
      rename("Pop" = popyear) %>%
      mutate(Year = year, Population = sum(Pop)) %>%
      dplyr::select(Year, Population) %>%
      unique()
    
    pop_df <- rbind(pop_df, tmp)
  }
  
  fat_df <- df_fatalities_2000to2019_edited %>%
    group_by(Year) %>%
    dplyr::summarize(Fatalities = n())
  
  new_df <- merge(pop_df, fat_df) %>%
    mutate(Proportion = (Fatalities/Population)*1000000)
  
  return(new_df)
}

# Function to create a data frame for each race and year
make_dataframe_by_race_year <- function(){
  races <- list("White", "African-American", "Asian", "Native American")
  new_df <- data_frame()
  
  for(year in 2000:2019){
    popyear <- paste("pop", year, sep = "")
    
    for(race in races){
      tmp_pop <- df_population_2000to2019 %>%
        dplyr::select(Race, popyear) %>%
        filter(Race == race) %>%
        rename("Pop" = popyear) %>%
        mutate(Year = year, Population = sum(Pop)) %>%
        dplyr::select(Race, Year, Population) %>%
        unique()
      
      tmp_fat <- df_fatalities_2000to2019_edited %>%
        group_by(Year, Race) %>%
        filter(Race == race, Year == year) %>%
        mutate(Fatalities = n()) %>%
        dplyr::select(Race, Year, Fatalities) %>%
        unique()
      
      # Number of fatalities per 1,000,000 people
      # Formula: (# fatalities by race/Population by race) * 1000000
      tmp <- merge(tmp_pop, tmp_fat) %>%
        mutate(Proportion = (Fatalities/Population)*1000000)
      
      new_df <- rbind(new_df, tmp)
    }
  }
  
  return(new_df)
} 

# Function to create a data frame with % fatalities and % population of races 
# in a given year 
make_dataframe_percent_fat_pop <- function(year){
  popyear <- paste("pop", year, sep = "")
  
  pop_df <- df_population_2000to2019 %>%
    rename("Pop" = popyear) %>%
    mutate(TotalPop = sum(Pop)) %>%
    group_by(Race) %>%
    mutate(TotalRace = sum(Pop)) %>%
    mutate("% Population" = TotalRace/TotalPop) %>%
    dplyr::select(Race, "% Population") %>%
    unique()
  
  fat_df <- df_fatalities_2000to2019_edited %>%
    filter(Year == year) %>%
    mutate(TotalFat = sum(Year == year)) %>%
    group_by(Race) %>%
    mutate(TotalRace = n()) %>%
    mutate("% Fatalities" = TotalRace/TotalFat) %>%
    dplyr::select(Race, "% Fatalities") %>%
    unique()
  
  new_df <- merge(fat_df, pop_df) %>%
    gather(Variable, Percent, -Race)
  
  return(new_df)
}

######################################################################
# Create a data frame with the proportion of total fatalities by population each year
df_totalpropfat <- make_dataframe_by_year()

######################################################################
# Create a dataframe for each year with the proportion of number of fatalities
# over the number of people of that race per year (total of all the states)

df_totalpropfatbyraceyear <- make_dataframe_by_race_year()
######################################################################
# Create a dataframe for each year with the proportion of number of fatalities 
# over the number of people of that race per year
races <- list("White", "African-American", "Asian", "Native American")
df_propfatbystate <- data_frame()

for(yr in 2000:2019){
  for(race in races){
    df <- make_dataframe_by_year_race_state(yr, race)
    
    df_propfatbystate <- rbind(df_propfatbystate, df)
  }
}

######################################################################
# Create a data frame with the proportion of fatalities grouped by year and state
df_propbyyearstate <- make_dataframe_by_year_state()

######################################################################
# Create a dataframe with the % fatalities and % of population
# of each race in 2019

df_percent_fatpop <- make_dataframe_percent_fat_pop(2019)

######################################################################
#GRAPHS - United States

# Number of fatalities per million people over time - line chart
plot_proportion_fatalities <- df_totalpropfat %>%
  ggplot(aes(x=Year, y=Proportion)) +
  geom_line() +
  theme_minimal() +
  geom_vline(xintercept = c(2006, 2011, 2014, 2015, 2016), linetype = "dashed") +
  labs(title="Police-Related Fatalites per Million People", 
       subtitle="United States, 2000-2019",
       caption = "Figure #. This graphs shows the number of police-related fatalities per million people each year in the
       United States from 2000-2019. The vertical dashed lines represent the years where there were 
       major protests against police brutality in the United States.",
       y = "Number of Fatalities") +
  theme(plot.caption = element_text(hjust = 0))

# Total deaths over time by race - Line chart
plot_total_fatalities_race_line <- df_fatalities_2000to2019_edited %>%
  group_by(Race, Year) %>%
  count(Race) %>%
  ungroup() %>%
  ggplot(aes(x=Year, y=n, group=Race)) +
  geom_line(aes(color=Race)) +
  theme_minimal() +
  geom_vline(xintercept = c(2006, 2011, 2014, 2015, 2016), linetype = "dashed") +
  scale_color_manual(values = c("black", "dodgerblue", "darkorange", "darkviolet", "brown", "darkgreen", "tan"), name = "") +
  labs(title="Police-Related Fatalites by Race", 
       subtitle="United States, 2000-2019",
       caption = "Figure #. This graph shows the number of police-related fatalities by race each year in the 
       United States from 2000-2019. The vertical dashed lines represent the years where there 
       were major protests against police brutality in the United States.",
       y = "Number of Fatalities") +
  theme(plot.caption = element_text(hjust = 0))

# Total deaths over time - line chart
plot_total_fatalites <- df_fatalities_2000to2019_edited %>%
  group_by(Year) %>%
  count() %>%
  ggplot(aes(x=Year, y=n)) +
  geom_line() +
  theme_minimal() +
  geom_vline(xintercept = c(2006, 2011, 2014, 2015, 2016), linetype = "dashed") +
  labs(title="Total Police-Related Fatalities over Time",
       subtitle="United States, 2000-2019",
       y = "Number of Fatalities")

# Growth rate of total fatalities - line chart
plot_growthrate <- df_fatalities_2000to2019_edited %>%
  group_by(Year) %>%
  count() %>%
  mutate(NumFatalities = n) %>%
  ungroup() %>%
  mutate(Previous_Year = lag(NumFatalities, 1), 
         Change = NumFatalities - Previous_Year, 
         Growth_Rate= (Change/Previous_Year)*100) %>%
  ggplot(aes(x = Year, y = Growth_Rate)) +
  geom_line() +
  theme_minimal() +
  geom_vline(xintercept = c(2006, 2011, 2014, 2015, 2016), linetype = "dashed") +
  labs(title="Growth Rate of Police-Related Fatalities per Year",
       subtitle = "United States, 2000-2019",
       caption = "Figure #. This graph shows the growth rate of police-related fatalities per million people compared to the previous year 
       in the United States from 2000-2019. The vertical dashed lines represent the years where there were major 
       protests against police brutality in the United States.",
       y="Growth Rate since Previous Year") +
  theme(plot.caption = element_text(hjust = 0))


# Proportion of fatalities by race - line chart
plot_proportion_fatalities_race <- df_totalpropfatbyraceyear %>%
  ggplot(aes(x=Year, y =Proportion, group=Race)) +
  geom_line(aes(color=Race)) +
  theme_minimal() +
  geom_vline(xintercept = c(2006, 2011, 2014, 2015, 2016), linetype = "dashed") +
  scale_color_manual(values = c("black", "dodgerblue", "brown", "tan"), name = "") +
  labs(title="Number of Police-Related Fatalities per Million People by Race",
       subtitle="United States, 2000-2019",
       caption = " Figure #. This graph shows the number of police-related fatalities per million people of each race
       per year in the United States from 2000-2019. The proportion was found by taking the number
       of fatalities of a race over the population of that race. The vertical dashed lines represent 
       the years where there were major protests against police brutality in the United States.",
       y = "Number of Fatalites per Million People") +
  theme(plot.caption = element_text(hjust = 0))

# % population vs % fatalities of races in 2019
plot_percentpopfat <- df_percent_fatpop %>%
  ggplot(aes(x = Race, y = Percent*100, fill = Variable)) +
  geom_col(position = position_dodge()) +
  theme_minimal() +
  labs(title="Percent Population vs. Percent Police-Related Fatalities by Race",
       subtitle = "United States, 2019",
       caption = "Figure #. This chart shows the percentage of police-related fatalities and the 
       percentage of the population that each race occupies for the year 2019.",
       y = "Percent") +
  theme(plot.caption = element_text(hjust = 0))

#######################################################################
# STATE-SPECIFIC PLOT FUNCTIONS

# Function to create a plot for the total fatalities for a state
createplot_totalfat_state <- function(state, year_vec, fig_num){
  return(df_fatalities_2000to2019_edited %>%
           filter(State == state) %>%
           group_by(Year) %>%
           count() %>%
           ggplot(aes(x=Year, y=n)) +
           geom_line() +
           theme_minimal() +
           geom_vline(xintercept = year_vec, linetype = "dashed") +
           labs(title="Total Police-Related Fatalities over Time",
                subtitle= paste0(state, ", ", "2000-2019"),
                caption = paste0("Figure ", as.character(fig_num), ". This plot shows the total number of police-related fatalities per year from 2000-2019
                in the state of ", state, ". The vertical dashed lines represent the years where there 
                were major protests against police brutality in the state of ", state, "."),
                y = "Number of Fatalities") +
    theme(plot.caption = element_text(hjust = 0)))
}

# Function to create a plot for the growth rate of total fatalities for a state
createplot_growthrate_state <- function(state, year_vec, fig_num){
  return(df_propbyyearstate %>%
           filter(State == state) %>%
           mutate(Previous_Year = lag(Proportion, 1), 
                  Change = Proportion - Previous_Year, 
                  Growth_Rate= (Change/Previous_Year)*100) %>%
    ggplot(aes(x = Year, y = Growth_Rate)) +
    geom_line() +
    theme_minimal() +
    geom_vline(xintercept = year_vec, linetype = "dashed") +
    labs(title="Growth Rate of Police Fatalities per Year",
        subtitle = paste0(state, ", ", "2000-2019"),
        caption = paste0("Figure ", as.character(fig_num), ". This graph shows the growth rate of police-related fatalities compared to the previous year in the
        state of ", state, " from 2000-2019. The vertical dashed lines represent the years where there 
        were major protests against police brutality in the state of ", state, "."),
        y="Percent Growth since Previous Year") +
    theme(plot.caption = element_text(hjust = 0)))
}

# Function to create a plot for the number of fatalities per million people in a specific state
createplot_propfat_state <- function(state, year_vec, fig_num){
  return(df_propbyyearstate %>%
    filter(State == state) %>%
    ggplot(aes(x=Year, y=Proportion)) +
    geom_line() +
    theme_minimal() +
    geom_vline(xintercept = year_vec, linetype = "dashed") +
    labs(title="Police-Related Fatalites per Million People", 
         subtitle=paste0(state, ", ", "2000-2019"),
         caption = paste0("Figure ", as.character(fig_num), ". This graphs shows the number of police-related fatalities per million people each year in the
                state of ", state, " from 2000-2019. The vertical dashed lines represent the years where there 
                were major protests against police brutality in the state of ", state, "."),
         y = "Number of Fatalities") +
    theme(plot.caption = element_text(hjust = 0)))
}

# Function to create a plot for the proportion of fatalities by race in a specific state
createplot_raceproportion_state <- function(state, year_vec, fig_num){
  return(df_propfatbystate %>%
    filter(State == state) %>%
    filter(Race == "African-American" | Race == "White") %>%
    ggplot(aes(x=Year, y =Proportion, group=Race)) +
    geom_line(aes(color=Race)) +
    theme_minimal() +
    geom_vline(xintercept = year_vec, linetype = "dashed") +
    scale_color_manual(values = c("black","tan"), name = "") +
    labs(title="Number of Police-Related Fatalities per Million People by Race",
        subtitle=paste0(state, ", ", "2000-2019"),
        caption = paste0("Figure ", as.character(fig_num), ". This graph shows the number of police-related fatalities per million people of each race
        per year in the state of ", state, " from 2000-2019. The proportion was found by taking the number
        of fatalities of a race over the population of that race.The vertical dashed lines represent the 
        years where there were major protests against police brutality in the state of ", state, "."),
        y = "Number of Fatalites per Million People") +
    theme(plot.caption = element_text(hjust = 0)))
}

# Function to create a plot for the total fatalities by race in a specific state
createplot_fatbyrace_state <- function(state, year_vec, fig_num){
  return(df_fatalities_2000to2019_edited %>%
    filter(State == state) %>%
    filter(Race == "African-American" | Race == "Hispanic" | Race == "Race unspecified" | Race == "White") %>%
    group_by(Race, Year) %>%
    count(Race) %>%
    ungroup() %>%
    ggplot(aes(x=Year, y=n, group=Race)) +
    geom_line(aes(color=Race)) +
    theme_minimal() +
    geom_vline(xintercept = year_vec, linetype = "dashed") +
    scale_color_manual(values = c("black", "darkorange", "darkgreen", "tan"), name = "") +
    labs(title="Police-Related Fatalites by Race", 
        subtitle=paste0(state, ", ", "2000-2019"),
        caption = paste0("Figure ", as.character(fig_num), ". This graph shows the number of police-related fatalities by race each year in the 
        state of ", state, " from 2000-2019. The vertical dashed lines represent the 
        years where there were major protests against police brutality in the state of ", state, "."),
        y = "Number of Fatalities") +
    theme(plot.caption = element_text(hjust = 0)))
}

#######################################################################
# GRAPHS FOR PROTESTS 

########### NEW YORK - 2006, 2014

state <- "New York"
protest_years <- c(2006, 2014)

# Total fatalities - New York
plot_totalfat_newyork <- createplot_totalfat_state(state, protest_years, 1)

# Growth rate of total fatalities - New York
plot_growthrate_newyork <- createplot_growthrate_state(state, protest_years, 2)

# Number of fatalities per million people - New York
plot_fatpermillion_newyork <- createplot_propfat_state(state, protest_years, 3)

# Total fatalities by race - New York
plot_racefat_newyork <- createplot_fatbyrace_state(state, protest_years, 4)

# Proportion of fatalities by race - New York
plot_prop_newyork <- createplot_raceproportion_state(state, protest_years, 5)

########### MISSOURI - 2011, 2014

state <- "Missouri"
protest_years <- c(2011, 2014)

# Total fatalities - Missouri
plot_totalfat_missouri <- createplot_totalfat_state(state, protest_years, 6)

# Growth rate of total fatalities - Missouri
plot_growthrate_missouri <- createplot_growthrate_state(state, protest_years, 7)

# Number of fatalities per million people - Missouri
plot_fatpermillion_missouri <- createplot_propfat_state(state, protest_years, 8)

# Total fatalities by race - Missouri
plot_racefat_missouri <- createplot_fatbyrace_state(state, protest_years, 9)

# Proportion of fatalities by race - Missouri
plot_prop_missouri <- createplot_raceproportion_state(state, protest_years, 10)

########### MARYLAND - 2015

state <- "Maryland"
protest_years <- c(2015)

# Total fatalities - Maryland
plot_totalfat_maryland <- createplot_totalfat_state(state, protest_years, 11)

# Growth rate of total fatalities - Maryland
plot_growthrate_maryland <- createplot_growthrate_state(state, protest_years, 12)

# Number of fatalities per million people - Maryland
plot_fatpermillion_maryland <- createplot_propfat_state(state, protest_years, 13)

# Total fatalities by race - Maryland
plot_racefat_maryland <- createplot_fatbyrace_state(state, protest_years, 14)

# Proportion of fatalities by race - Maryland
plot_prop_maryland <- createplot_raceproportion_state(state, protest_years, 15)

########### LOUISIANA - 2016

state <- "Louisiana"
protest_years <- c(2016)

# Total fatalities - Louisiana
plot_totalfat_louisiana <- createplot_totalfat_state(state, protest_years, 16)

# Growth rate of total fatalities - Louisiana
plot_growthrate_louisiana <- createplot_growthrate_state(state, protest_years, 17)

# Number of fatalities per million people - Louisiana
plot_fatpermillion_louisiana <- createplot_propfat_state(state, protest_years, 18)

# Total fatalities by race - Louisiana
plot_racefat_louisiana <- createplot_fatbyrace_state(state, protest_years, 19)

# Proportion of fatalities by race - Louisiana
plot_prop_louisiana <- createplot_raceproportion_state(state, protest_years, 20)

########### MINNESOTA - 2016

state <- "Minnesota"
protest_years <- c(2016)

# Total fatalities - Minnesota
plot_totalfat_minnesota <- createplot_totalfat_state(state, protest_years, 21)

# Growth rate of total fatalities - Minnesota
plot_growthrate_minnesota <- createplot_growthrate_state(state, protest_years, 22)

# Number of fatalities per million people - Minnesota
plot_fatpermillion_minnesota <- createplot_propfat_state(state, protest_years, 23)

# Total fatalities by race - Minnesota
plot_racefat_minnesota <- createplot_fatbyrace_state(state, protest_years, 24)

# Proportion of fatalities by race - Minnesota
plot_prop_minnesota <- createplot_raceproportion_state(state, protest_years, 25)