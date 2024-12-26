#rm(list=ls())
library(tidyr)
library(jtools)
library(readr) 
library(readxl)
library(dplyr)
library(fuzzyjoin)
library(ggplot2)
setwd("/Users/rahilchadha/Desktop/Data Project #1")

NBAWinPercIndex <- read_excel("NBAWinPercIndex.xlsx")
NBATeamValues11 <- read_excel("NBATeamValues11.xlsx")
NBATeamValues12 <- read_excel("NBATeamValues12.xlsx")
NBATeamValues13 <- read_excel("NBATeamValues13.xlsx")
NBATeamValues14 <- read_excel("NBATeamValues14.xlsx")
NBATeamValues15 <- read_excel("NBATeamValues15.xlsx")
NBATeamValues16 <- read_excel("NBATeamValues16.xlsx")
NBATeamValues16 <- read_excel("NBATeamValues16.xlsx")
NBATeamValues17 <- read_excel("NBATeamValues17.xlsx")
NBATeamValues18 <- read_excel("NBATeamValues18.xlsx")
NBATeamValues19 <- read_excel("NBATeamValues19.xlsx")
NBATeamValues20 <- read_excel("NBATeamValues20.xlsx")
NBATeamValues21 <- read_excel("NBATeamValues21.xlsx")
NBAFanCostIndex <- read_excel("NBAFCIindex.xls")

############################################################################################################################################
#CLEANING Fan Cost Index FROM 91-21

# Convert TEAM column to uppercase for case-insensitive comparison
NBAFanCostIndex$TEAM <- toupper(trimws(NBAFanCostIndex$TEAM))

# Convert relevant_teams to uppercase
relevant_teams <- toupper(c(
  "New York Knicks", "Los Angeles Lakers", "Golden State Warriors", "Chicago Bulls", 
  "Boston Celtics", "Los Angeles Clippers", "Brooklyn Nets", "Houston Rockets", 
  "Dallas Mavericks", "Toronto Raptors", "Philadelphia 76ers", "Miami Heat", 
  "Portland Trail Blazers", "San Antonio Spurs", "Sacramento Kings", "Washington Wizards", 
  "Phoenix Suns", "Denver Nuggets", "Milwaukee Bucks", "Oklahoma City Thunder", 
  "Utah Jazz", "Indiana Pacers", "Atlanta Hawks", "Cleveland Cavaliers", 
  "Charlotte Hornets", "Detroit Pistons", "Orlando Magic", "Minnesota Timberwolves", 
  "New Orleans Pelicans", "Memphis Grizzlies"
))

# Filter the dataset to include rows where TEAM partially matches any relevant team
NBAFanCostIndex <- NBAFanCostIndex %>%
  filter(sapply(TEAM, function(team) any(grepl(team, relevant_teams, ignore.case = TRUE))))

# Rename year columns from "2010-11" to just "2010"
names(NBAFanCostIndex) <- gsub("^(\\d{4})-\\d{2}$", "\\1", names(NBAFanCostIndex))
names(NBAFanCostIndex) <- gsub("^(\\d{4}).*", "\\1", names(NBAFanCostIndex))

# Add the 2016 column as the average of 2015 and 2017
NBAFanCostIndex$`2016` <- rowMeans(NBAFanCostIndex[, c("2015", "2017")], na.rm = TRUE)

# Add the 2019 column as the average of 2018 and 2020
NBAFanCostIndex$`2019` <- rowMeans(NBAFanCostIndex[, c("2018", "2020")], na.rm = TRUE)

NBAFanCostIndex <- NBAFanCostIndex %>%
  relocate(`2016`, .after = `2015`) %>%  # Place 2016 after 2015
  relocate(`2019`, .after = `2018`) %>%  # Place 2019 after 2018
  mutate(across(`1991`:`2021`, as.numeric))  # Convert all year columns to numeric for consistency


# Display the updated data frame
head(NBAFanCostIndex)


############################################################################################################################################
#CLEANING WIN % FROM 91-21

# Trim any extra spaces in `Team Name` and `Team Location`
NBAWinPercIndex$`Team Location` <- trimws(NBAWinPercIndex$`Team Location`)
NBAWinPercIndex$`Team Name` <- trimws(NBAWinPercIndex$`Team Name`)

NBAWinPercIndex  <- NBAWinPercIndex %>%
  mutate(Full_Team_Name = paste(`Team Location`, `Team Name`, sep = " "))


relevant_teams <- c(
  "New York Knicks", "Los Angeles Lakers", "Golden State Warriors", "Chicago Bulls", 
  "Boston Celtics", "Los Angeles  Clippers", "Brooklyn Nets", "Houston Rockets", 
  "Dallas Mavericks", "Toronto Raptors", "Philadelphia 76ers", "Miami Heat", 
  "Portland Trail Blazers", "San Antonio Spurs", "Sacramento Kings", "Washington Wizards", 
  "Phoenix Suns", "Denver Nuggets", "Milwaukee Bucks", "Oklahoma City Thunder", 
  "Utah Jazz", "Indiana Pacers", "Atlanta Hawks", "Cleveland Cavaliers", 
  "Charlotte Hornets", "Detroit Pistons", "Orlando Magic", "Minnesota Timberwolves", 
  "New Orleans Pelicans", "Memphis Grizzlies"
)

NBAWinPercIndex <- NBAWinPercIndex %>%
  filter(Full_Team_Name %in% relevant_teams | Full_Team_Name == "Los Angeles  Clippers")

NBAWinPercIndex <- as.data.frame(NBAWinPercIndex)


# Rename year columns from "2010-11" to just "2010"
names(NBAWinPercIndex) <- gsub("^(\\d{4})-\\d{2}$", "\\1", names(NBAWinPercIndex))
names(NBAWinPercIndex) <- gsub("^(\\d{4}).*", "\\1", names(NBAWinPercIndex))


# Define the columns to keep
columns_to_keep <- c("Full_Team_Name", as.character(1991:2021))

# Check the current column names to confirm their format
colnames(NBAWinPercIndex)

# Select only the columns to keep
NBAWinPercIndex <- NBAWinPercIndex %>%
  select(all_of(columns_to_keep))

# Display the first few rows to confirm the selection
head(NBAWinPercIndex)

# Display the updated data frame
head(NBAWinPercIndex)


############################################################################################################################################
#CLEANING NBA TEAM VALUES FROM 16-21
# Function to clean and convert monetary columns to integers
convert_to_int <- function(column) {
  # Check for "B" and multiply by 1,000,000,000 (billion)
  column <- ifelse(grepl("B", column), as.numeric(gsub("[$,BM#]", "", column)) * 1e9,
                   ifelse(grepl("M", column), as.numeric(gsub("[$,BM#]", "", column)) * 1e6,
                          as.numeric(gsub("[$,BM#]", "", column))))
  return(column)
}

NBATeamValues12 <- NBATeamValues12 %>%
  mutate(
    `Current Value` = convert_to_int(`Current Value`),  
    Revenue = convert_to_int(Revenue),                 
    `Operating Income` = convert_to_int(`Operating Income`),  
    Rank = convert_to_int(Rank),               
    Team = as.character(Team)                       
  )

NBATeamValues13 <- NBATeamValues13 %>%
  mutate(
    `Current Value` = convert_to_int(`Current Value`),  
    Revenue = convert_to_int(Revenue),                 
    `Operating Income` = convert_to_int(`Operating Income`),  
    Rank = convert_to_int(Rank),               
    Team = as.character(Team)                       
  )

NBATeamValues14 <- NBATeamValues14 %>%
  mutate(
    `Current Value` = convert_to_int(`Current Value`),  
    Revenue = convert_to_int(Revenue),                 
    `Operating Income` = convert_to_int(`Operating Income`),  
    Rank = convert_to_int(Rank),               
    Team = as.character(Team)                       
  )

NBATeamValues15 <- NBATeamValues15 %>%
  mutate(
    `Current Value` = convert_to_int(`Current Value`),  
    Revenue = convert_to_int(Revenue),                 
    `Operating Income` = convert_to_int(`Operating Income`),  
    Rank = convert_to_int(Rank),               
    Team = as.character(Team)                       
  )

# Convert specific columns to integers and others to strings
NBATeamValues16 <- NBATeamValues16 %>%
  mutate(
    `Current Value` = convert_to_int(`Current Value`),  
    Revenue = convert_to_int(Revenue),                 
    `Operating Income` = convert_to_int(`Operating Income`),  
    Rank = convert_to_int(Rank),               
    Team = as.character(Team)                       
  )

NBATeamValues17 <- NBATeamValues17 %>%
  mutate(
    `Current Value` = convert_to_int(`Current Value`),  
    Revenue = convert_to_int(Revenue),                 
    `Operating Income` = convert_to_int(`Operating Income`),  
    Rank = convert_to_int(Rank),               
    Team = as.character(Team)                       
  )

NBATeamValues18 <- NBATeamValues18 %>%
  mutate(
    `Current Value` = convert_to_int(`Current Value`),  
    Revenue = convert_to_int(Revenue),                 
    `Operating Income` = convert_to_int(`Operating Income`),  
    Rank = convert_to_int(Rank),               
    Team = as.character(Team)                       
  )

NBATeamValues19 <- NBATeamValues19 %>%
  mutate(
    `Current Value` = convert_to_int(`Current Value`),  
    Revenue = convert_to_int(Revenue),                 
    `Operating Income` = convert_to_int(`Operating Income`),  
    Rank = convert_to_int(Rank),               
    Team = as.character(Team)                       
  )

NBATeamValues20 <- NBATeamValues20 %>%
  mutate(
    `Current Value` = convert_to_int(`Current Value`),  
    Revenue = convert_to_int(Revenue),                 
    `Operating Income` = convert_to_int(`Operating Income`),  
    Rank = convert_to_int(Rank),               
    Team = as.character(Team)                       
  )

NBATeamValues21 <- NBATeamValues21 %>%
  mutate(
    `Current Value` = convert_to_int(`Current Value`),  
    Revenue = convert_to_int(Revenue),                 
    `Operating Income` = convert_to_int(`Operating Income`),  
    Rank = convert_to_int(Rank),               
    Team = as.character(Team)                       
  )

head(NBATeamValues12)

############################################################################################################################################
#JOINING WINNING % WITH TEAM VALUES & FAN COST INDEX FROM YEARS 16-21


# Step 1: Melt the NBAWinPercIndex dataset to long format
NBAWinPercIndex <- NBAWinPercIndex %>%
  mutate(across(`1991`:`2021`, as.character))

NBAWinPercIndex_long <- NBAWinPercIndex %>%
  pivot_longer(cols = `1991`:`2021`, names_to = "Year", values_to = "Winning_Percentage")

NBAFanCostIndex_long <- NBAFanCostIndex %>%
  pivot_longer(cols = `1991`:`2021`, names_to = "Year", values_to = "Fan_Cost_Index") %>%
  mutate(Year = as.character(Year))  # Ensure Year is a character for joining

final_data$Full_Team_Name <- toupper(trimws(final_data$Full_Team_Name))
NBAFanCostIndex_long$TEAM <- toupper(trimws(NBAFanCostIndex_long$TEAM))
NBAWinPercIndex_long$Full_Team_Name <- toupper(trimws(NBAWinPercIndex_long$Full_Team_Name))


# Step 2: Add a Year column to each NBATeamValues dataset
NBATeamValues12 <- NBATeamValues12 %>% mutate(Year = "2012")
NBATeamValues13 <- NBATeamValues13 %>% mutate(Year = "2013")
NBATeamValues14 <- NBATeamValues14 %>% mutate(Year = "2014")
NBATeamValues15 <- NBATeamValues15 %>% mutate(Year = "2015")
NBATeamValues16 <- NBATeamValues16 %>% mutate(Year = "2016")
NBATeamValues17 <- NBATeamValues17 %>% mutate(Year = "2017")
NBATeamValues18 <- NBATeamValues18 %>% mutate(Year = "2018")
NBATeamValues19 <- NBATeamValues19 %>% mutate(Year = "2019")
NBATeamValues20 <- NBATeamValues20 %>% mutate(Year = "2020")
NBATeamValues21 <- NBATeamValues21 %>% mutate(Year = "2021")

# Step 3: Combine all NBATeamValues datasets into one
NBATeamValues_combined <- bind_rows(
  NBATeamValues12, NBATeamValues13, NBATeamValues14,
  NBATeamValues15, NBATeamValues16, NBATeamValues17,
  NBATeamValues18, NBATeamValues19, NBATeamValues20,
  NBATeamValues21
)

NBATeamValues_combined$Team <- toupper(trimws(NBATeamValues_combined$Team))

# Step 4: Join the datasets on the Full Team Name and Year
final_data <- NBAWinPercIndex_long %>%
  inner_join(NBATeamValues_combined, by = c("Full_Team_Name" = "Team", "Year" = "Year"))

# Step 5: Select and arrange the final columns
final_data <- final_data %>%
  select(Full_Team_Name, Year, `Current Value`, Revenue, `Operating Income`, Rank, Winning_Percentage)

# Step 6: Join NBAFanCostIndex_long to final_data by Full_Team_Name and Year
final_data <- final_data %>%
  left_join(NBAFanCostIndex_long, by = c("Full_Team_Name" = "TEAM", "Year" = "Year"))


# Step 7: Select and arrange the final columns for better visualization
final_data <- final_data %>%
  select(Full_Team_Name, Year, `Current Value`, Revenue, `Operating Income`, Rank, Winning_Percentage, Fan_Cost_Index)

############################################################################################################################################

# CONVERTING ALL OUR VALUES TO THE 2020 DOLLARS
# Updated inflation rates for deflation and adjustments to 2020 dollars
inflation_adjustments <- c(
  "2012" = 1.1273,  # 12.73% cumulative increase from 2012 to 2020
  "2013" = 1.111,   # 11.10% cumulative increase from 2013 to 2020
  "2014" = 1.0933,  # 9.33% cumulative increase from 2014 to 2020
  "2015" = 1.092,   # 9.20% cumulative increase from 2015 to 2020
  "2016" = 1.0784,  # 7.84% cumulative increase from 2016 to 2020
  "2017" = 1.0559,  # 5.59% cumulative increase from 2017 to 2020
  "2018" = 1.0302,  # 3.02% cumulative increase from 2018 to 2020
  "2019" = 1.0123,  # 1.23% cumulative increase from 2019 to 2020
  "2020" = 1.0,     # 2020 is the reference year, no change needed
  "2021" = 0.9553   # Deflate 2021 by 4.47% to 2020 dollars (1 / 1.047)
)

# Apply the inflation adjustment to the existing columns for conversion to 2020 dollars
final_data_2020_dollars <- final_data %>%
  mutate(
    `Current Value` = ifelse(
      Year %in% names(inflation_adjustments),
      `Current Value` * inflation_adjustments[as.character(Year)],
      `Current Value`
    ),
    Revenue = ifelse(
      Year %in% names(inflation_adjustments),
      Revenue * inflation_adjustments[as.character(Year)],
      Revenue
    ),
    `Operating Income` = ifelse(
      Year %in% names(inflation_adjustments),
      `Operating Income` * inflation_adjustments[as.character(Year)],
      `Operating Income`
    ),
    `Fan_Cost_Index` = ifelse(
      Year %in% names(inflation_adjustments),
      `Fan_Cost_Index` * inflation_adjustments[as.character(Year)],
      `Fan_Cost_Index`
    )
  )

final_data_2020_dollars <- final_data_2020_dollars %>%
  mutate(Winning_Percentage = as.numeric(Winning_Percentage))

final_data_2020_dollars$Winning_Percentage <- final_data_2020_dollars$Winning_Percentage * 100


# Display the updated data frame
head(final_data_2020_dollars)


############################################################################################################################################
# REGRESSION OF REVENEUE OVER WINNING % & TEAM VALUE

# Filter the data for the Los Angeles Lakers only
lakers <- subset(final_data_2020_dollars, Full_Team_Name == "LOS ANGELES LAKERS")


# FOR THE NBA
nba_model_winning <- lm(`Current Value` ~ Winning_Percentage, data = final_data_2020_dollars)
nba_model_fancost <- lm(`Current Value` ~ Fan_Cost_Index, data = final_data_2020_dollars)

# FOR THE LAKERS
lakers_model_winning <- lm(`Current Value` ~ Winning_Percentage,  data = lakers_data)
lakers_model_fancost <- lm(`Current Value` ~ Fan_Cost_Index,  data = lakers_data)

# Display the summary of the regression model
summary(nba_model_winning)
summary(nba_model_fancost)
summary(lakers_model_winning)
summary(lakers_model_fancost)

############################################################################################################################################
# YEAR VS MARKET VALUE: GRAPH

# Filter the data for Los Angeles Lakers between 2016 and 2021
lakers_data_filtered <- lakers_data %>%
  filter(Year >= 2016 & Year <= 2021)

# Plot the graph
ggplot(lakers_data_filtered, aes(x = as.numeric(Year), y = `Current Value`)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Los Angeles Lakers: Year vs Market Value (Adjusted to 2020 Dollars)",
       x = "Year",
       y = "Market Value (in 2020 Dollars)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2016, 2021, by = 1)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-9, suffix = "B"))


############################################################################################################################################
# WINNING % VS YEAR FOR LAKERS

# Ensure Winning_Percentage is numeric
lakers_data <- lakers_data %>%
  mutate(Winning_Percentage = as.numeric(Winning_Percentage))

# Plot the graph for Year vs Winning Percentage
ggplot(lakers_data, aes(x = as.numeric(Year), y = Winning_Percentage)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "orange", size = 2) +
  labs(title = "Los Angeles Lakers: Year vs Winning Percentage",
       x = "Year",
       y = "Winning Percentage") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(as.numeric(lakers_data$Year)), max(as.numeric(lakers_data$Year)), by = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

############################################################################################################################################
# YEAR VS FAN COST FOR LAKERS


# Plot the graph for Year vs Fan Cost Index
ggplot(lakers_data, aes(x = as.numeric(Year), y = Fan_Cost_Index)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "orange", size = 2) +
  labs(title = "Los Angeles Lakers: Year vs Fan Cost Index",
       x = "Year",
       y = "Fan Cost Index (Adjusted to 2020 Dollars)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(as.numeric(lakers_data$Year)), max(as.numeric(lakers_data$Year)), by = 1)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1))

