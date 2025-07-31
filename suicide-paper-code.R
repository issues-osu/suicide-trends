library(dplyr)
library(readr)
library(lubridate)
library(gtsummary)
library(ggplot2)
library(tmap)
library(sf)
library(officer)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(flextable)
library(purrr)
library(gridExtra)  
library(broom) 
library(tigris)
library(tidycensus)
library(zoo)
library(tidyr)
library(tidycensus)
library(stats)
library(emmeans)
library(MASS)
data <- read_csv("D:/data/Santa Clara/Medical_Examiner-Coroner__Full_dataset (1).csv")

data_filtered <- data %>%
  dplyr::select(
    `Manner of Death`, 
    Age, 
    Race, 
    Gender, 
    `Death Date`, 
    `Death City`, 
    `Resident City`, 
    `Cause of Death`, 
    `Other Significant Condition`, 
    `Incident Location`,
    Latitude, 
    Longitude
  ) %>% dplyr::filter(Gender != "Non-Binary")

char_cols <- dplyr::select(data_filtered, where(is.character)) %>% names()

# Replace 'N/A' with NA in character columns only
data_clean <- data_filtered %>%
  mutate(across(all_of(char_cols), ~na_if(., 'N/A'))) %>%
  filter(Age != -1) %>%
  filter(`Manner of Death` == "Suicide") %>%
  mutate(
    Death_Date = as.Date(`Death Date`, format = "%m/%d/%Y"),
    Year = year(Death_Date),
    Month = month(Death_Date)
  ) %>%
  filter(Year >= 2018 & Year <= 2023) 

data_recoded <- data_clean %>%
  mutate(Race = case_when(
    `Race` %in% c("American Indian", "American Indian / Alaskan Native") ~ "AIAN",
    `Race` %in% c("Asian", "Asian; White") ~ "Asian",
    `Race` %in% c("BlackAfricanAmerican") ~ "Black",
    `Race` %in% c("Hispanic/Latino", "Hispanic/Latino; Other; Pacific Islander / Native Hawaiian; White", "BlackAfricanAmerican; Hispanic/Latino; White") ~ "Hispanic",
    `Race` %in% c("Pacific Islander / Native Hawaiian", "OtherPacificIslander", "Pacific Islander / Native Hawaiian") ~ "NHPI",
    `Race` == "White" ~ "White",
    TRUE ~ "Other"
  )) %>% dplyr::filter(Race != "AIAN" & Race != "NHPI" & Race != "Other")


print(data_recoded %>% count(Race))

data_filtered <- data_recoded %>%
  mutate(`Death Date` = as.Date(`Death Date`, format = "%m/%d/%Y")) %>%
  mutate(Year = year(`Death Date`)) %>%
  filter(Year <= 2023)

data_filtered <- data_recoded %>%
  filter(`Manner of Death` == "Suicide") %>%
  mutate(
    Death_Date = as.Date(`Death Date`, format = "%m/%d/%Y"),
    Year = year(Death_Date)
     ) %>%
  filter(Year >= 2018 & Year <= 2023) %>%
  count(Year) %>%
  arrange(Year) %>%
  mutate(Moving_Avg = rollapply(n, width = 3, FUN = mean, fill = NA, align = "center"))  # Moving average with a 3-month window

ggplot(data_filtered, aes(x = Year)) +
  geom_line(aes(y = n, color = "Number of Suicides"), size = .8) +
  geom_line(aes(y = Moving_Avg, color = "Moving Average"), linetype = "dashed", size = .8) +
  labs(title = "Suicide Count by Year with Moving Average",
       y = "Count",
       x = "Year",
       color = "Legend") +
  theme_minimal() + ylim(0, 200) +
  scale_color_manual(values = c("Number of Suicides" = "grey30", "Moving Average" = "red"))

data_filtered <- data_recoded %>%
  filter(`Manner of Death` == "Suicide") %>%
  mutate(
    Death_Date = as.Date(`Death Date`, format = "%m/%d/%Y"),
    Year = year(Death_Date),
    Month = month(Death_Date)
  ) %>%
  filter(Year >= 2018 & Year <= 2023) %>%
  group_by(Year, Month) %>%
  summarise(n = n()) %>%
  arrange(Year, Month) %>%
  ungroup() %>%
  mutate(
    Date = as.Date(paste(Year, Month, "01", sep = "-")),  # Correct date format
    Moving_Avg = rollapply(n, width = 3, FUN = mean, fill = NA, align = "center")
  )

covid_start <- as.Date("2020-12-01")
covid_end <- as.Date("2021-01-31")
shelter_in_place <- as.Date("2020-03-16")
risk_reduction_order <- as.Date("2020-07-02")
stay_at_home_lift <- as.Date("2021-01-25")
mask_mandate <- as.Date("2022-02-22")
# Define the eviction moratorium and rent relief key dates
eviction_moratorium_start <- as.Date("2020-04-01")
eviction_moratorium_end <- as.Date("2021-08-31")   
rent_relief_end <- as.Date("2022-03-31")

# Plot with additional vertical lines for policy dates
ggplot(data_filtered, aes(x = Date, y = n)) +
  geom_line(aes(color = "Number of Suicides"), size = 1.2) +
  geom_line(aes(y = Moving_Avg, color = "Moving Average"), linetype = "dashed", size = .8) +
  
  # COVID height period
  geom_vline(xintercept = as.numeric(covid_start), linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = as.numeric(covid_end), linetype = "dashed", color = "red", size = 1) +
  
  # Add key policy dates
  geom_vline(xintercept = as.numeric(shelter_in_place), linetype = "solid", color = "green", size = 1) +
  geom_vline(xintercept = as.numeric(risk_reduction_order), linetype = "solid", color = "blue", size = 1) +
  geom_vline(xintercept = as.numeric(stay_at_home_lift), linetype = "solid", color = "purple", size = 1) +
  geom_vline(xintercept = as.numeric(mask_mandate), linetype = "solid", color = "orange", size = 1) +
  
  # Eviction moratorium and rent relief
  geom_vline(xintercept = as.numeric(eviction_moratorium_start), linetype = "solid", color = "brown", size = 1) +
  geom_vline(xintercept = as.numeric(eviction_moratorium_end), linetype = "solid", color = "pink", size = 1) +
  geom_vline(xintercept = as.numeric(rent_relief_end), linetype = "solid", color = "limegreen", size = 1) +
  
  # Labels for the dates
  annotate("text", x = eviction_moratorium_start, y = 23, label = "Eviction Moratorium Start", angle = 90, vjust = -0.5, color = "brown") +
  annotate("text", x = eviction_moratorium_end, y = 24, label = "Eviction Moratorium End", angle = 90, vjust = -0.5, color = "pink") +
  annotate("text", x = rent_relief_end, y = 24, label = "Rent Relief Ends", angle = 90, vjust = -0.5, color = "limegreen") +
  
  # Existing annotations
  annotate("text", x = shelter_in_place, y = 24, label = "Shelter-in-Place", angle = 90, vjust = -0.5, color = "green") +
  annotate("text", x = risk_reduction_order, y = 24, label = "Risk Reduction Order", angle = 90, vjust = -0.5, color = "blue") +
  annotate("text", x = stay_at_home_lift, y = 24, label = "Stay-at-Home Lift", angle = 90, vjust = -0.5, color = "purple") +
  annotate("text", x = mask_mandate, y = 24, label = "Mask Mandate", angle = 90, vjust = -0.5, color = "orange") +
  
  labs(title = "Suicides by Month and Year with Moving Average and Key COVID Policies",
       y = "Number of Suicides",
       x = "Date",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Number of Suicides" = "grey30", "Moving Average" = "darkblue")) +
  ylim(0, 30) +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "3 months") +
  
  # Move legend to the top and rotate x-axis labels
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Create the plot, faceted by year
ggplot(data_filtered, aes(x = Date, y = n)) +
  geom_line(aes(color = "Number of Suicides"), size = .8) +
  geom_line(aes(y = Moving_Avg, color = "Moving Average"), linetype = "dashed", size = 0.8) +
  
   labs(title = "Number of Suicides by Month and Year",
       y = "Number of Suicides",
       x = "Month",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Number of Suicides" = "grey30", "Moving Average" = "darkblue")) +
  ylim(0, 30) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +  # Show only months
  
  # Move legend to the top and rotate x-axis labels
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  
  # Facet by year
  facet_wrap(~ Year, scales = "free_x")  # Ensure only months for the corresponding year are shown

data_filtered <- data_recoded %>%
  filter(`Manner of Death` == "Suicide") %>%
  mutate(
    Death_Date = as.Date(`Death Date`, format = "%m/%d/%Y"),
    Year = year(Death_Date)  # Only keep Year
  ) %>%
  filter(Year >= 2018 & Year <= 2023) %>%
  count(Year, Race) %>%  # Remove Month_Year from the count
  arrange(Year) %>%
  group_by(Year, Race) %>%
  mutate(Moving_Avg = rollapply(n, width = 3, FUN = mean, fill = NA, align = "center"))

ggplot(data_filtered, aes(x = Year, y = n, color = Race, group = Race)) +
  geom_line(size = .8) +
  geom_point(size = 2) +  # Optional: to add points on the line
  labs(title = "Suicides by Year and Race",
       y = "Number of Suicides",
       x = "Year") +
  theme_minimal() +
  theme(legend.position = "top")

# Prepare the data
data_annual <- data_recoded %>%
  filter(`Manner of Death` == "Suicide") %>%
  mutate(
    Death_Date = as.Date(`Death Date`, format = "%m/%d/%Y"),
    Year = year(Death_Date)
  ) %>%
  filter(Year >= 2018 & Year <= 2023) %>%
  filter(!Race %in% c("NHPI", "Other", "AIAN", NA)) %>%  # Exclude specific races
  count(Year, Race)  # Count suicides by year and race

data_filtered <- data_recoded %>%
  mutate(`Death Date` = as.Date(`Death Date`, format = "%m/%d/%Y")) %>%
  mutate(Year = year(`Death Date`)) %>%
  filter(Year <= 2023)

shapefile <- st_read("D:/data/Santa Clara/City_Spheres_of_Influence_-3149486507366854953.geojson")

# Join the data with the shapefile based on Death City
data_city <- data_recoded %>%
  group_by(`Resident City`) %>%
  summarise(Deaths = n(), .groups = 'drop') %>%
  right_join(shapefile, by = c("Resident City" = "SOI")) %>%
  st_as_sf()

# Create the map
tmap_mode("plot")
tm_shape(data_city) + tm_borders() +
  tm_shape(data_city) +
  tm_bubbles(size = "Deaths", col = "blue", alpha = 0.5, legend.size.show = TRUE) +
  tm_basemap("OpenStreetMap")

# Create a table for cases where Death City is different from Resident City
different_city_table <- data_recoded %>%
  filter(`Death City` != `Resident City`) %>%
  count(`Death City`, `Resident City`) %>%
  arrange(desc(n))

# Print the table to console or save it to a file
print(different_city_table)

acs_data <- get_acs(
  geography = "place",
  variables = c(
    white_male_0_4 = "B01001A_003", white_female_0_4 = "B01001A_018",
    white_male_5_9 = "B01001A_004", white_female_5_9 = "B01001A_019",
    white_male_10_14 = "B01001A_005", white_female_10_14 = "B01001A_020",
    white_male_15_17 = "B01001A_006", white_female_15_17 = "B01001A_021",
    white_male_18_19 = "B01001A_007", white_female_18_19 = "B01001A_022",
    white_male_20_24 = "B01001A_008", white_female_20_24 = "B01001A_023",
    white_male_25_29 = "B01001A_009", white_female_25_29 = "B01001A_024",
    white_male_30_34 = "B01001A_010", white_female_30_34 = "B01001A_025",
    white_male_35_44 = "B01001A_011", white_female_35_44 = "B01001A_026",
    white_male_45_54 = "B01001A_012", white_female_45_54 = "B01001A_027",
    white_male_55_64 = "B01001A_013", white_female_55_64 = "B01001A_028",
    white_male_65_74 = "B01001A_014", white_female_65_74 = "B01001A_029",
    white_male_75_84 = "B01001A_015", white_female_75_84 = "B01001A_030",
    white_male_85_plus = "B01001A_016", white_female_85_plus = "B01001A_031",
    black_male_0_4 = "B01001B_003", black_female_0_4 = "B01001B_018",
    black_male_5_9 = "B01001B_004", black_female_5_9 = "B01001B_019",
    black_male_10_14 = "B01001B_005", black_female_10_14 = "B01001B_020",
    black_male_15_17 = "B01001B_006", black_female_15_17 = "B01001B_021",
    black_male_18_19 = "B01001B_007", black_female_18_19 = "B01001B_022",
    black_male_20_24 = "B01001B_008", black_female_20_24 = "B01001B_023",
    black_male_25_29 = "B01001B_009", black_female_25_29 = "B01001B_024",
    black_male_30_34 = "B01001B_010", black_female_30_34 = "B01001B_025",
    black_male_35_44 = "B01001B_011", black_female_35_44 = "B01001B_026",
    black_male_45_54 = "B01001B_012", black_female_45_54 = "B01001B_027",
    black_male_55_64 = "B01001B_013", black_female_55_64 = "B01001B_028",
    black_male_65_74 = "B01001B_014", black_female_65_74 = "B01001B_029",
    black_male_75_84 = "B01001B_015", black_female_75_84 = "B01001B_030",
    black_male_85_plus = "B01001B_016", black_female_85_plus = "B01001B_031",
    hispanic_male_0_4 = "B01001I_003", hispanic_female_0_4 = "B01001I_018",
    hispanic_male_5_9 = "B01001I_004", hispanic_female_5_9 = "B01001I_019",
    hispanic_male_10_14 = "B01001I_005", hispanic_female_10_14 = "B01001I_020",
    hispanic_male_15_17 = "B01001I_006", hispanic_female_15_17 = "B01001I_021",
    hispanic_male_18_19 = "B01001I_007", hispanic_female_18_19 = "B01001I_022",
    hispanic_male_20_24 = "B01001I_008", hispanic_female_20_24 = "B01001I_023",
    hispanic_male_25_29 = "B01001I_009", hispanic_female_25_29 = "B01001I_024",
    hispanic_male_30_34 = "B01001I_010", hispanic_female_30_34 = "B01001I_025",
    hispanic_male_35_44 = "B01001I_011", hispanic_female_35_44 = "B01001I_026",
    hispanic_male_45_54 = "B01001I_012", hispanic_female_45_54 = "B01001I_027",
    hispanic_male_55_64 = "B01001I_013", hispanic_female_55_64 = "B01001I_028",
    hispanic_male_65_74 = "B01001I_014", hispanic_female_65_74 = "B01001I_029",
    hispanic_male_75_84 = "B01001I_015", hispanic_female_75_84 = "B01001I_030",
    hispanic_male_85_plus = "B01001I_016", hispanic_female_85_plus = "B01001I_031",
    asian_male_0_4 = "B01001D_003", asian_female_0_4 = "B01001D_018",
    asian_male_5_9 = "B01001D_004", asian_female_5_9 = "B01001D_019",
    asian_male_10_14 = "B01001D_005", asian_female_10_14 = "B01001D_020",
    asian_male_15_17 = "B01001D_006", asian_female_15_17 = "B01001D_021",
    asian_male_18_19 = "B01001D_007", asian_female_18_19 = "B01001D_022",
    asian_male_20_24 = "B01001D_008", asian_female_20_24 = "B01001D_023",
    asian_male_25_29 = "B01001D_009", asian_female_25_29 = "B01001D_024",
    asian_male_30_34 = "B01001D_010", asian_female_30_34 = "B01001D_025",
    asian_male_35_44 = "B01001D_011", asian_female_35_44 = "B01001D_026",
    asian_male_45_54 = "B01001D_012", asian_female_45_54 = "B01001D_027",
    asian_male_55_64 = "B01001D_013", asian_female_55_64 = "B01001D_028",
    asian_male_65_74 = "B01001D_014", asian_female_65_74 = "B01001D_029",
    asian_male_75_84 = "B01001D_015", asian_female_75_84 = "B01001D_030",
    asian_male_85_plus = "B01001D_016", asian_female_85_plus = "B01001D_031"
  ),
  state = "CA",
  year = 2022,
  survey = "acs5"
)


# View the data
head(acs_data)

population_estimates <- acs_data %>%
  group_by(NAME, variable) %>%
  summarize(estimate = sum(estimate)) %>%
  ungroup() %>%
  mutate(
    gender = case_when(
      grepl("male", variable) ~ "Male",
      grepl("female", variable) ~ "Female"
    ),
    race = case_when(
      grepl("white", variable) ~ "White",
      grepl("black", variable) ~ "Black",
      grepl("hispanic", variable) ~ "Hispanic",
      grepl("asian", variable) ~ "Asian"
    )
  ) %>%
  group_by(NAME, gender, race) %>%
  summarize(total_estimate = sum(estimate))

# View the results
print(population_estimates)
# Get city boundaries for California
all_cities_boundaries <- places(state = "CA", cb = TRUE)
# List of cities in Santa Clara County with the correct format

sc_city_names <- c(
  "San Jose", "Sunnyvale", "Santa Clara", "Mountain View", "Palo Alto",
  "Milpitas", "Campbell", "Los Gatos", "Morgan Hill", "Gilroy"
)

# Create a vector of standardized city names
sc_city_names_standardized <- sc_city_names

sc_cities_boundaries <- all_cities_boundaries %>%
  filter(NAME %in% sc_city_names_standardized)

acs_data <- acs_data %>%
  mutate(Standardized_Name = gsub(" city, California", "", NAME),
         Standardized_Name = gsub(" town, California", "", Standardized_Name),
         Standardized_Name = gsub(" village, California", "", Standardized_Name))

acs_dataR   <- acs_data %>%
  separate(variable, into = c("race", "gender", "age"), sep = "_", remove = FALSE) %>%
  mutate(age = case_when(
    age %in% c("0", "5", "10") ~ "3-14",
    age %in% c("15", "18") ~ "15-19",
    age == "20" ~ "20-24",
    age == "25" ~ "25-29",
    age == "30" ~ "30-34",
    age == "35" ~ "35-44",
    age %in% c("45", "55") ~ "45-64",
    age %in% c("65", "75", "85") ~ "65+"
  )) %>% group_by(Standardized_Name, race,gender, age) %>% 
  summarize(population = sum(estimate))


sc_cities_data <- acs_dataR %>%
  filter(Standardized_Name %in% sc_city_names_standardized)

sc_data <- sc_cities_boundaries  %>%
  left_join(sc_cities_data, by = c("NAME" = "Standardized_Name"))

sc_data <- sc_data %>%
  filter(NAME != "Mountain View") 

# Recode 'age' into categories
data_recoded <- data_recoded %>%
  mutate(age_group = cut(Age, 
   breaks = c(0, 14, 19, 24, 29, 34, 44, 64, Inf),
   labels = c("3-14", "15-19", "20-24", "25-29", "30-34", "35-44", "45-64", "65+"),
   right = TRUE))  # right = TRUE means intervals include the upper bound

suicide_by_resident <- data_recoded %>%
  group_by(`Resident City`, age_group, Race, Gender) %>%
  summarise(suicide_count = n(), .groups = 'drop')

suicide_by_resident$Gender <- tolower(suicide_by_resident$Gender)
suicide_by_resident$Race <- tolower(suicide_by_resident$Race)

#sc_data <- sc_data %>%
#  left_join(suicide_by_resident, by = c("NAME" = "Resident City"))
sc_data <- sc_data %>%
  left_join(suicide_by_resident, by = c("NAME" = "Resident City", "age" = "age_group", "race" = "Race", "gender" = "Gender")) %>%
  replace_na(list(suicide_count = 0))

sc_data$adj_rate <- round((sc_data$suicide_count/ sc_data$population)*100000,3)

sc_data_race <- sc_data %>%
  group_by(NAME, race) %>%
  summarize(deaths = sum(suicide_count), popoulation = sum(population), rate = mean(adj_rate, na.rm=TRUE))

sc_data_race_black <- sc_data_race %>% filter(race == "black")
sc_data_race_white <- sc_data_race %>% filter(race == "white")
sc_data_race_hispanic <- sc_data_race %>% filter(race == "hispanic")
sc_data_race_asian <- sc_data_race %>% filter(race == "asian")

# Count suicides by death city and race
#suicide_by_death <- data_recoded %>%
#  group_by(`Death City`, Race, Gender) %>%
#  summarise(suicideDC = n()) %>%
#  ungroup()%>%
#  pivot_wider(names_from = Race, values_from = suicideDC, values_fill = 0)

#sc_data <- sc_data %>%
#  left_join(suicide_by_death, by = c("NAME" = "Death City"))

#sc_data1 <- sc_data %>% st_drop_geometry() %>%
#  group_by(NAME, race, gender, age) %>%
#  summarize(deaths = n(), population = sum(estimate))

white <- ggplot(data = sc_data_race_white) +
  geom_sf(aes(fill = rate), color = "white") +  # Plot polygons
  scale_fill_viridis_c(option = "plasma", name = "Rate") +
  labs(title = "NH White") +
  theme_void() +
  geom_sf_text(aes(label = NAME), size = 3.5, color = "black")

black<-ggplot(data = sc_data_race_black) +
  geom_sf(aes(fill = rate), color = "white") +  # Plot polygons
  scale_fill_viridis_c(option = "plasma", name = "Rate") +
  labs(title = "Rate") +
  theme_void() +
  geom_sf_text(aes(label = NAME), size = 3.5, color = "black")

asian<-ggplot(data = sc_data_race_asian) +
  geom_sf(aes(fill = rate), color = "white") +  # Plot polygons
  scale_fill_viridis_c(option = "plasma", name = "Rate") +
  labs(title = "Asian") +
  theme_void() +
  geom_sf_text(aes(label = NAME), size = 3.5, color = "black")

hisp<- ggplot(data = sc_data_race_hispanic) +
  geom_sf(aes(fill = rate ), color = "white") +  # Plot polygons
  scale_fill_viridis_c(option = "plasma", name = "Rate") +
  labs(title = "Hispanic") +
  theme_void() +
  geom_sf_text(aes(label = NAME), size = 3.5, color = "black")

grid.arrange(white, asian, hisp, ncol = 1)

########################################################################
# Convert Race to character to avoid sum error
data_recoded$Race <- as.character(data_recoded$Race)

recode_cause_of_death <- function(cause) {
  cause <- tolower(cause)
  if (grepl("gunshot|gun", cause)) {
    return("gunshot wound")
  } else if (grepl("toxicity|overdose|poisoning", cause)) {
    return("toxicity, overdose, or poisoning")
  } else if (grepl("blunt force", cause)) {
    return("blunt force injury")
  } else if (grepl("hanging|suffocation|asphyxia|strangulation", cause)) {
    return("hanging/suffocation")
  } else if (grepl("drowning", cause)) {
    return("drowning")
  } else if (grepl("thermal|burn", cause)) {
    return("thermal injuries")
  } else if (grepl("stab|knife|sharp force", cause)) {
    return("stabbing incidents")
  } else if (grepl("skull|head|neck", cause)) {
    return("wound to skull, head or neck")
  } else {
    return("everything else")
  }
}

# Apply the recoding function to the Cause of Death column
data1 <- data_recoded %>%
  mutate(Cause_of_Death_Recoded = sapply(`Cause of Death`, recode_cause_of_death))

# Define the recoding function for "Other Significant Condition"
recode_other_significant_condition <- function(condition) {
  if (is.na(condition) || condition == "None") {
    return("None")
  }
  condition <- tolower(condition)
  categories <- c()
  
  # Detect mental health issues
  if (grepl("ptsd|posttraumatic stress", condition)) {
    categories <- c(categories, "PTSD")
  }
  if (grepl("depression|major depressive disorder|depressive disorder", condition)) {
    categories <- c(categories, "depression")
  }
  if (grepl("anxiety", condition)) {
    categories <- c(categories, "anxiety")
  }
  if (grepl("mood disorder", condition)) {
    categories <- c(categories, "mood disorder")
  }
  if (grepl("bipolar", condition)) {
    categories <- c(categories, "bipolar disorder")
  }
  
  # Detect substance abuse
  if (grepl("substance|alcohol|drug", condition)) {
    categories <- c(categories, "substance abuse")
  }
  
  # Detect previous suicide attempts/ideation
  if (grepl("suicide attempt|suicidal ideation", condition)) {
    categories <- c(categories, "previous suicide attempts/ideation")
  }
  
  # Detect chronic illnesses
  if (grepl("diabetes", condition)) {
    categories <- c(categories, "diabetes")
  }
  if (grepl("hypertension", condition)) {
    categories <- c(categories, "hypertension")
  }
  if (grepl("cardiovascular", condition)) {
    categories <- c(categories, "cardiovascular disease")
  }
  if (grepl("cancer", condition)) {
    categories <- c(categories, "cancer")
  }
  if (grepl("asthma", condition)) {
    categories <- c(categories, "asthma")
  }
  if (grepl("obesity", condition)) {
    categories <- c(categories, "obesity")
  }
  
  # Detect pain
  if (grepl("pain", condition)) {
    categories <- c(categories, "pain")
  }
  
  if (length(categories) == 0) {
    return("other")
  }
  
  return(paste(categories, collapse = "; "))
}
data1 <- data1 %>%
  mutate(Other_Significant_Condition_Recoded = sapply(`Other Significant Condition`, recode_other_significant_condition))

recode_age <- function(age) {
  if (age >= 5 & age <= 14) {
    return("5-14")
  } else if (age >= 15 & age <= 19) {
    return("15-19")
  } else if (age >= 20 & age <= 24) {
    return("20-24")
  } else if (age >= 25 & age <= 29) {
    return("25-29")
  } else if (age >= 30 & age <= 34) {
    return("30-34")
  } else if (age >= 35 & age <= 44) {
    return("35-44")
  } else if (age >= 45 & age <= 64) {
    return("45-64")
  } else if (age >= 65) {
    return("≥ 65")
  } else {
    return("Unknown")
  }
}
data1 <- data1 %>%
  mutate(Age_Recoded = sapply(Age, recode_age))

table1 <- data1 %>%
  group_by(Race, Gender, Cause_of_Death_Recoded) %>%
  summarise(Count = n()) %>%
  spread(Cause_of_Death_Recoded, Count, fill = 0)

##########################################################


difference_in_proportions_test <- function(x1, n1, x2, n2) {
  # Calculate the proportions
  p1 <- x1 / n1
  p2 <- x2 / n2
  
  # Calculate the pooled proportion
  p_pooled <- (x1 + x2) / (n1 + n2)
  
  # Calculate the standard error
  SE <- sqrt(p_pooled * (1 - p_pooled) * (1/n1 + 1/n2))
  
  # Calculate the Z-score
  Z <- (p1 - p2) / SE
  
  # Calculate the p-value
  p_value <- 2 * (1 - pnorm(abs(Z)))
  
  return(list(Z = Z, p_value = p_value))
}

difference_in_proportions_test <- function(x1, n1, x2, n2) {
  # Calculate the proportions
  p1 <- x1 / n1
  p2 <- x2 / n2
  
  # Calculate the pooled proportion
  p_pooled <- (x1 + x2) / (n1 + n2)
  
  # Calculate the standard error
  SE <- sqrt(p_pooled * (1 - p_pooled) * (1/n1 + 1/n2))
  
  # Calculate the Z-score
  Z <- (p1 - p2) / SE
  
  # Calculate the p-value
  p_value <- 2 * (1 - pnorm(abs(Z)))
  
  return(list(Z = Z, p_value = p_value))
}

data2 <- list(
  Asian = list(
    "blunt force injury" = list(female = 8, male = 11, n_female = 74, n_male = 177),
    "drowning" = list(female = 1, male = 5, n_female = 74, n_male = 177),
    "everything else" = list(female = 3, male = 4, n_female = 74, n_male = 177),
    "gunshot wound" = list(female = 4, male = 39, n_female = 74, n_male = 177),
    "hanging/suffocation" = list(female = 37, male = 92, n_female = 74, n_male = 177),
    "stabbing incidents" = list(female = 1, male = 6, n_female = 74, n_male = 177),
    "thermal injuries" = list(female = 0, male = 1, n_female = 74, n_male = 177),
    "toxicity, overdose, or poisoning" = list(female = 17, male = 19, n_female = 74, n_male = 177),
    "wound to skull, head or neck" = list(female = 3, male = 0, n_female = 74, n_male = 177)
  ),
  Black = list(
    "blunt force injury" = list(female = 0, male = 4, n_female = 9, n_male = 19),
    "drowning" = list(female = 0, male = 0, n_female = 9, n_male = 19),
    "everything else" = list(female = 2, male = 1, n_female = 9, n_male = 19),
    "gunshot wound" = list(female = 0, male = 8, n_female = 9, n_male = 19),
    "hanging/suffocation" = list(female = 4, male = 5, n_female = 9, n_male = 19),
    "stabbing incidents" = list(female = 0, male = 0, n_female = 9, n_male = 19),
    "thermal injuries" = list(female = 0, male = 0, n_female = 9, n_male = 19),
    "toxicity, overdose, or poisoning" = list(female = 3, male = 1, n_female = 9, n_male = 19),
    "wound to skull, head or neck" = list(female = 0, male = 0, n_female = 9, n_male = 19)
  ),
  Hispanic = list(
    "blunt force injury" = list(female = 0, male = 11, n_female = 46, n_male = 153),
    "drowning" = list(female = 0, male = 1, n_female = 46, n_male = 153),
    "everything else" = list(female = 5, male = 9, n_female = 46, n_male = 153),
    "gunshot wound" = list(female = 3, male = 31, n_female = 46, n_male = 153),
    "hanging/suffocation" = list(female = 32, male = 82, n_female = 46, n_male = 153),
    "stabbing incidents" = list(female = 1, male = 3, n_female = 46, n_male = 153),
    "thermal injuries" = list(female = 0, male = 1, n_female = 46, n_male = 153),
    "toxicity, overdose, or poisoning" = list(female = 5, male = 9, n_female = 46, n_male = 153),
    "wound to skull, head or neck" = list(female = 0, male = 6, n_female = 46, n_male = 153)
  ),
  White = list(
    "blunt force injury" = list(female = 15, male = 23, n_female = 125, n_male = 414),
    "drowning" = list(female = 0, male = 0, n_female = 125, n_male = 414),
    "everything else" = list(female = 13, male = 15, n_female = 125, n_male = 414),
    "gunshot wound" = list(female = 11, male = 187, n_female = 125, n_male = 414),
    "hanging/suffocation" = list(female = 46, male = 137, n_female = 125, n_male = 414),
    "stabbing incidents" = list(female = 5, male = 8, n_female = 125, n_male = 414),
    "thermal injuries" = list(female = 0, male = 2, n_female = 125, n_male = 414),
    "toxicity, overdose, or poisoning" = list(female = 33, male = 35, n_female = 125, n_male = 414),
    "wound to skull, head or neck" = list(female = 2, male = 7, n_female = 125, n_male = 414)
  )
)

results <- list()
for (race in names(data2)) {
  results[[race]] <- list()
  for (injury in names(data2[[race]])) {
    x1 <- data2[[race]][[injury]]$female
    n1 <- data2[[race]][[injury]]$n_female
    x2 <- data2[[race]][[injury]]$male
    n2 <- data2[[race]][[injury]]$n_male
    
    test_result <- difference_in_proportions_test(x1, n1, x2, n2)
    
    results[[race]][[injury]] <- test_result
  }
}

# Print the results
for (race in names(results)) {
  cat("Results for", race, ":\n")
  for (injury in names(results[[race]])) {
    cat(injury, ": Z-score =", results[[race]][[injury]]$Z, ", P-value =", results[[race]][[injury]]$p_value, "\n")
  }
  cat("\n")
}

# Crude Count of Deaths by Suicide in Chicago, IL, by Sex, Race/Ethnicity, and Age Group: 2015–2021
table2 <- data1 %>%
  dplyr::group_by(Race, Gender, Age_Recoded) %>%
  summarise(Count = n()) %>%
  spread(Age_Recoded, Count, fill = 0)
table2
######################################################

data2 <- list(
  Asian = list(
    "5-14" = list(female = 2, male = 1, n_female = 74, n_male = 177),
    "15-19" = list(female = 4, male = 15, n_female = 74, n_male = 177),
    "20-24" = list(female = 6, male = 15, n_female = 74, n_male = 177),
    "25-29" = list(female = 8, male = 18, n_female = 74, n_male = 177),
    "30-34" = list(female = 6, male = 17, n_female = 74, n_male = 177),
    "35-44" = list(female = 11, male = 24, n_female = 74, n_male = 177),
    "45-64" = list(female = 18, male = 50, n_female = 74, n_male = 177),
    ">=65" = list(female = 19, male = 37, n_female = 74, n_male = 177),
    "age_mean_sd" = list(female = c(46.57, 20.76), male = c(45.71, 20.44))
  ),
  Black = list(
    "5-14" = list(female = 0, male = 0, n_female = 9, n_male = 19),
    "15-19" = list(female = 1, male = 0, n_female = 9, n_male = 19),
    "20-24" = list(female = 0, male = 7, n_female = 9, n_male = 19),
    "25-29" = list(female = 3, male = 1, n_female = 9, n_male = 19),
    "30-34" = list(female = 1, male = 2, n_female = 9, n_male = 19),
    "35-44" = list(female = 0, male = 4, n_female = 9, n_male = 19),
    "45-64" = list(female = 3, male = 4, n_female = 9, n_male = 19),
    ">=65" = list(female = 1, male = 1, n_female = 9, n_male = 19),
    "age_mean_sd" = list(female = c(39.44, 17.49), male = c(35.95, 17.05))
  ),
  Hispanic = list(
    "5-14" = list(female = 4, male = 1, n_female = 46, n_male = 153),
    "15-19" = list(female = 5, male = 4, n_female = 46, n_male = 153),
    "20-24" = list(female = 3, male = 21, n_female = 46, n_male = 153),
    "25-29" = list(female = 8, male = 22, n_female = 46, n_male = 153),
    "30-34" = list(female = 4, male = 22, n_female = 46, n_male = 153),
    "35-44" = list(female = 9, male = 34, n_female = 46, n_male = 153),
    "45-64" = list(female = 11, male = 38, n_female = 46, n_male = 153),
    ">=65" = list(female = 2, male = 11, n_female = 46, n_male = 153),
    "age_mean_sd" = list(female = c(35.52, 16.10), male = c(39.54, 39.54))
  ),
  White = list(
    "5-14" = list(female = 0, male = 1, n_female = 125, n_male = 414),
    "15-19" = list(female = 6, male = 13, n_female = 125, n_male = 414),
    "20-24" = list(female = 5, male = 11, n_female = 125, n_male = 414),
    "25-29" = list(female = 9, male = 20, n_female = 125, n_male = 414),
    "30-34" = list(female = 3, male = 26, n_female = 125, n_male = 414),
    "35-44" = list(female = 22, male = 55, n_female = 125, n_male = 414),
    "45-64" = list(female = 51, male = 156, n_female = 125, n_male = 414),
    ">=65" = list(female = 29, male = 132, n_female = 125, n_male = 414),
    "age_mean_sd" = list(female = c(52.05, 19.45), male = c(54.93, 19.03))
  )
)

fisher_test_results <- list()
for (race in names(data2)) {
  fisher_test_results[[race]] <- list()
  for (age_group in names(data2[[race]])) {
    if (age_group != "age_mean_sd") {
      female_count <- data2[[race]][[age_group]]$female
      male_count <- data2[[race]][[age_group]]$male
      n_female <- data2[[race]][[age_group]]$n_female
      n_male <- data2[[race]][[age_group]]$n_male
      
      # Create a contingency table
      contingency_table <- matrix(c(female_count, n_female - female_count, male_count, n_male - male_count), nrow = 2)
      
      # Perform Fisher's Exact Test
      fisher_test <- fisher.test(contingency_table)
      
      fisher_test_results[[race]][[age_group]] <- fisher_test$p.value
    }
  }
}

# Print Fisher's Exact Test results
for (race in names(fisher_test_results)) {
  cat("Fisher's Exact Test results for", race, ":\n")
  for (age_group in names(fisher_test_results[[race]])) {
    cat(age_group, ": p-value =", fisher_test_results[[race]][[age_group]], "\n")
  }
  cat("\n")
}

t_test_results <- list()
for (race in names(data2)) {
  female_mean_sd <- data2[[race]]$age_mean_sd$female
  male_mean_sd <- data2[[race]]$age_mean_sd$male
  
  # Perform t-test
  t_test <- t.test(female_mean_sd, male_mean_sd)
  
  t_test_results[[race]] <- t_test$p.value
}

# Print t-test results
for (race in names(t_test_results)) {
  cat("t-test results for", race, ": p-value =", t_test_results[[race]], "\n")
}

data2 <- list(
  Asian = list(
    female = c(46.57, 20.76),  # Mean and SD for females
    male = c(45.71, 20.44)     # Mean and SD for males
  ),
  Black = list(
    female = c(39.44, 17.49),
    male = c(35.95, 17.05)
  ),
  Hispanic = list(
    female = c(35.52, 16.10),
    male = c(39.54, 39.54)
  ),
  White = list(
    female = c(52.05, 19.45),
    male = c(54.93, 19.03)
  )
)
median_test_results <- list()

# Print Mann-Whitney U Test results
for (race in names(median_test_results)) {
  cat("Mann-Whitney U Test results for", race, ": p-value =", median_test_results[[race]], "\n")
}

#################################################

library(dplyr)

# Define the variables for each group
variables <- c(
  # Black or African American Alone (Males)
  "B01001B_003", # Under 5 Years
  "B01001B_004", # 5 To 9 Years
  "B01001B_005", # 10 To 14 Years
  "B01001B_006", # 15 To 17 Years
  "B01001B_007", # 18 And 19 Years
  "B01001B_008", # 20 To 24 Years
  "B01001B_009", # 25 To 29 Years
  "B01001B_010", # 30 To 34 Years
  "B01001B_011", # 35 To 44 Years
  "B01001B_012", # 45 To 54 Years
  "B01001B_013", # 55 To 64 Years
  "B01001B_014", # 65 To 74 Years
  "B01001B_015", # 75 To 84 Years
  "B01001B_016",  # 85+ Years
  
  # Black or African American Alone (Females)
  
  "B01001B_018", # Under 5 Years
  "B01001B_019", # 5 To 9 Years
  "B01001B_020", # 10 To 14 Years
  "B01001B_021", # 15 To 17 Years
  "B01001B_022", # 18 And 19 Years
  "B01001B_023", # 20 To 24 Years
  "B01001B_024", # 25 To 29 Years
  "B01001B_025", # 30 To 34 Years
  "B01001B_026", # 35 To 44 Years
  "B01001B_027", # 45 To 54 Years
  "B01001B_028", # 55 To 64 Years
  "B01001B_029", # 65 To 74 Years
  "B01001B_030", # 75 To 84 Years
  "B01001B_031", # 85+ Years
  
  # Asian Alone (Males)
  "B01001D_003", # Under 5 Years
  "B01001D_004", # 5 To 9 Years
  "B01001D_005", # 10 To 14 Years
  "B01001D_006", # 15 To 17 Years
  "B01001D_007", # 18 And 19 Years
  "B01001D_008", # 20 To 24 Years
  "B01001D_009", # 25 To 29 Years
  "B01001D_010", # 30 To 34 Years
  "B01001D_011", # 35 To 44 Years
  "B01001D_012", # 45 To 54 Years
  "B01001D_013", # 55 To 64 Years
  "B01001D_014", # 65 To 74 Years
  "B01001D_015", # 75 To 84 Years
  "B01001D_016", # 85 Years
  
  # Asian Alone (Females)
  
  "B01001D_018", # Under 5 Years
  "B01001D_019", # 5 To 9 Years
  "B01001D_020", # 10 To 14 Years
  "B01001D_021", # 15 To 17 Years
  "B01001D_022", # 18 And 19 Years
  "B01001D_023", # 20 To 24 Years
  "B01001D_024", # 25 To 29 Years
  "B01001D_025", # 30 To 34 Years
  "B01001D_026", # 35 To 44 Years
  "B01001D_027", # 45 To 54 Years
  "B01001D_028", # 55 To 64 Years
  "B01001D_029", # 65 To 74 Years
  "B01001D_030", # 75 To 84 Years
  "B01001D_031", # 85 +
  
  # Hispanic or Latino (Males)
  "B01001I_003", # Under 5 Years
  "B01001I_004", # 5 To 9 Years
  "B01001I_005", # 10 To 14 Years
  "B01001I_006", # 15 To 17 Years
  "B01001I_007", # 18 And 19 Years
  "B01001I_008", # 20 To 24 Years
  "B01001I_009", # 25 To 29 Years
  "B01001I_010", # 30 To 34 Years
  "B01001I_011", # 35 To 44 Years
  "B01001I_012", # 45 To 54 Years
  "B01001I_013", # 55 To 64 Years
  "B01001I_014", # 65 To 74 Years
  "B01001I_015", # 75 To 84 Years
  "B01001I_016", # 85 Years
  
  # Hispanic or Latino (Females)
  "B01001I_018", # Under 5 Years
  "B01001I_019", # 5 To 9 Years
  "B01001I_020", # 10 To 14 Years
  "B01001I_021", # 15 To 17 Years
  "B01001I_022", # 18 And 19 Years
  "B01001I_023", # 20 To 24 Years
  "B01001I_024", # 25 To 29 Years
  "B01001I_025", # 30 To 34 Years
  "B01001I_026", # 35 To 44 Years
  "B01001I_027", # 45 To 54 Years
  "B01001I_028", # 55 To 64 Years
  "B01001I_029", # 65 To 74 Years
  "B01001I_030",  # 75 To 84 Years
  "B01001I_031",  # 85 Years
  
  # White Alone (Males)
  "B01001H_003",# Under 5 Years
  "B01001H_004", # 5 To 9 Years
  "B01001H_005", # 10 To 14 Years
  "B01001H_006",  # 15 To 17 Years
  "B01001H_007", # 18 And 19 Years
  "B01001H_008", # 20 To 24 Years
  "B01001H_009", # 25 To 29 Years
  "B01001H_010", # 30 To 34 Years
  "B01001H_011", # 35 To 44 Years
  "B01001H_012", # 45 To 54 Years
  "B01001H_013", # 55 To 64 Years
  "B01001H_014", # 65 To 74 Years
  "B01001H_015", # 75 To 84 Years
  "B01001H_016", # 85+ Years
  
  # White Alone (Females)
  "B01001H_018", # Under 5 Years 
  "B01001H_019", # 5 To 9 Years
  "B01001H_020",  # 10 To 14 Years
  "B01001H_021", # 15 To 17 Years
  "B01001H_022", # 18 To 19 Years
  "B01001H_023", # 20 To 24 Years
  "B01001H_024", # 25 To 29 Years
  "B01001H_025", # 30 To 34 Years
  "B01001H_026", # 35 To 44 Years
  "B01001H_027", # 45 To 54 Years
  "B01001H_028", # 55 To 64 Years
  "B01001H_029", # 65 To 74 Years
  "B01001H_030", # 75 To 84 Years
  "B01001H_031"  # 85+ Years
)

# Display the final list of variables
print(variables)

# Define corresponding age groups
age_groups <- c(
  rep("Under 5", 1), rep("5-9", 1), rep("10-14", 1), rep("15-17", 1), rep("18-19", 1), rep("20-24", 1),
  rep("25-29", 1), rep("30-34", 1), rep("35-44", 1), rep("45-54", 1), rep("55-64", 1), rep("65-74", 1), rep("75-84", 1), rep("85+", 1)
)

# Create the age group dataframe
age_group_df <- data.frame(
  variable = variables,
  age_group = c(rep(age_groups, 8)), # Repeat age groups for all race and gender categories
  race = rep(c("Black", "Black", "Asian", "Asian", "Hispanic", "Hispanic", "White", "White"), each = 14),
  gender = rep(c("Male", "Female"), times = 4, each = 14)
)

# View the dataframe
print(age_group_df)



# Fetch the data
population_data <- get_acs(
  geography = "county",
  variables = variables,
  state = "CA",
  county = "Santa Clara",
  year = 2022
)


# Join the population data with the age categories
population_data <- population_data %>%
  left_join(age_group_df, by = "variable")

# Display the final dataset with age categories
print(population_data)

 
# Summarize the data
summarized_data <- population_data %>%
 # left_join(age_group_df, by = "variable") %>%
  group_by(age_group, race, gender) %>%
  summarise(total_estimate = sum(estimate, na.rm = TRUE),
            total_moe = sum(moe, na.rm = TRUE),
            .groups = "drop")

df_recode <- summarized_data %>%
  mutate(
    age_group = case_when(
      age_group %in% c("Under 5", "5-9", "10-14") ~ "5-14",
      age_group %in% c("15-17", "18-19") ~ "15-19",
      age_group %in% c("20-24") ~ "20-24",
      age_group %in% c("25-29") ~ "25-29",
      age_group %in% c("30-34") ~ "30-34",
      age_group %in% c("35-44") ~ "35-44",
      age_group %in% c("45-54", "55-64") ~ "45-64",
      age_group %in% c("65-74", "75-84", "85+") ~ "≥ 65",
      TRUE ~ age_group  # fallback, if necessary
    )
  )

# Group by the new age_group, race, and gender, and sum the total_estimate
df_summarized <- df_recode %>%
  group_by(age_group, race, gender) %>%
  summarise(total_estimate = sum(total_estimate)) %>%
  ungroup()

# Merge the data
summarized_data <- function(age) {
  if (age >= 0 & age <= 14) {
    return("5-14")
  } else if (age >= 15 & age <= 19) {
    return("15-19")
  } else if (age >= 20 & age <= 24) {
    return("20-24")
  } else if (age >= 25 & age <= 29) {
    return("25-29")
  } else if (age >= 30 & age <= 34) {
    return("30-34")
  } else if (age >= 35 & age <= 44) {
    return("35-44")
  } else if (age >= 45 & age <= 64) {
    return("45-64")
  } else if (age >= 65) {
    return("≥ 65")
  } else {
    return(NA)
  }
}
data_recoded <- data_recoded %>%
  mutate(Age_Recoded = sapply(Age, summarized_data))
merged_data <- merge(data_recoded, df_summarized, by.x = c("Age_Recoded", "Race", "Gender"), by.y = c("age_group", "race", "gender"), all.x = TRUE)


#############################

#data_recoded <- data_recoded %>%
#  mutate(Age_Recoded = sapply(Age, summarized_data))

merged_data <- merged_data %>% dplyr::filter(Race != "AIAN" & Race != "NHPI" & Race != "Other")
data <- merged_data
merged_data$`Death Date` <- as.Date(data$`Death Date`, format = "%m/%d/%Y %H:%M")
categorize_location <- function(location) {
  location <- tolower(location)  # Make the location string lowercase to standardize
  
  # Specific checks for lighthouse and lightrail
  if (grepl("lighthouse", location)) {
    return("Open Space")
  } else if (grepl("lightrail", location)) {
    return("Transportation Structure")
  }
  
  # Assign categories based on keywords in the LOCATION column
  if (grepl("residence inn", location)) {
    return("Hotel/Motel")  # Specifically classify "Residence Inn" as a Hotel/Motel
  } else if (grepl("hotel|motel|marriott|inn|holiday inn|extended stay|best western", location)) {
    return("Hotel/Motel")
  } else if (grepl("home|valerian court|john street|closet|residential|main street|unit|yolo drive|morrill|crescent|overland|nuthatch lane|moorpark avenue|apartment|4th street|residence|house|bedroom|trailor|apt", location)) {
    return("Residential")
  } else if (grepl("\\bpark\\b|shed|field|tree|cemetery|alma|courtyard|beach|forest|golf|mile marker|charleston rd|pathway|open space|preserve|alum rock ave|creek|ravine|trail|atrium|sidewalk|wilderness|levy|backyard|reservoir", location)) {
    return("Open Space")  # Only the word "park" will be classified here
  } else if (grepl("hospital|prison|scvmc|detention|la selva group|sober living|secured facility|correctional|jail|psychiatric", location)) {
    return("Secured Facility")
  } else if (grepl("road|highway|on-ramp|tracks|intersection|rail|bridge|onramp|overpass|station|railroad|freeway|train|transport|hwy", location)) {
    return("Transportation Structure")
  } else if (grepl("car|automobile|parking|garage|driveway|parked|vehicle", location)) {
    return("Parking Structure")  # Keywords for parking and garage-related locations
  } else if (grepl("building|orchard street|nasa|funeral home|willow|floor|leavesley rd|office|wild wings|loading dock|workplace|business|superstore|store|restaurant|work|achievement fitness", location)) {
    return("Business/Building")
  } else if (grepl("university|dorm|jose|stanford|school", location)) {
    return("School/University")
  } else {
    return("Other/Unknown")
  }
}
#note i looked up all addresses to correctly classify them

# Apply the categorization to the dataset
suicide_data <- merged_data %>%
  mutate(Location_Category = sapply(`Incident Location`, categorize_location))

# View the updated dataset with the new 'Location_Category' column
head(suicide_data)
######################################
suicide_data$`Death Date` <- as.Date(suicide_data$`Death Date`, format = "%m/%d/%Y %H:%M")
suicide_data$Year <- year(suicide_data$`Death Date`)

regression_data <- suicide_data %>%
  group_by(Race, Gender, Age_Recoded, Year) %>%
  summarise(deaths = n(), population = unique(total_estimate)) %>%
  ungroup()
  
data_rates <- regression_data %>%
  mutate(death_rate = (deaths / population) * 100000)

regression_data <- regression_data %>% dplyr::filter(Gender != "Non-Binary")
data_list <- regression_data %>%  
  group_by(Race, Gender) %>%
  group_split()

# Name each data frame in the list for easier access
names(data_list) <- regression_data %>%
  group_by(Race, Gender) %>%
  group_keys() %>%
  mutate(name = paste(Race, Gender, sep = "_")) %>%
  pull(name)

# Print the names of the data frames
names(data_list)

asian_female <- data_list[["Asian_Female"]]

model_list <- list()

# Iterate over each data frame in the list and fit the model
for (name in names(data_list)) {
  data_subset <- data_list[[name]]
  
  # Fit the negative binomial model
  model <- glm.nb(deaths ~ as.numeric(Year) + offset(log(population)), data = data_subset)
  
  # Store the model in the list
  model_list[[name]] <- model
}

# Print the summary of the first model as an example
summary(model_list[[4]])

results_table <- data.frame(Race = character(), Gender = character(), Exp_Coefficient_Year = numeric(), CI_Lower = numeric(), CI_Upper = numeric(), stringsAsFactors = FALSE)
# Iterate over each model and exponentiate the coefficient for year
for (name in names(model_list)) {
  model <- model_list[[name]]
  tidy_model <- tidy(model, conf.int = TRUE)
  
  # Check if the year coefficient exists
  if ("as.numeric(Year)" %in% tidy_model$term) {
    # Extract the exponentiated coefficient for year and its confidence interval
    exp_coef <- exp(tidy_model$estimate[tidy_model$term == "as.numeric(Year)"])
    conf_int <- exp(tidy_model[tidy_model$term == "as.numeric(Year)", c("conf.low", "conf.high")])
    
    # Extract race and gender from the name
    parts <- strsplit(name, "_")[[1]]
    race <- parts[1]
    gender <- parts[2]
     
    
    # Append results to the table
    results_table <- rbind(results_table, data.frame(Race = race, Gender = gender, Exp_Coefficient_Year = exp_coef, CI_Lower = conf_int$conf.low, CI_Upper = conf_int$conf.high))
  }
}

# Print the results table
print(results_table)

#####################################################################
data_list <- regression_data %>%  
  group_by(Race, Gender) %>%
  group_split()

# Name each data frame in the list for easier access
names(data_list) <- regression_data %>%
  group_by(Race, Gender) %>%
  group_keys() %>%
  mutate(name = paste(Race, Gender, sep = "_")) %>%
  pull(name)

# Print the names of the data frames
names(data_list)

asian_female <- data_list[["Asian_Female"]]

model_list <- list()

# Iterate over each data frame in the list and fit the model
for (name in names(data_list)) {
  data_subset <- data_list[[name]]
  
  # Fit the negative binomial model
  model <- glm.nb(deaths ~ as.numeric(Year)  , data = data_subset)
  
  # Store the model in the list
  model_list[[name]] <- model
}

# Print the summary of the first model as an example
summary(model_list[[4]])

results_table <- data.frame(Race = character(), Gender = character(), Exp_Coefficient_Year = numeric(), CI_Lower = numeric(), CI_Upper = numeric(), stringsAsFactors = FALSE)
# Iterate over each model and exponentiate the coefficient for year
for (name in names(model_list)) {
  model <- model_list[[name]]
  tidy_model <- tidy(model, conf.int = TRUE)
  
  # Check if the year coefficient exists
  if ("as.numeric(Year)" %in% tidy_model$term) {
    # Extract the exponentiated coefficient for year and its confidence interval
    exp_coef <- exp(tidy_model$estimate[tidy_model$term == "as.numeric(Year)"])
    conf_int <- exp(tidy_model[tidy_model$term == "as.numeric(Year)", c("conf.low", "conf.high")])
    
    # Extract race and gender from the name
    parts <- strsplit(name, "_")[[1]]
    race <- parts[1]
    gender <- parts[2]
    
    
    # Append results to the table
    results_table <- rbind(results_table, data.frame(Race = race, Gender = gender, Exp_Coefficient_Year = exp_coef, CI_Lower = conf_int$conf.low, CI_Upper = conf_int$conf.high))
  }
}

# Print the results table
print(results_table)
#####################################################################

# Split the data into groups
data_list <- regression_data %>%
  group_by(Age_Recoded, Race, Gender) %>%
  group_split()

# Name each data frame in the list for easier access
names(data_list) <- regression_data %>%
  group_by(Age_Recoded, Race, Gender) %>%
  group_keys() %>%
  mutate(name = paste(Age_Recoded, Race, Gender, sep = "_")) %>%
  pull(name)

# Print the names of the data frames
print(names(data_list))

# Filter out tibbles with fewer than 6 rows
filtered_data_list <- data_list[sapply(data_list, nrow) >= 2]
print(names(filtered_data_list))

# Example access to a specific data frame
White_Female30_34 <- filtered_data_list[["5-14_Hispanic_Female"]]
glm.nb(deaths ~ as.numeric(Year) + offset(log(population)), data = White_Female30_34)

# Initialize a list to store models
model_list <- list()

# Iterate over each data frame in the list and fit the model
for (name in names(filtered_data_list)) {
  data_subset <- filtered_data_list[[name]]
  
  # Fit the negative binomial model with error handling
  model <- tryCatch({
    glm.nb(deaths ~ as.numeric(Year) + offset(log(population)), data = data_subset)
  }, error = function(e) {
    message(paste("Error in fitting model for", name, ":", e$message))
    return(NULL)
  })
  
  # Store the model in the list if it was successfully created
  if (!is.null(model)) {
    model_list[[name]] <- model
  }
}

# Print the summary of the first model as an example, if it exists
if (length(model_list) > 0) {
  print(summary(model_list[[6]]))
}

# Initialize a results table
results_table <- data.frame(Age = character(), Race = character(), Gender = character(), Exp_Coefficient_Year = numeric(), CI_Lower = numeric(), CI_Upper = numeric(), stringsAsFactors = FALSE)

# Iterate over each model and exponentiate the coefficient for year
for (name in names(model_list)) {
  model <- model_list[[name]]
  tidy_model <- tidy(model, conf.int = TRUE)
  
  # Check if the year coefficient exists and is not NaN
  if ("as.numeric(Year)" %in% tidy_model$term) {
    # Extract the exponentiated coefficient for year and its confidence interval
    exp_coef <- exp(tidy_model$estimate[tidy_model$term == "as.numeric(Year)"])
    conf_int <- exp(tidy_model[tidy_model$term == "as.numeric(Year)", c("conf.low", "conf.high")])
    
    # Check for NaN values
    if (!is.nan(exp_coef) && !is.nan(conf_int$conf.low) && !is.nan(conf_int$conf.high)) {
      # Extract age, race, and gender from the name
      parts <- strsplit(name, "_")[[1]]
      age <- parts[1]
      race <- parts[2]
      gender <- parts[3]
      
      # Append results to the table
      results_table <- rbind(results_table, data.frame(Age = age, Race = race, Gender = gender, Exp_Coefficient_Year = exp_coef, CI_Lower = conf_int$conf.low, CI_Upper = conf_int$conf.high))
    } else {
      message(paste("NaN values found in model for", name))
    }
  } else {
    message(paste("Year coefficient not found in model for", name))
  }
}

# Print the results table
print(results_table)

##############################################################
# Apply the recoding function to the Cause of Death column
suicide_data <- suicide_data %>%
  mutate(Cause_of_Death_Recoded = sapply(`Cause of Death`, recode_cause_of_death))
race_by_location <- suicide_data %>%
  group_by(Race, Location_Category) %>%
  summarize(Count = n()) %>%
  ungroup()

# If you want it in a wide format (like a contingency table)
race_by_location_wide <- race_by_location %>%
  pivot_wider(names_from = Location_Category, values_from = Count, values_fill = list(Count = 0))

# View the summary table
print(race_by_location_wide)

suicide_data <- suicide_data %>%
  mutate(Other_Significant_Condition_Recoded = sapply(`Other Significant Condition`, recode_other_significant_condition))
race_by_location <- suicide_data %>%
  group_by(Race, Other_Significant_Condition_Recoded) %>%
  summarize(Count = n()) %>%
  ungroup()
race_by_location_wide <- race_by_location %>%
  pivot_wider(names_from = Other_Significant_Condition_Recoded, values_from = Count, values_fill = list(Count = 0))
############################

# Load necessary libraries
library(segmented)
library(MASS)
library(sandwich)
library(lmtest)
library(dplyr)
library(tidyr)
data_rates <- data_rates %>% filter(Gender != "Non-Binary" & Year != 2024)
model_nb <- glm.nb(deaths ~ Year + Race + Gender + Age_Recoded + offset(log(population)), data = data_rates)
robust_se <- vcovHC(model_nb, type = "HC0")
robust_ci <- coeftest(model_nb, vcov = robust_se)

emmeans_results <- emmeans(model_nb, ~ Race * Gender * Age_Recoded, vcov. = robust_se)
summary(emmeans_results)

#data <- read.csv("C:/Users/barboza-salerno.1/Downloads/preprocessed_regression_data.csv")
# Function to apply Davies test for each Race and Age_Recoded combination
# Load necessary libraries

## Function to apply Davies test and return a data frame with the results, including error handling
apply_davies_test <- function(df) {
  # Check if there are enough unique years to fit the model (e.g., at least 3 unique years)
  if (length(unique(df$Year)) < 3) {
    return(data.frame(p_value = NA, warning = "Not enough unique years"))
  }
  
  # Fit a linear model: death_rate ~ Year, with error handling
  model <- tryCatch({
    lm(death_rate ~ Year, data = df)
  }, error = function(e) {
    return(NULL) # Return NULL if the model fails
  })
  
  # If the model failed, return NA for the p-value
  if (is.null(model)) {
    return(data.frame(p_value = NA, warning = "Model fitting failed"))
  }
  
  # Apply Davies test with error handling
  test_result <- tryCatch({
    davies.test(model, seg.Z = ~ Year)
  }, error = function(e) {
    return(NULL) # Return NULL if the Davies test fails
  })
  
  # If the test failed, return NA for the p-value
  if (is.null(test_result)) {
    return(data.frame(p_value = NA, warning = "Davies test failed"))
  }
  
  # Extract the p-value from the test result
  p_value <- test_result$p.value
  
  # Return a data frame with the p-value
  return(data.frame(p_value = p_value, warning = NA))
}
mod1 <- stats::glm(deaths ~ Race + Year + offset(log(population)), 
                   family = poisson(link = "log"), 
                   data = data_rates)
  mod1 <- glm.nb(deaths ~ Race + Year + offset(log(population)), data = data_rates)
  robust_se <- vcovHC(mod1, type = "HC0")
  robust_ci <- coeftest(mod1, vcov = robust_se)
  
  emmeans_results <- emmeans(mod1, ~ Race  , vcov. = robust_se)
  summary(emmeans_results)
  
  results <- data_rates %>%
    group_by(Race ) %>%
    group_modify(~ apply_davies_test(.x))
  
  emmeans_results <- emmeans(mod1, ~  Race  , vcov. = robust_se)
  summary(emmeans_results)
  
  contrast_results <- contrast(emmeans_results, interaction = "pairwise" )
  summary(contrast_results, adjust = "bonferroni")

data_rates$Year <- as.numeric(data_rates$Year)
data_rates$Race <- as.factor(data_rates$Race)
data_rates$Gender <- as.factor(data_rates$Gender)
data_rates$Age_Recoded <- as.factor(data_rates$Age_Recoded)

data_rates$Race <- relevel(data_rates$Race, ref = "White")
data_rates$Gender <- relevel(data_rates$Gender, ref = "Female")
data_rates$Age_Recoded <- relevel(data_rates$Age_Recoded, ref = "≥ 65")

# Fit the initial Poisson regression model
mod1 <- glm(deaths ~   Race*Year*Gender   + offset(log(population)), 
            family = poisson(link = "log"), 
            data = data_rates)

summary(mod1)

# Fit the piecewise regression model
seg_mod <- segmented(mod1, seg.Z = ~Year, psi = list(Year = c(2018)), control = seg.control(quant = TRUE))
# Summary of the segmented model
summary(seg_mod)

# Test the significance of the breakpoints
summary(seg_mod)$psi
fitted_values <- fitted(seg_mod)
breakpoints <- seg_mod$psi

plot_data <- data.frame(
  Year = data_rates$Year,
  Race = data_rates$Race,
  Age = data_rates$Age_Recoded,
  Gender = data_rates$Gender,
  DependentVariable = data_rates$deaths,
  FittedValues = fitted_values
)

# Create the plot with separate panels for each gender
ggplot(plot_data, aes(x = Year, y = DependentVariable, color = Race)) +
  # geom_point(alpha = 0.6, size = 2) +  # Original data points with some transparency and larger size
  geom_smooth(aes(y = FittedValues), method = "glm", se = FALSE, size = 1) +  # Smoothed fitted values
  geom_vline(xintercept = 2022, linetype = "dashed", color = "red") +  # Breakpoints
  labs(title = "Segmented Regression Model Results",
       x = "Year",
       y = "Dependent Variable",
       color = "Race/Ethnicity") +  # Add a legend title
  theme_minimal() +
  facet_wrap(~ Gender) +  # Facet by gender, race, and age
  theme(legend.position = "bottom") +  # Move legend to the bottom
  scale_color_brewer(palette = "Set1")  # Move legend to the bottom

# Fit Poisson regression model
poisson_model <- glm(deaths ~ Year + Race +Gender  + Year:Race:Gender,
                     data = data_rates,
                     family = poisson(link = "log"),
                     offset = log(population))

# Get IRR estimates
irr_values <- exp(coef(poisson_model))
print(irr_values)
summary(poisson_model)

####################################
# Remove "Other/Unknown" category from the data
df_filtered <- suicide_data %>% filter(Location_Category != "Other/Unknown")
df_filtered <- suicide_data[suicide_data$Gender != "Non-Binary", ]

# Create a contingency table of Location_Category by Race
location_race_table <- table(df_filtered$Location_Category, df_filtered$Gender)

# Convert the contingency table to proportions by rows (i.e., proportion of locations within each race)
location_race_prop <- prop.table(location_race_table, margin = 2)

# Convert to a data frame for better readability
location_race_prop_df <- as.data.frame(location_race_prop)

# Rename columns for clarity
colnames(location_race_prop_df) <- c("Location_Category", "Race", "Proportion")

# Display the proportion table
location_race_prop_df

# Create a contingency table of Location_Category by Race (counts)
location_race_counts <- table(df_filtered$Location_Category, df_filtered$Gender)

# Convert to a data frame for better readability
location_race_counts_df <- as.data.frame(location_race_counts)

# Rename columns for clarity
colnames(location_race_counts_df) <- c("Location_Category", "Race", "Count")

# Display the counts table
location_race_counts_df

location_table <- table(df_filtered$Location_Category)
(location_table_with_sums <- addmargins(location_table, margin = 1))

location_table <- table(df_filtered$Other_Significant_Condition_Recoded)
(location_table_with_sums <- addmargins(location_table, margin = 1))

regression_data$rate <- (regression_data$deaths/regression_data$population)*100000

regression_data_aggregated <- regression_data %>%
  filter(Race != "Black") %>%
  group_by(Year, Race, Gender) %>%
  summarise(rate = mean(rate, na.rm = TRUE), .groups = 'drop')

# Fill missing rates with the mean rate for that race/gender group
regression_data_aggregated <- regression_data_aggregated %>%
  group_by(Race, Gender) %>%
  mutate(rate = ifelse(is.na(rate), mean(rate, na.rm = TRUE), rate))

# Create the plot
ggplot(regression_data_aggregated, aes(x = Year, y = rate, color = Gender, shape = Gender)) +
  geom_line() +
  geom_point(size = 3) +
  scale_shape_manual(values = c("Male" = 17, "Female" = 16)) + # Triangles for males, circles for females
  facet_wrap(~ Race, scales = "free_y") +
  labs(title = " ",
       x = "Year",
       y = "Rate") +
  ylim(0,24) +
  theme_minimal() +
    labs(title = " ",
         x = "Year",
         y = "Rate",
         color = "Sex",
         shape = "Sex") +
    theme_minimal() +
    theme(legend.position = c(0.1, 0.9), # Position legend at top left
          legend.box.background = element_rect(color = "black", size = 0.5), # Add box around legend
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.justification = c(0, 1)) 

table_one <- data_recoded %>%
  dplyr::select(Race, Age, Gender, `Manner of Death`) %>%
  tbl_summary(by = `Manner of Death`) %>%
  as_flex_table()
doc <- read_docx() %>% 
  body_add_flextable(., table_one)
print(doc, target = "table_one.docx")