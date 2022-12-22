## Data Science 1 with R (STAT 301-1) ####
# Final Project: Data Collection
# Includes Data Extraction, Collection & Cleaning

##### Preparations

# Data Citation: 
# U.S. Census Bureau, 2019, “2015-2019 American Community Survey (ACS) 5-year Public Use Microdata Samples” [SAS Data file], api.census.gov

# Getting Variables Required for Extraction
variables <- load_variables(2019, "acs5", cache = TRUE)
View(variables)

# Labeling Categories of Interest
races <- c("White", "Black/African American", "American Indian/Alaska Native", "Asian", "Native Hawaiian/Pacific Islander", "Other", "Two or More Races", "Hispanic/Latino")genders <- c("Male", "Female")
transport <- c("Drive", "Carpool", "Public Transport", "Bicycle", "Walk", "Work at Home")
jobs <- c("STEM", "Service", "Sales", "Construction", "Transportation", "Private", "Self-Employed", "Government")

Northeast <- c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA")
Midwest <- c("OH", "MI", "IN", "WI", "IL", "MN", "IA", "MO", "ND", "SD", "NE", "KS")
South <- c("DE", "MD", "VA", "WV", "KY", "NC", "SC", "TN", "GA", "FL", "AL", "MS", "AR", "LA", "TX", "OK")
West <- c("MT", "ID", "WY", "CO", "NM", "AZ", "UT", "NV", "CA", "OR", "WA", "AK", "HI")





##### Race Data

# Table of Count of Each Race in Each State
race <-
  get_acs(geography = "state", 
          variables = c("B03002_003", "B03002_004", "B03002_005", "B03002_006", "B03002_007", "B03002_008", "B03002_009", "B03002_012"),
          year = 2019) %>%
  select(-moe, -GEOID) %>%                                              # removes unnecessary data, will not use margin of error & will call using state not GEOID
  pivot_wider(names_from = variable, values_from = estimate) %>%        # allows for organization by State and gives separate columns for each selected variable
  rename("State" = "NAME",
         "White" = "B03002_003",
         "Black/African American" = "B03002_004",
         "American Indian/Alaska Native" = "B03002_005",
         "Asian" = "B03002_006",
         "Native Hawaiian/Pacific Islander" = "B03002_007",
         "Other" = "B03002_008",
         "Two or More Races" = "B03002_009",
         "Hispanic/Latino" = "B03002_012")                              # renames the variables with the corresponding race
race <- race[-c(9, 52), ]                                               # removes District of Columbia and Puerto Rico, only studying states
race


# Extracting State Names & Abbreviations
states_full <- race %>% pull(State)                                     # isolates list of states
states_abb <- state.abb[match(states_full,state.name)]                  # creates list of abbreviations of states, used for renaming (easier to read data) and calling
states_tib <- tibble(State = states_abb)                                # isolates abbreviated state tibble, used for mutating 


# Table of Proportion of Each Race in Each State
race_prop <-
  data.matrix(race[2:9], rownames.force = NA) %>%                       # need to convert to matrix to use prop.table() function, excludes State column to avoid computing issues
  prop.table(1) %>%
  round(digits = 4) %>%                                                 # avoids unnecessary amount of decimal places
  as_tibble(check.names = FALSE) %>%
  mutate(State = states_abb, .before = 1)                               # adds back state column
race_prop




##### Gender Data

# Table of Count of Each Gender in Each State
gender <-
  get_acs(geography = "state", 
          variables = c("B01001_002", "B01001_026"),
          year = 2019) %>%
  select(-moe, -GEOID) %>%                                              # removes unnecessary data, will not use margin of error & will call using state not GEOID
  pivot_wider(names_from = variable, values_from = estimate) %>%        # allows for organization by State and gives separate columns for each selected variable
  rename("State" = "NAME",
         "Male" = "B01001_002",
         "Female" = "B01001_026")                                       # renames the variables with the corresponding gender
gender <- gender[-c(9, 52), ]                                           # removes District of Columbia and Puerto Rico, only studying states
gender


# Table of Proportion of Each Gender in Each State
gender_prop <-
  data.matrix(gender[2:3], rownames.force = NA) %>%                     # need to convert to matrix to use prop.table() function, excludes State column to avoid computing issues
  prop.table(1) %>%
  round(digits = 4) %>%                                                 # avoids unnecessary amount of decimal places
  as_tibble(check.names = FALSE) %>%
  mutate(State = states_abb, .before = 1)                               # adds back state column
gender_prop





##### Transportation Data

# Table of Count of Each Form of Transportation in Each State
transportation <-
  get_acs(geography = "state", 
          variables = c("B08006_003", "B08006_004", "B08006_008", "B08006_014", "B08006_015", "B08006_017"),
          year = 2019) %>%
  select(-moe, -GEOID) %>%                                              # removes unnecessary data, will not use margin of error & will call using state not GEOID
  pivot_wider(names_from = variable, values_from = estimate) %>%        # allows for organization by State and gives separate columns for each selected variable
  rename("State" = "NAME",
         "Drive" = "B08006_003",
         "Carpool" = "B08006_004",
         "Public Transport" = "B08006_008",
         "Bicycle" = "B08006_014",
         "Walk" ="B08006_015",
         "Work at Home" = "B08006_017")                                 # renames the variables with the corresponding form of transportation used to get to work
transportation <- transportation[-c(9, 52), ]                           # removes District of Columbia and Puerto Rico, only studying states


# Table of Proportion of Each Form of Transportation in Each State
transportation_prop <-
  data.matrix(transportation[2:7], rownames.force = NA) %>%             # need to convert to matrix to use prop.table() function, excludes State column to avoid computing issues
  prop.table(1) %>%
  round(digits = 4) %>%                                                 # avoids unnecessary amount of decimal places
  as_tibble(check.names = FALSE) %>%
  mutate(State = states_abb, .before = 1)                               # adds back state column
transportation_prop





##### Occupation Data

# Table of Count of Each Occupation in Each State
occupation <-
  get_acs(geography = "state", 
          variables = c("C24060_002", "C24060_003", "C24060_004", "C24060_005", "C24060_006", "C24060_007", "C24060_013", "C24060_025"),
          year = 2019) %>%
  select(-moe, -GEOID) %>%                                              # removes unnecessary data, will not use margin of error & will call using state not GEOID
  pivot_wider(names_from = variable, values_from = estimate) %>%        # allows for organization by State and gives separate columns for each selected variable
  rename("State" = "NAME",
         "STEM" = "C24060_002",
         "Service" = "C24060_003",
         "Sales" = "C24060_004",
         "Construction" = "C24060_005",
         "Transportation" = "C24060_006",
         "Private" = "C24060_007",
         "Self-Employed" = "C24060_013",
         "Government" = "C24060_025")                                   # renames the variables with the corresponding occupation
occupation <- occupation[-c(9, 52), ]                                   # removes District of Columbia and Puerto Rico, only studying states
occupation


# Table of Proportion of Each Occupation in Each State
occupation_prop <-
  data.matrix(occupation[2:9], rownames.force = NA) %>%                 # need to convert to matrix to use prop.table() function, excludes State column to avoid computing issues
  prop.table(1) %>%
  round(digits = 4) %>%                                                 # avoids unnecessary amount of decimal places
  as_tibble(check.names = FALSE) %>%
  mutate(State = states_abb, .before = 1)                               # adds back state column
occupation_prop





##### Age & Income Data
age_income <-
  get_acs(geography = "state", 
          variables = c("B01002_001", "B06011_001"),
          year = 2019) %>%
  select(-moe, -GEOID) %>%                                              # removes unnecessary data, will not use margin of error & will call using state not GEOID
  pivot_wider(names_from = variable, values_from = estimate) %>%        # allows for organization by State and gives separate columns for each selected variable
  rename("State" = "NAME",
         "Median Age" = "B01002_001",
         "Median Income" = "B06011_001")                                # renames the variables with the corresponding age/income
age_income <- age_income[-c(9, 52), ] %>%                               # removes District of Columbia and Puerto Rico, only studying states
  mutate(states_tib)                                                    # replaces full state names with abbreviations, allows to join with other data
age_income





##### Merging

# Data Set of Proportion of the Races, Genders, Forms of Transportation, and Occupations in Each State
dataframe_prop_1 <- left_join(race_prop, gender_prop)                   # merges first set of proportion tibbles
dataframe_prop_2 <- left_join(transportation_prop, occupation_prop)     # merges second set of proportion tibbles
dataframe_prop_3 <- left_join(dataframe_prop_1, dataframe_prop_2)       # merges both sets of proportion tibbles
dataframe_prop_perc <- mutate(dataframe_prop_3[2:25] * 100) %>%
  round(digits = 4) %>%
  mutate(states_tib, .before = 1)                                       # displays proportions as a percentage rather than a decimal, mutation excludes state column to avoid error then readds it
dataframe_prop_final <- left_join(age_income, dataframe_prop_perc)      # merges median age & income tibble with proportion tibbles
dataframe_prop_final <- data.frame(dataframe_prop_final, check.names = FALSE)     # converts tibble to data frame, maintains names of columns
dataframe_prop_final                                                              # organized in a way that allows for selective extraction of age, income, race, gender, transportation, and occupation data
write_csv(dataframe_prop_final, "Mustafa_Sammy_FinalProject/processed_data/census_proportions.csv")    # only need processed_data folder as creating my own data set avoids unprocessed data 

