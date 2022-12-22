## Data Science 1 with R (STAT 301-1) ####
# Final Project: Initial EDA



# Returning Data Back to Decimals
decimal <- 
  select(dataframe_prop_final, State) %>%
  mutate(dataframe_prop_final[4:27] / 100) 
data <- left_join(age_income, decimal)
data




#### AGE & INCOME

# Age
age_data <- select(data, c(State, "Median Age")) %>%
  rename("Age" = "Median Age")
age_data

age_graph <-
  ggplot(age_data) +
  geom_col(aes(x = State, y = Age))
age_graph

# Income
income_data <- select(data, c(State, "Median Income")) %>%
  rename("Income" = "Median Income")

income_graph <-
  ggplot(income_data) +
  geom_col(aes(x = State, y = Income))
income_graph





#### RACE
race_data <- select(data, c(State, races))
race_data_1 <- pivot_longer(race_data, races, names_to = "Race", values_to = "Proportion")        # Creates proportion column for each state and race, used for values in pivot_wider
race_data_2 <- pivot_wider(race_data_1, names_from = State, values_from = Proportion)             # Creates columns for each state, allows for data to be called by region
race_data_2

# Histogram of the Proportion of Each Race
race_dis <-
  ggplot(race_data_1) +                                                                           # Studying distribution from all states, do not need to call specific US region so do not need pivot_wider data
  geom_histogram(aes(x = Proportion), binwidth = 0.05) +
  facet_wrap(~Race)                                                                               # Displays general distribution/range of proportions of each race
race_dis

# Bar Graphs of Proportion of Each Race in Each Region/the US
race_graph_NE <-
  select(race_data_2, Race, Northeast) %>%
  pivot_longer(Northeast, names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_col(aes(fill = Race, x = State, y = Proportion), position = "fill")                        # A percent stacked column chart allows for better analysis of state racial diversity
race_graph_NE                                                                                     # NJ is the most racially diverse in the Northeast

race_graph_MW <-
  select(race_data_2, Race, Midwest) %>%
  pivot_longer(Midwest, names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_col(aes(fill = Race, x = State, y = Proportion), position = "fill")
race_graph_MW                                                                                     # IL is the most racially diverse in the Midwest

race_graph_S <-
  select(race_data_2, Race, South) %>%
  pivot_longer(South, names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_col(aes(fill = Race, x = State, y = Proportion), position = "fill")
race_graph_S                                                                                      # TX is the most racially diverse in the South

race_graph_W <-
  select(race_data_2, Race, West) %>%
  pivot_longer(West, names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_col(aes(fill = Race, x = State, y = Proportion), position = "fill")
race_graph_W                                                                                      # HI is the most racially diverse in the West 

race_graph_US <-
  select(race_data_2, Race, NJ, IL, TX, HI) %>%
  pivot_longer(c(NJ, IL, TX, HI), names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_col(aes(fill = Race, x = State, y = Proportion), position = "fill")
race_graph_US                                                                                     # HI is the most racially diverse in the US





#### GENDER
gender_data <- select(data, c(State, genders))
gender_data_1 <- pivot_longer(gender_data, genders, names_to = "Gender", values_to = "Proportion")    # Creates proportion column for each state and race, used for values in pivot_wider
gender_data_2 <- pivot_wider(gender_data_1, names_from = State, values_from = Proportion)             # Creates columns for each state, allows for data to be called by region
gender_data_2

# Histogram of the Proportion of Each Gender
gender_dis <-
  ggplot(gender_data_1) +                                                                             # Studying distribution from all states, do not need to call specific US region so do not need pivot_wider data
  geom_histogram(aes(x = Proportion), binwidth = 0.001) +
  facet_wrap(~Gender)                                                                                 # Displays general distribution/range of proportions of each gender
gender_dis

# 2D Heatmaps of Proportion of Each Gender in Each Region/the US
gender_graph_NE <-
  select(gender_data_2, Gender, Northeast) %>%
  pivot_longer(Northeast, names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_tile(aes(fill = Proportion, x = Gender, y = State))                                            # Utilize geom_tile as the differences are very minimal, using colors allows for the easier identification of differences
gender_graph_NE                                                                                       # MA has the most unequal gender balance in the Northeast

gender_graph_MW <-
  select(gender_data_2, Gender, Midwest) %>%
  pivot_longer(Midwest, names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_tile(aes(fill = Proportion, x = Gender, y = State))
gender_graph_MW                                                                                      # ND has the most unequal gender balance in the Midwest

gender_graph_S <-
  select(gender_data_2, Gender, South) %>%
  pivot_longer(South, names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_tile(aes(fill = Proportion, x = Gender, y = State))
gender_graph_S                                                                                      # AL has the most unequal gender balance in the South

gender_graph_W <-
  select(gender_data_2, Gender, West) %>%
  pivot_longer(West, names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_tile(aes(fill = Proportion, x = Gender, y = State))
gender_graph_W                                                                                      # AK has the most unequal gender balance in the West

gender_graph_US <-
  select(gender_data_2, Gender, MA, ND, AL, AK) %>%
  pivot_longer(c(MA, ND, AL, AK), names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_tile(aes(fill = Proportion, x = State, y = Gender))
gender_graph_US                                                                                     # AK has the most unequal gender balance in the US 





#### TRANSPORTATION
transport_data <- select(data, c(State, transport))
transport_data_1 <- pivot_longer(transport_data, transport, names_to = "Transportation", values_to = "Proportion")      # Creates proportion column for each state and race, used for values in pivot_wider
transport_data_2 <- pivot_wider(transport_data_1, names_from = State, values_from = Proportion)                         # Creates columns for each state, allows for data to be called by region
transport_data_2

# Histogram of the Proportion of Each Form of Transportation Used to Get to Work
transport_dis <-
  ggplot(transport_data_1) +                                                                                            # Studying distribution from all states, do not need to call specific US region so do not need pivot_wider data
  geom_histogram(aes(x = Proportion), binwidth = 0.05) +
  facet_wrap(~Transportation)                                                                                           # Displays general distribution/range of proportions of each form of transportation
transport_dis

# 2D Heatmaps of Proportion of Each Form of Transportation Used to Get to Work in Each Region/the US
transport_graph_NE <-
  select(transport_data_2, Transportation, Northeast) %>%
  pivot_longer(Northeast, names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_tile(aes(fill = Proportion, x = Transportation, y = State))                                                      # Utilize geom_tile as the differences are very minimal, using colors allows for the easier identification of differences
transport_graph_NE                                                                                                      # NY has the most diverse transportation usage in the Northeast

transport_graph_MW <-
  select(transport_data_2, Transportation, Midwest) %>%
  pivot_longer(Midwest, names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_tile(aes(fill = Proportion, x = Transportation, y = State))
transport_graph_MW                                                                                                      # IL has the most diverse transportation usage in the Midwest

transport_graph_S <-
  select(transport_data_2, Transportation, South) %>%
  pivot_longer(South, names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_tile(aes(fill = Proportion, x = Transportation, y = State))
transport_graph_S                                                                                                       # MD has the most diverse transportation usage in the South

transport_graph_W <-
  select(transport_data_2, Transportation, West) %>%
  pivot_longer(West, names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_tile(aes(fill = Proportion, x = Transportation, y = State))
transport_graph_W                                                                                                       # HI has the most diverse transportation usage in the South

transport_graph_US <-
  select(transport_data_2, Transportation, NY, IL, MD, HI) %>%
  pivot_longer(c(NY, IL, MD, HI), names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_tile(aes(fill = Proportion, x = State, y = Transportation))
transport_graph_US                                                                                                      # NY has the most diverse transportation usage in the US





#### OCCUPATION
occupation_data <- select(data, c(State, jobs))
occupation_data_1 <- pivot_longer(occupation_data, jobs, names_to = "Occupation", values_to = "Proportion")       # Creates proportion column for each state and race, used for values in pivot_wider
occupation_data_2 <- pivot_wider(occupation_data_1, names_from = State, values_from = Proportion)                 # Creates columns for each state, allows for data to be called by region
occupation_data_2

# Histogram of the Proportion of Each Occupation
occupation_dis <-
  ggplot(occupation_data_1) +                                                                                     # Studying distribution from all states, do not need to call specific US region so do not need pivot_wider data
  geom_histogram(aes(x = Proportion), binwidth = 0.05) +
  facet_wrap(~Occupation)                                                                                         # Displays general distribution/range of proportions of each occupation
occupation_dis

# 2D Heatmaps of Proportion of Each Occupation in Each Region/the US
occupation_graph_NE <-
  select(occupation_data_2, Occupation, Northeast) %>%
  pivot_longer(Northeast, names_to = "State", values_to = "Proportion") %>%                                       # Repeat to isolate proportions for each occupation for each state of the selected US region
  group_by(State) %>%
  ggplot() +
  geom_tile(aes(fill = Proportion, x = Occupation, y = State))                                                    # Utilize geom_tile as the differences are very minimal, using colors allows for the easier identification of differences
occupation_graph_NE                                                                                               # VT has the most diverse range of occupations in the Northeast

occupation_graph_MW <-
  select(occupation_data_2, Occupation, Midwest) %>%
  pivot_longer(Midwest, names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_tile(aes(fill = Proportion, x = Occupation, y = State))
occupation_graph_MW                                                                                               # SD has the most diverse range of occupations in the Midwest

occupation_graph_S <-
  select(occupation_data_2, Occupation, South) %>%
  pivot_longer(South, names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_tile(aes(fill = Proportion, x = Occupation, y = State))
occupation_graph_S                                                                                                # MD has the most diverse range of occupations in the South

occupation_graph_W <-
  select(occupation_data_2, Occupation, West) %>%
  pivot_longer(West, names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_tile(aes(fill = Proportion, x = Occupation, y = State))
occupation_graph_W                                                                                                # AK has the most diverse range of occupations in the West

occupation_graph_US <-
  select(occupation_data_2, Occupation, VT, SD, MD, AK) %>%
  pivot_longer(c(VT, SD, MD, AK), names_to = "State", values_to = "Proportion") %>%
  group_by(State) %>%
  ggplot() +
  geom_tile(aes(fill = Proportion, x = State, y = Occupation))
occupation_graph_US                                                                                              # AK has the most diverse range of occupations in the US


