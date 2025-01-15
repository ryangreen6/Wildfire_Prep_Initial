##################################
#####Targeting in Time############
##################################

rm(list=ls())
library(sf)
library(tidyverse)


#Census variables
#B03003_003 total hispanic
#B01003_001 total pop
#B19127_001 income
#B19013_001 household income

# Specify the variables you want to download
variables <- c("B01003_001", "B03003_003", "B19127_001", "B19013_001")

# Download data for census tracts in a specific state (e.g., New York)
sb_census <- get_acs(geography = "tract", 
                     variables = variables, 
                     state = "CA", 
                     county = "Santa Barbara County",
                     geometry = TRUE)

sb_census2 <- pivot_wider(sb_census, names_from = "variable", values_from = "estimate") %>% group_by(GEOID, NAME) %>%
  summarise(tot_pop = sum(B01003_001, na.rm = T),
            hispanic_pop = sum(B03003_003, na.rm = T),
            income = sum(B19127_001, na.rm = T)) %>%
  mutate(pct_hispanic = hispanic_pop / tot_pop*100,
         income_pc = income / tot_pop,
         hispanic = ifelse(pct_hispanic > 30, "Hispanic", "Non-Hispanic"),
         hispanic2 = ifelse(pct_hispanic > 30, 1, 0),
         wealthy = ifelse(income_pc > 63850, "High", "Non-High"),
         wealthy2 = ifelse(income_pc > 63850,1, 0))

sb_county_fire2019 <- st_read("2019_defensible_space_inspections_SBC/2019_defensible_space_inspections_SBC.shp") %>%
  mutate(compliant = ifelse(status %in% c("1st Compliant","2nd Compliant","3rd Compliant"), 1, 0),
         compliant2 = ifelse(status %in% c("1st Compliant","2nd Compliant","3rd Compliant"), "Compliant", "Non-Compliant"),
         uninspected = ifelse(status %in% c("Incomplete Data or Never Inspected", "Uninspected (Locked Gate)"), 1, 0),
         uninspected2 = ifelse(status %in% c("Incomplete Data or Never Inspected", "Uninspected (Locked Gate)"), "Uninspected", "Inspected"),
         deadtrees3 = as.numeric(deadtrees3),
         deadtreesbin = ifelse(deadtrees3 > 0, 1, 0),
         wood_porche = ifelse(deckporche == "Wood", 1, 0),
         water_storage = ifelse(is.na(water_stor), 0, 1),
         combustible_fence = ifelse(fenceattac == "Combustible", 1, 0),
         nofence = ifelse(fenceattac == "No Fence", 1, 0),
         roofconstr_concrete = ifelse(roofconstr == "Asphalt", 1, 0),
         roofconstr_metal = ifelse(roofconstr == "Metal", 1, 0)) %>%
  dplyr::select(compliant, compliant2, status, latitude, longitude, address_fu) %>%
  mutate(year = 2019)

sb_county_fire2020 <- st_read("2020_defensible_space_inspections_SBC.shp") %>%
  mutate(compliant = ifelse(status == "Compliant", 1, 0),
         compliant2 = ifelse(status == "Compliant", "Compliant", "Non-Compliant")) %>%
  dplyr::select(compliant, compliant2, status, latitude, longitude, address_fu) %>%
  mutate(year = 2020)

sb_county_fire2021 <- st_read("2021_defensible_space_inspections_SBC.shp") %>%
  mutate(compliant = ifelse(status == "Compliant", 1, 0),
         compliant2 = ifelse(status == "Compliant", "Compliant", "Non-Compliant")) %>%
  dplyr::select(compliant, compliant2, status, latitude, longitude, address_fu) %>%
  mutate(year = 2021)

sb_county_fire2022 <- st_read("2022_defensible_space_inspections_SBC.shp") %>%
  mutate(compliant = ifelse(status == "Compliant", 1, 0),
         compliant2 = ifelse(status == "Compliant", "Compliant", "Non-Compliant")) %>%
  dplyr::select(compliant, compliant2, status, latitude, longitude, address_fu) %>%
  mutate(year = 2022)

sb_county_fire2023 <- st_read("defensible_space_inspections_2023.gdb")
sb_county_fire2023 <- st_as_sf(sb_county_fire2023, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(compliant = ifelse(status == "Compliant", 1, 0),
         compliant2 = ifelse(status == "Compliant", "Compliant", "Non-Compliant")) %>%
  dplyr::select(compliant, compliant2, status, latitude, longitude, address_full) %>%
  rename(address_fu = address_full) %>%
  mutate(year = 2023)

sb_county_fire_2019_2024 <- rbind(sb_county_fire2019, sb_county_fire2020, sb_county_fire2021,
                                  sb_county_fire2022, sb_county_fire2023)

crs_polygons <- st_crs(sb_county_fire_2019_2024)
sb_census2 <- st_transform(sb_census2, crs = crs_polygons)

dta <- sb_county_fire_2019_2024 %>%
  st_drop_geometry() %>%
  group_by(address_fu) %>%
  summarise(tot = n())

#Spatial join

sb_county_fire_2019_2024b <- sb_county_fire_2019_2024 %>%
  st_join(sb_census2) 

sb_county_fire_2019b <- sb_county_fire2019 %>%
  st_join(sb_census2) 

# Split the dataset into training and testing sets
set.seed(345)  # For reproducibility
train_indices <- sample(1:nrow(sb_county_fire2019b), 0.5 * nrow(sb_county_fire2019b))  # 70% for training
train_data <- sb_county_fire2019b[train_indices, ]
test_data <- sb_county_fire2019b[-train_indices, ]

# Train a logistic regression model using the training data
model <- glm(compliant ~ income_pc+yearbuilt+occupantho+
               combustible_fence+roofconstr_concrete, data = train_data, family = binomial)
modelb <- lm(compliant ~ pct_hispanic+income_pc+yearbuilt+occupantho+wood_porche, data = train_data)

# Make predictions on the test data
predictions <- predict(model, newdata = test_data, type = "response")

# Convert predicted probabilities to class labels (0 or 1)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Create a confusion matrix
conf_matrix <- table(test_data$compliant, predicted_classes)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

#Distribution of places

#By home features
ggplot() +
  geom_sf(data = sb_census, 
          fill = "white",
          linewidth = 0.1) +
  theme_linedraw() +
  geom_sf(data = subset(sb_county_fire_2019_2024b, compliant == 0 & wealthy=="Non-Wealthy" & year == 2019),
          size = 0.25,
          color = "lightblue1") +
  geom_sf(data = subset(sb_county_fire_2019_2024b, compliant == 0 & wealthy=="Non-Wealthy" & year == 2020),
          size = 0.25,
          color = "deepskyblue1") +
  geom_sf(data = subset(sb_county_fire_2019_2024b, compliant == 0 & wealthy=="Non-Wealthy" & year == 2021),
          size = 0.25,
          color = "dodgerblue3") +
  geom_sf(data = subset(sb_county_fire_2019_2024b, compliant == 0 & wealthy=="Non-Wealthy" & year == 2022),
          size = 0.25,
          color = "dodgerblue4") +
  geom_sf(data = subset(sb_county_fire_2019_2024b, compliant == 0 & wealthy=="Non-Wealthy" & year == 2022),
          size = 0.25,
          color = "navyblue") +
  geom_sf(data = subset(sb_county_fire_2019_2024b, compliant == 0 & wealthy=="Wealthy"),
          size = 0.15,
          color = "grey") + 
  ylim(34.36,35.1) + xlim(-120.62,-119.4)

dta1 <- sb_county_fire_2019b %>%
  dplyr::select(wealthy, compliant2) %>%
  na.omit()
ggplot(dta1, aes(wealthy, ..count..)) + geom_bar(aes(fill = compliant2), position = "dodge") + xlab("Income") + theme_classic()

sb_county_fire_2019_2024c <- sb_county_fire_2019_2024b %>%
  na.omit()

dta2_tot <- sb_county_fire_2019_2024c %>%
  na.omit() %>%
  st_drop_geometry() %>%
  group_by(wealthy, year) %>%
  summarise(tot = n())

dta2_part <- sb_county_fire_2019_2024c %>%
  na.omit() %>%
  st_drop_geometry() %>%
  group_by(wealthy, year, compliant2) %>%
  summarise(status = n()) %>%
  left_join(dta2_tot, by = c("year", "wealthy")) %>%
  mutate(pct = status / tot*100)

dta3_tot <- sb_county_fire_2019_2024c %>%
  na.omit() %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(tot = n())

dta3_part <- sb_county_fire_2019_2024c %>%
  na.omit() %>%
  st_drop_geometry() %>%
  group_by(year, compliant2) %>%
  summarise(status = n()) %>%
  left_join(dta3_tot, by = c("year")) %>%
  mutate(pct = status / tot*100)

ggplot(subset(dta2_part, compliant2 == "Non-Compliant"), aes(x = as.factor(year), y = pct, fill = wealthy)) +
  geom_bar(stat = "identity", position = "dodge") + theme_classic() + scale_fill_brewer(palette = "Set1") +
  xlab("Year") + ylab("Percent Non-Compliance") + labs(fill = "Income")

ggplot(subset(dta3_part, compliant2 == "Non-Compliant"), aes(x = as.factor(year), y = pct)) +
  geom_bar(stat = "identity", position = "dodge") + theme_classic() +
  xlab("Year") + ylab("Percent Non-Compliance") 



#Distribution of Non-Compliant

#By home features
ggplot() +
  geom_sf(data = sb_census, 
          linewidth = 0.1) +
  theme_linedraw() + 
  geom_sf(data = subset(sb_county_fire2019b, !status %in% c("Uninspected (Locked Gate)","Incomplete Data or Never Inspected",
                                                            "1st Non Compliant","Structure Destroyed","3rd Non Compliant",
                                                            "2nd Non Compliant") & wealthy=="Wealthy"), 
          size = 0.2,
          color = "blue") +
  geom_sf(data = subset(sb_county_fire2019b, status %in% c("Uninspected (Locked Gate)","Incomplete Data or Never Inspected",
                                                            "1st Non Compliant","Structure Destroyed","3rd Non Compliant",
                                                            "2nd Non Compliant") & wealthy=="Wealthy"), 
          size = 0.2,
          color = "red") +
  ylim(34.36,35.1) + xlim(-120.62,-119.4)

#By home features
ggplot() +
  geom_sf(data = sb_census, 
          linewidth = 0.1) +
  theme_linedraw() + 
  geom_sf(data = subset(sb_county_fire2019b, !status %in% c("Uninspected (Locked Gate)","Incomplete Data or Never Inspected",
                                                           "1st Non Compliant","Structure Destroyed","3rd Non Compliant",
                                                           "2nd Non Compliant") & wealthy=="Wealthy"), 
          size = 0.2,
          color = "blue") +
  ylim(34.36,35.1) + xlim(-120.62,-119.4)

ggplot() +
  geom_sf(data = sb_census, 
          linewidth = 0.1) +
  theme_linedraw() + 
  geom_sf(data = subset(sb_county_fire2019b, status %in% c("Uninspected (Locked Gate)","Incomplete Data or Never Inspected",
                                                            "1st Non Compliant","Structure Destroyed","3rd Non Compliant",
                                                            "2nd Non Compliant") & nofence==1), 
          size = 0.2,
          color = "red") +
  ylim(34.36,35.1) + xlim(-120.62,-119.4)




ggplot() +
  geom_sf(data = sb_census, 
          linewidth = 0.1) +
  theme_linedraw() + 
  geom_sf(data = subset(sb_county_fire2019b, !status %in% c("Uninspected (Locked Gate)","Incomplete Data or Never Inspected",
                                                           "1st Non Compliant","Structure Destroyed","3rd Non Compliant",
                                                           "2nd Non Compliant") & combustible_fence==1), 
          size = 0.2,
          color = "blue") +
  geom_sf(data = subset(sb_county_fire2019, status %in% c("Uninspected (Locked Gate)","Incomplete Data or Never Inspected",
                                                          "1st Non Compliant","Structure Destroyed","3rd Non Compliant",
                                                          "2nd Non Compliant")), 
          size = 0.2,
          color = "red") +
  ylim(34.36,35.1) + xlim(-120.62,-119.4)




ggplot() +
  geom_sf(data = sb_census, 
          linewidth = 0.1) +
  theme_linedraw() + 
  geom_sf(data = subset(sb_county_fire2019, !status %in% c("Uninspected (Locked Gate)","Incomplete Data or Never Inspected",
                                                       "1st Non Compliant","Structure Destroyed","3rd Non Compliant",
                                                       "2nd Non Compliant")), 
          size = 0.2,
          color = "blue") +
  geom_sf(data = subset(sb_county_fire2019, status %in% c("Uninspected (Locked Gate)","Incomplete Data or Never Inspected",
                                                      "1st Non Compliant","Structure Destroyed","3rd Non Compliant",
                                                      "2nd Non Compliant")), 
          size = 0.2,
          color = "red") +
  ylim(34.36,35.1) + xlim(-120.62,-119.4)

ggplot() +
  geom_sf(data = sb_census, 
          linewidth = 0.1) +
  theme_linedraw() + geom_sf(data = subset(sb_county_fire2019, status %in% c("Uninspected (Locked Gate)","Incomplete Data or Never Inspected",
                                                          "1st Non Compliant","Structure Destroyed","3rd Non Compliant",
                                                          "2nd Non Compliant")), 
          size = 0.2,
          color = "red") +
  ylim(34.36,35.1) + xlim(-120.62,-119.4)

ggplot() +
  geom_sf(data = santa_barbara, 
          linewidth = 0.1) +
  theme_linedraw() + 
  geom_sf(data = subset(sb_county_fire, status %in% c("Uninspected (Locked Gate)","Incomplete Data or Never Inspected",
                                                      "1st Non Compliant","Structure Destroyed","3rd Non Compliant",
                                                      "2nd Non Compliant")), 
          size = 0.2,
          color = "red") +
  ylim(34.36,35.1) + xlim(-120.62,-119.4)

#Timing of the inspections

sb_dates <- sb_county_fire %>%
  st_drop_geometry() %>%
  mutate(date = inspectiondate_calculate) %>%
  filter(date > "2023-01-01") %>%
  group_by(date) %>%
  summarise(tot = n()) %>%
  mutate(date = as.Date(date))

ggplot(data = sb_dates, aes(x = date, y = tot, group = 1)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme_classic()

dta1 <- sb_county_fire2019b %>%
  dplyr::select(wealthy, uninspected2) %>%
  na.omit()

dta2 <- sb_county_fire2019b %>%
  dplyr::select(hispanic, uninspected2) %>%
  na.omit()

dta3 <- sb_county_fire2019b %>%
  dplyr::select(combustible_fence, uninspected2) %>%
  na.omit()

dta4 <- sb_county_fire2019b %>%
  dplyr::select(roofconstr_concrete, uninspected2) %>%
  na.omit()

dta5 <- sb_county_fire2019b %>%
  dplyr::select(nofence, uninspected2) %>%
  na.omit()

ggplot(dta1, aes(wealthy, ..count..)) + geom_bar(aes(fill = uninspected2), position = "dodge") + xlab("Income") + theme_classic()
ggplot(dta2, aes(hispanic, ..count..)) + geom_bar(aes(fill = uninspected2), position = "dodge") + xlab("Hispanic") + theme_classic()
ggplot(dta3, aes(as.factor(combustible_fence), ..count..)) + geom_bar(aes(fill = uninspected2), position = "dodge") + xlab("Combustible Fence") + theme_classic()
ggplot(dta4, aes(as.factor(roofconstr_concrete), ..count..)) + geom_bar(aes(fill = uninspected2), position = "dodge") + xlab("Concrete Roof") + theme_classic()
ggplot(dta5, aes(as.factor(nofence), ..count..)) + geom_bar(aes(fill = uninspected2), position = "dodge") + xlab("No Fence") + theme_classic()

frequency_table <- table(dta1$wealthy, dta1$compliant2)