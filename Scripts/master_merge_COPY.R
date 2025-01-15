rm(list = ls())

library(tidyverse)
library(sf)
library(here)



defsp22 <-  st_read(here('data', 'inspections_data', '2022_defensible_space_inspections_SBC', '2022_defensible_space_inspections_SBC.shp')) %>% 
  janitor::clean_names() %>% 
  mutate(year = 2022)
defsp21 <- st_read(here('data', 'inspections_data', '2021_defensible_space_inspections_SBC', '2021_defensible_space_inspections_SBC.shp')) %>% 
  janitor::clean_names() %>% 
  mutate(year = 2021)
defsp20 <- st_read(here('data', 'inspections_data', '2020_defensible_space_inspections_SBC', '2020_defensible_space_inspections_SBC.shp')) %>% 
  janitor::clean_names() %>% 
  mutate(year = 2020)
defsp19 <- st_read(here('data', 'inspections_data', '2019_defensible_space_inspections_SBC', '2019_defensible_space_inspections_SBC.shp')) %>% 
  janitor::clean_names() %>% 
  mutate(year = 2019)
defsp18 <- st_read(here('data', 'inspections_data', '2018_defensible_space_inspections_SBC', '20181231_defensible_space_inspections_SBC.shp')) %>% 
  janitor::clean_names() %>% 
  mutate(year = 2018)

defsp23 <- st_read("data/inspections_data/2023_defensible_space_inspections_SBC.gdb") %>%
  janitor::clean_names()

#changing variable names to align with previous years
defsp23 <- defsp23 %>%
  mutate(system_cre = as.character(system_created_at),
         address_su = address_sub_thoroughfare,
         address_th = address_thoroughfare,
         address_lo = address_locality,
         address_1 = address_suite,
         address_2 = address_sub_admin_area,
         address_po = address_postal_code,
         address_fu = address_full,
         year = 2023)

#loading in new address data, it looks like there are three new files to use
defsp24_addresses <- st_read("data/parcel_data/Defensible_Space_2024_address_points.gdb") %>% 
  janitor::clean_names()
#defsp24_addresses looks contains multiple duplicates for some addresses, looks like an incomplete inspections dataset
#for repeated addresses, coordinates vary, will assume that this dataset is more like previous defsp datasets
#for this reason, i will use the other two new datasets for merging instead

#just addresses, useful because it also separates addresses into chunks
readysetgo <- read_csv("data/parcel_data/2024_ReadySetGo_DSP_Address_List_20240415.csv") %>% 
  janitor::clean_names() %>% 
  distinct(full_address, .keep_all = TRUE)

#just the straight address with coordinate data
latlong <- read_csv("data/parcel_data/2024_Defensible_Space_LatLong_Addr.csv") %>% 
  janitor::clean_names() %>% 
  filter(!is.na(latitude) & !is.na(longitude)
         & !is.na(address)) %>% 
  distinct(address, .keep_all = TRUE)


#looking at dupes in datasets
defsp24_addresses %>% 
  count(address) %>% 
  filter(n > 1)
#checking for dupes in master lists
readysetgo %>% 
  count(full_address) %>% 
  filter(n > 1)


#i want to begin with a merge test using left join
#first will test readysetgo and latlong by merging them
readysetgo2 <- readysetgo %>%
  mutate(full_address1 = tolower(full_address)) %>%
  mutate(rsg_address = full_address1 %>%
           str_replace_all("us highway", "highway") %>%
           str_replace_all("california", "ca") %>%
           str_replace_all("north ", "n ") %>%
           str_replace_all("south ", "s ") %>%
           str_replace_all("east ", "e ") %>%
           str_replace_all("west ", "w ") %>%
           str_replace_all("highway", "hwy") %>%
           str_replace_all(" n hwy", " hwy") %>%
           str_replace_all(" s hwy", " hwy") %>%
           str_replace_all(" e hwy", " hwy") %>%
           str_replace_all(" w hwy", " hwy") %>%
           str_replace_all("lane", " ln ") %>%
           str_replace_all("road", " rd ") %>%
           str_replace_all("drive", "dr") %>%
           str_replace_all("street", "st") %>%
           str_replace_all("avenue", "ave") %>%
           str_replace_all("court", "ct") %>%
           str_replace_all("circle", "cir") %>%
           str_replace_all("boulevard", "blvd") %>%
           str_replace_all("place", "pl") %>%
           str_replace_all("parkway", "pkwy") %>%
           str_replace_all("trail", "trl") %>%
           str_replace_all("way", "wy") %>%
           str_replace_all("point", "pt") %>%
           str_replace_all("junction", "jct") %>%
           str_replace_all("center", "ctr") %>%
           str_replace_all(" us", "") %>%
           gsub("\\.", "", .) %>%
           str_trim()) %>% 
  distinct(rsg_address, .keep_all = TRUE)


latlong2 <- latlong %>%
  mutate(address1 = tolower(address)) %>%
  mutate(latlong_address = address1 %>%
           str_replace_all("us highway", "highway") %>%
           str_replace_all("california", "ca") %>%
           str_replace_all("north ", "n ") %>%
           str_replace_all("south ", "s ") %>%
           str_replace_all("east ", "e ") %>%
           str_replace_all("west ", "w ") %>%
           str_replace_all("highway", "hwy") %>%
           str_replace_all(" n hwy", " hwy") %>%
           str_replace_all(" s hwy", " hwy") %>%
           str_replace_all(" e hwy", " hwy") %>%
           str_replace_all(" w hwy", " hwy") %>%
           str_replace_all("lane", " ln ") %>%
           str_replace_all("road", " rd ") %>%
           str_replace_all("drive", "dr") %>%
           str_replace_all("street", "st") %>%
           str_replace_all("avenue", "ave") %>%
           str_replace_all("court", "ct") %>%
           str_replace_all("circle", "cir") %>%
           str_replace_all("boulevard", "blvd") %>%
           str_replace_all("place", "pl") %>%
           str_replace_all("parkway", "pkwy") %>%
           str_replace_all("trail", "trl") %>%
           str_replace_all("way", "wy") %>%
           str_replace_all("point", "pt") %>%
           str_replace_all("junction", "jct") %>%
           str_replace_all("center", "ctr") %>%
           str_replace_all(" us", "") %>%
           str_replace_all("california", "ca") %>%
           gsub("\\.", "", .) %>%
           str_trim()) %>% 
  distinct(latlong_address, .keep_all = TRUE)


#with previous cleaning integrated, lets get merging
masterlist3 <- left_join(latlong2, readysetgo2, by = c("latlong_address" = "rsg_address"))
#adding a full join for the sake of testing missing addresses for the defsp list
masterlist4 <- full_join(readysetgo2, latlong2, by = c("rsg_address" = "latlong_address"))

#there are 118 sets of coordinates that do not merge with the readysetgo2 address list
masterlist4 %>%
  filter(is.na(latitude)) %>%
  nrow()


#now i am bringing in the defsp data
#NOTE FOR FUTURE: I am using the system_cre variable as my working date variable but
#I am not sure if that is the best date to be using in practice
#Left it in since I don't really need it here
#I recommend consulting the data dictionary the fire dept provided to better understand date variables in future
for(i in seq(18, 23)){
  tmp <- get(paste0("defsp", i)) %>% 
    select(fulcrum_id, system_cre, year, status, address_su, address_th, address_lo,
           address_po, address_1, address_2, address_fu, geometry) 
  assign(paste0("defsp", i, "_clean"), tmp)
}

defsp18_23 <- bind_rows(defsp18_clean, defsp19_clean, defsp20_clean,defsp21_clean,defsp22_clean, defsp23_clean)

#pretty much the same changes to masterlist are applied to inspections addresses
defsp18_23mainaddy <- defsp18_23 %>% 
  mutate(address_fu = tolower(address_fu),
         address_2 = tolower(address_2)) %>%
  rowwise() %>%
  mutate(defsp_address = str_replace(address_fu, fixed(paste0(address_2, " ")), "")) %>%
  ungroup() %>% 
  mutate(defsp_address = defsp_address %>%
           str_replace_all("us highway", "highway") %>%
           str_replace_all("california", "ca") %>%
           str_replace_all("north ", "n ") %>%
           str_replace_all("south ", "s ") %>%
           str_replace_all("east ", "e ") %>%
           str_replace_all("west ", "w ") %>%
           str_replace_all("highway", "hwy") %>%
           str_replace_all(" n hwy", " hwy") %>%
           str_replace_all(" s hwy", " hwy") %>%
           str_replace_all(" e hwy", " hwy") %>%
           str_replace_all(" w hwy", " hwy") %>%
           str_replace_all("lane", " ln ") %>%
           str_replace_all("road", " rd ") %>%
           str_replace_all("drive", "dr") %>%
           str_replace_all("street", "st") %>%
           str_replace_all("avenue", "ave") %>%
           str_replace_all("court", "ct") %>%
           str_replace_all("circle", "cir") %>%
           str_replace_all("boulevard", "blvd") %>%
           str_replace_all("place", "pl") %>%
           str_replace_all("parkway", "pkwy") %>%
           str_replace_all("trail", "trl") %>%
           str_replace_all("way", "wy") %>%
           str_replace_all("point", "pt") %>%
           str_replace_all("junction", "jct") %>%
           str_replace_all("center", "ctr") %>%
           str_replace_all(" us", "") %>%
           str_replace_all("california", "ca") %>%
           gsub("\\.", "", .) %>%
           str_trim()) %>% 
  mutate(status_num = case_when(  ###1 for noncomp, 0 for other###
    status == "Incomplete Data or Never Inspected" ~ "0",
    status == "Due for 2019 Inspection" ~ "0",
    status == "Not Inspected since 2017" ~ "0",
    status == "1st Compliant" ~ "0",
    status == "Uninspected (Locked Gate)" ~ "0",
    status == "2nd Non Compliant" ~ "1",
    status == "2nd Compliant" ~ "1",
    status == "1st Non Compliant" ~ "1",
    status == "Structure Destroyed" ~ "0",
    status == "3rd Non Compliant" ~ "1",
    status == "3rd Compliant" ~ "1",
    status == "Compliant" ~ "0",
    status == "Due for Inspection 2020" ~ "0",
    status == "Citation" ~ "1",
    status == "Due for Inspection 2021" ~ "0",
    status == "Due for Inspection" ~ "0",
    status == "Compliant 6 months - 1 year" ~ "0",
    status == "AB-38 Request (please complete within 5 days)" ~ "0" #marking this as uninspected
  ))


#merges the inspections data with masterlist data
main_merge1 <- left_join(defsp18_23mainaddy, masterlist3, by = c("defsp_address" = "latlong_address"))


#creating a merged dataset with a complete address list
#will use addresses from readysetgo when available and latlong when NA
#so it will be NA if inspections did not merge with masterlist
main_merge2 <- main_merge1 %>%
  mutate(working_address = ifelse(is.na(full_address), address, full_address),
         working_address_num = ifelse(is.na(working_address), 0, 1)) #1 if merged and 0 if not merged
  


#### testing for shared and missing zip codes ####
test1 <- defsp18_23mainaddy %>%
  group_by(address_po) %>%
  summarise(props = n(), .groups = "drop") %>%
  mutate(zip = as.numeric(address_po))

test2 <- readysetgo2 %>% 
  group_by(zip) %>% 
  summarise(props = n(), .groups = "drop") %>% 
  mutate(zip1 = as.numeric(zip))


test <- full_join(test1, test2, by = c("zip" = "zip"))
#test provides a rough count of each zip code and its frequency
#nearly all of NA values for address_po are from 2019 and are from the 93108 zip code

#### end of section ####

#when merged with readysetgo2 i have 9390 missing full addresses
#instead, when merged with latlong2 8305 missing full addresses

#### checking the state of the datasets ####
main_merge2 %>%
  filter(is.na(working_address)) %>%
  nrow()

main_merge2 %>%
  filter(is.na(defsp_address)) %>%
  nrow()

latlong2 %>%
  filter(is.na(latlong_address)) %>%
  nrow()

masterlist3 %>%
  filter(is.na(full_address)) %>%
  nrow()
#turns out there are 318 sets of coordinates that did not merge with the readysetgo2 list
#i figure we need a variable that is the most complete address possible, so I created working_address and main_merge2

masterlist3 %>%
  distinct(zip)

main_merge1 %>%
  filter(is.na(address)) %>%
  select(defsp_address, latitude)

#### end of section ####


#the rest of the code below is creating and mapping the unmerged data
missing_merge <- main_merge1 %>%
  filter(is.na(address)) %>%
  select(defsp_address, year, status_num, geometry)


missing_noncomp <- missing_merge %>% 
  filter(status_num == "1")
missing_noncomp_sum <- missing_noncomp %>% 
  group_by(defsp_address) %>%
  summarise(num_missing = n(), .groups = "drop")

#when merged with latlong2, there are 99 missing noncompliant inspections across 91 addresses
#now i have a list of all the noncompliant addresses
#i want to summarize the data by defsp_address and keep the geometry variable for each value

missing_merge_sum <- missing_merge %>% 
  group_by(defsp_address) %>%
  summarise(num_missing = n(), .groups = "drop") %>% 
  filter(num_missing < 60) #this is to exclude NAs grouped together


#now for making the maps
library(tidycensus)
library(viridis)

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

#total unmerged inspections
ggplot() +
  geom_sf(data = sb_census, 
          fill = "white",
          linewidth = 0.1) +
  theme_linedraw() +
  geom_sf(data = missing_merge_sum, aes(color = num_missing), size = .5) +
  scale_color_gradient(low = "blue", high = "red", name = "Frequency of \nSame Location") +  
  ylim(34.36, 35.1) + xlim(-120.62, -119.4) +
  labs(title = "Unmerged Inspections")  # Add title

#unmerged noncompliant inspections
ggplot() +
  geom_sf(data = sb_census, 
          fill = "white",
          linewidth = 0.1) +
  theme_linedraw() +
  geom_sf(data = missing_noncomp_sum, aes(color = num_missing), size = .5) +
  scale_color_gradient(low = "blue", high = "red", name = "Noncompliant\nFrequency") +  
  ylim(34.36, 35.1) + xlim(-120.62, -119.4) +
  labs(title = "Unmerged Noncompliant Inspections")  # Add title





library(tmap)

tm_shape(sb_census2) +
    tm_polygons(fill = 'white')

