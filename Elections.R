library(readxl)
library(dplyr)
library(stringr)

### IMPORTING THE DATASETS

import1 <- read.csv("sd-t-17.02-NRW2023-parteien-appendix.csv",
                   header = TRUE,
                   sep = ";")

import2 <- read_excel("px-x-0102010000_104_20250127-155044.xlsx", 
                      skip = 2)

import3 <- read_excel("px-x-0102020000_201_20250129-105355.xlsx", 
                      skip = 2, 
                      col_names = FALSE)  

### DATASET 1: Population Numbers (Swiss vs. Non-Swiss Population)

swisspop <- import2 %>%
  select(c = 3, i = 9, j = 10) 

swisspop <- swisspop %>%
  mutate(row_group = rep(1:(n() / 2), each = 2, length.out = n())) %>%  
  group_by(row_group) %>%
  summarise(
    municipalityId = first(c),
    population = sum(as.numeric(i), na.rm = TRUE), 
    swiss_population = paste(j, collapse = " ")  
  ) %>%
  ungroup() %>%
  select(-row_group) %>%
  mutate(swiss_population = sapply(strsplit(swiss_population, " "), function(x) {
    nums <- gsub("[^0-9.]", "", x)
    first_num <- nums[nums != ""][1]
    as.numeric(first_num)
  })) %>%
  filter(nchar(municipalityId) == 4 & municipalityId != "8001") %>%
  mutate(
    non_swiss_population = population - swiss_population,
    percentage_non_swiss_pop = round((non_swiss_population / population) * 100, 2)
)


### DATASET 2: Election Results 2023

election2023 <- import1 %>%
  select(gemeinde_bezeichnung,
         gemeinde_nummer,
         partei_staerke,
         partei_bezeichnung_de,
         partei_bezeichnung_fr) %>%   
  filter(!is.na(gemeinde_nummer) & gemeinde_nummer != "") %>% 
  filter(partei_bezeichnung_de == "SVP") %>%               
  mutate(partei_staerke = round(partei_staerke, 0)) %>% 
  rename(partynameDE = partei_bezeichnung_de)    %>%  
  rename(partynameFR = partei_bezeichnung_fr)    %>%  
  rename(municipality = gemeinde_bezeichnung)    %>%  
  rename(municipalityId = gemeinde_nummer)    %>%  
  rename(SVP_result = partei_staerke) 

### DATASET 3: Demographic Data (Age, Birth, Citizenship aquisition etc.)
# We adjust the names of the columns

column_names <- c(
  "ID", "municipalityId",  "citizenship",  "sex",  "Population_on_1_January",  "Live_birth",  "Death",  "Natural_change",  "Immigration_incl_change_of_population_type",
  "In_migration_from_another_canton",  "In_migration_from_same_canton",  "Emigration",  "Out_migration_to_another_canton",  "Out_migration_to_same_canton",      
  "Net_migration_incl_change_of_population_type",  "Change_of_population_type",  "Acquisition_of_Swiss_citizenship",  "Gender_change_in_civil_register_entry",    
  "Gender_change_in_civil_register_exit",  "Statistical_adjustment",  "Population_on_31_December",  "Population_change"
)

colnames(import3) <- column_names
demography <- import3 %>%
  select(-ID, -citizenship, -sex) %>%
  filter(grepl("^\\.\\.\\.\\.\\.\\.", municipalityId)) %>%
  mutate(
    municipalityId = substr(municipalityId, 7, 10)
  )
print(demography)

### JOINING THE DATASETS

election2023 <- election2023 %>%
  mutate(municipalityId = str_pad(as.character(municipalityId), width = 4, side = "left", pad = "0"))

combined_data <- election2023 %>%
  left_join(swisspop, by = "municipalityId")

combined_data <- election2023 %>%
  left_join(demography, by = "municipalityId")

combined_data


