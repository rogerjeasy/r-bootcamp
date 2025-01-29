library(readxl)
library(dplyr)
library(stringr)

### IMPORTING THE DATASETS

import1 <- read.csv("sd-t-17.02-NRW2023-parteien-appendix.csv",
                   header = TRUE,
                   sep = ";")

import2 <- read_excel("px-x-0102010000_104_20250127-155044.xlsx", 
                      skip = 2)

import3 <- read_excel("px-x-0102020000_201_20250129-134648.xlsx", 
                      skip = 2, 
                      col_names = FALSE)  

import4 <- read_excel("Gemeindestand.xlsx")  

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
swisspop

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

election2023 <- election2023 %>%
  mutate(municipalityId = str_pad(as.character(municipalityId), width = 4, side = "left", pad = "0"))


### DATASET 3: Citizenship aquisition

import3 <- import3[, !is.na(colnames(import3))]
import3 <- import3[, 1:5]
column_names <- c(
  "ID", "municipalityId", "citizenship", "sex", "Acquisition_of_Swiss_citizenship"
)

colnames(import3) <- column_names

citizenship <- import3 %>%
  select(-ID, -citizenship, -sex) %>%
  filter(grepl("^\\.\\.\\.\\.\\.\\.", municipalityId)) %>%
  mutate(
    municipalityId = substr(municipalityId, 7, 10)
  )

print(citizenship)


### DATASET 4: Overview of Municiaplity, Distric, Canton for improved matching

municipaldata <- import4 %>%
  select("Kanton",	"Bezirks-nummer",	"Bezirksname",	"BFS Gde-nummer",	"Gemeindename") %>%   
  rename(districtId = "Bezirks-nummer")    %>%  
  rename(districtName = "Bezirksname")    %>%  
  rename(municipality = "Gemeindename")    %>%  
  rename(municipalityId = "BFS Gde-nummer") 

municipaldata <- municipaldata %>%
  mutate(municipalityId = str_pad(as.character(municipalityId), width = 4, side = "left", pad = "0"))

municipaldata

### JOINING THE DATASETS

combined_data <- election2023 %>%
  left_join(swisspop %>% select(municipalityId, municipalityId, population, swiss_population, non_swiss_population, percentage_non_swiss_pop), by = "municipalityId") %>%
  left_join(citizenship %>% select(municipalityId, Acquisition_of_Swiss_citizenship), by = "municipalityId") %>%
  left_join(municipaldata %>% select(municipalityId, districtId, districtName), by = "municipalityId")

combined_data