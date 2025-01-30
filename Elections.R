library(readxl)
library(dplyr)
library(stringr)

### IMPORTING THE DATASETS

import1 <- read.csv("sd-t-17.02-NRW2023-parteien-appendix.csv",
                   header = TRUE,
                   sep = ";")
import1

import2 <- read_excel("px-x-0102010000_104_20250127-155044.xlsx", 
                      skip = 2)

import4 <- read_excel("px-x-0102020000_201_20250129-134648.xlsx", 
                      skip = 2, 
                      col_names = FALSE)  

import5 <- read_excel("Gemeindestand.xlsx")  


### DATASET 1: Election Results 2023
election2023 <- import1 %>%
  select(gemeinde_bezeichnung,
         gemeinde_nummer,
         partei_staerke,
         letzte_wahl_partei_staerke,
         partei_bezeichnung_de,
         partei_bezeichnung_fr) %>%   
  filter(!is.na(gemeinde_nummer) & gemeinde_nummer != "") %>% 
  mutate(partei_staerke = round(partei_staerke, 0),
         letzte_wahl_partei_staerke = round(letzte_wahl_partei_staerke, 0)) %>%
  rename(partynameDE = partei_bezeichnung_de,  
         partynameFR = partei_bezeichnung_fr,  
         municipality = gemeinde_bezeichnung,  
         municipalityId = gemeinde_nummer) %>%
  mutate(
    CSP = ifelse(partynameDE == "CSP", partei_staerke, 0),
    EDU = ifelse(partynameDE == "EDU", partei_staerke, 0),
    EVP = ifelse(partynameDE == "EVP", partei_staerke, 0),
    FDP = ifelse(partynameDE == "FDP", partei_staerke, 0),
    FGA = ifelse(partynameDE == "FGA", partei_staerke, 0),
    GLP = ifelse(partynameDE == "GLP", partei_staerke, 0),
    GRUENE = ifelse(partynameDE == "GRÜNE", partei_staerke, 0),
    Lega = ifelse(partynameDE == "Lega", partei_staerke, 0),
    LPS = ifelse(partynameDE == "LPS", partei_staerke, 0),
    MCR = ifelse(partynameDE == "MCR", partei_staerke, 0),
    Mitte = ifelse(partynameDE == "Mitte", partei_staerke, 0),
    PdA_Sol = ifelse(partynameDE == "PdA/Sol.", partei_staerke, 0),
    SD = ifelse(partynameDE == "SD", partei_staerke, 0),
    SP = ifelse(partynameDE == "SP", partei_staerke, 0),
    SVP = ifelse(partynameDE == "SVP", partei_staerke, 0),
    Uebrige = ifelse(partynameDE == "Übrige", partei_staerke, 0),
    CSP_19 = ifelse(partynameDE == "CSP", letzte_wahl_partei_staerke, 0),
    EDU_19 = ifelse(partynameDE == "EDU", letzte_wahl_partei_staerke, 0),
    EVP_19 = ifelse(partynameDE == "EVP", letzte_wahl_partei_staerke, 0),
    FDP_19 = ifelse(partynameDE == "FDP", letzte_wahl_partei_staerke, 0),
    FGA_19 = ifelse(partynameDE == "FGA", letzte_wahl_partei_staerke, 0),
    GLP_19 = ifelse(partynameDE == "GLP", letzte_wahl_partei_staerke, 0),
    GRUENE_19 = ifelse(partynameDE == "GRÜNE", letzte_wahl_partei_staerke, 0),
    Lega_19 = ifelse(partynameDE == "Lega", letzte_wahl_partei_staerke, 0),
    LPS_19 = ifelse(partynameDE == "LPS", letzte_wahl_partei_staerke, 0),
    MCR_19 = ifelse(partynameDE == "MCR", letzte_wahl_partei_staerke, 0),
    Mitte_19 = ifelse(partynameDE == "Mitte", letzte_wahl_partei_staerke, 0),
    PdA_Sol_19 = ifelse(partynameDE == "PdA/Sol.", letzte_wahl_partei_staerke, 0),
    SD_19 = ifelse(partynameDE == "SD", letzte_wahl_partei_staerke, 0),
    SP_19 = ifelse(partynameDE == "SP", letzte_wahl_partei_staerke, 0),
    SVP_19 = ifelse(partynameDE == "SVP", letzte_wahl_partei_staerke, 0),
    Uebrige_19 = ifelse(partynameDE == "Übrige", letzte_wahl_partei_staerke, 0)
  ) %>%
  select(-partynameDE , -partynameFR, -partei_staerke, -letzte_wahl_partei_staerke) %>%
  group_by(municipality, municipalityId) %>% 
  dplyr::summarize(across(CSP:Uebrige_19, ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>% 
  arrange(municipalityId) 

election2023 <- election2023 %>%
  mutate(municipalityId = str_pad(as.character(municipalityId), width = 4, side = "left", pad = "0"))

election2023


### DATASET 2: Population Numbers (Swiss vs. Non-Swiss Population)

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


### DATASET 3: Citizenship aquisition

import4 <- import4[, !is.na(colnames(import4))]
import4 <- import4[, 1:5]
column_names <- c(
  "ID", "municipalityId", "citizenship", "sex", "Acquisition_of_Swiss_citizenship"
)

colnames(import4) <- column_names

citizenship <- import4 %>%
  select(-ID, -citizenship, -sex) %>%
  filter(grepl("^\\.\\.\\.\\.\\.\\.", municipalityId)) %>%
  mutate(
    municipalityId = substr(municipalityId, 7, 10)
  )

print(citizenship)


### DATASET 4: Overview of Municipality, Distric, Canton for improved matching

municipaldata <- import5 %>%
  select("Kanton",	"Bezirks-nummer",	"Bezirksname",	"BFS Gde-nummer",	"Gemeindename") %>%   
  rename(districtId = "Bezirks-nummer")    %>%  
  rename(districtName = "Bezirksname")    %>%  
  rename(municipality = "Gemeindename")    %>%  
  rename(municipalityId = "BFS Gde-nummer") 

municipaldata <- municipaldata %>%
  mutate(municipalityId = str_pad(as.character(municipalityId), width = 4, side = "left", pad = "0"))

### JOINING THE DATASETS

combined_data <- election2023 %>%
  left_join(swisspop %>% select(municipalityId, municipalityId, population, swiss_population, non_swiss_population, percentage_non_swiss_pop), by = "municipalityId") %>%
  left_join(citizenship %>% select(municipalityId, Acquisition_of_Swiss_citizenship), by = "municipalityId") %>%
  left_join(municipaldata %>% select(municipalityId, Kanton, districtId, districtName), by = "municipalityId")

combined_data

## SAVE COMBINED DATA IN DATATABLE AS CSV

if (file.exists("datatable.csv")) {
  file.remove("datatable.csv")
}
write.csv(combined_data, "datatable.csv", row.names = FALSE)



