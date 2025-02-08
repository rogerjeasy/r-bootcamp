library(readxl)
library(dplyr)
library(readr)
library(stringr)

### IMPORTING THE DATASETS

import_data <- function(file_path, skip = 0, col_names = TRUE, sep = ";") {
  if (grepl("\\.csv$", file_path)) {
    return(read.csv(file_path, header = TRUE, sep = sep))
  } else if (grepl("\\.xlsx$", file_path)) {
    return(read_excel(file_path, skip = skip, col_names = col_names))
  } else {
    stop("Unsupported file format: ", file_path)
  }
}
 files <- list(
  list("Data/sd-t-17.02-NRW2023-parteien-appendix.csv", 0, TRUE, ";"),
  list("Data/px-x-0102010000_104_20250127-155044.xlsx", 2, TRUE),
  list("Data/su-e-40.02.15.08.05-2022.xlsx", 4, TRUE),
  list("Data/px-x-0102020000_201_20250129-134648.xlsx", 2, FALSE),
  list("Data/su-d-01.02.03.06.xlsx", 5, FALSE),
  list("Data/27600_131.xlsx", 6, FALSE),
  list("Data/Gemeindestand.xlsx", 0, TRUE)
)

imported_data <- lapply(files, function(f) import_data(f[[1]], f[[2]], f[[3]]))
names(imported_data) <- paste0("import", 1:length(files))

### DATASET 1: Election Results 2023
election2023 <- import1 %>%
  dplyr::select(gemeinde_bezeichnung,
         gemeinde_nummer,
         partei_staerke,
         letzte_wahl_partei_staerke,
         partei_bezeichnung_de,
         partei_bezeichnung_fr) %>%   
  filter(!is.na(gemeinde_nummer) & gemeinde_nummer != "" & nchar(gemeinde_nummer) < 5) %>% 
  mutate(partei_staerke = round(partei_staerke, 0),
         letzte_wahl_partei_staerke = round(letzte_wahl_partei_staerke, 0)) %>%
  rename(partynameDE = partei_bezeichnung_de,  
         partynameFR = partei_bezeichnung_fr,  
         municipality = gemeinde_bezeichnung,  
         municipalityId = gemeinde_nummer) %>%
  mutate(
    CSP_23 = ifelse(partynameDE == "CSP", partei_staerke, 0),
    EDU_23 = ifelse(partynameDE == "EDU", partei_staerke, 0),
    EVP_23 = ifelse(partynameDE == "EVP", partei_staerke, 0),
    FDP_23 = ifelse(partynameDE == "FDP", partei_staerke, 0),
    FGA_23 = ifelse(partynameDE == "FGA", partei_staerke, 0),
    GLP_23 = ifelse(partynameDE == "GLP", partei_staerke, 0),
    GRUENE_23 = ifelse(partynameDE == "GRÜNE", partei_staerke, 0),
    Lega_23 = ifelse(partynameDE == "Lega", partei_staerke, 0),
    LPS_23 = ifelse(partynameDE == "LPS", partei_staerke, 0),
    MCR_23 = ifelse(partynameDE == "MCR", partei_staerke, 0),
    Mitte_23 = ifelse(partynameDE == "Mitte", partei_staerke, 0),
    PdA_Sol_23 = ifelse(partynameDE == "PdA/Sol.", partei_staerke, 0),
    SD_23 = ifelse(partynameDE == "SD", partei_staerke, 0),
    SP_23 = ifelse(partynameDE == "SP", partei_staerke, 0),
    SVP_23 = ifelse(partynameDE == "SVP", partei_staerke, 0),
    Uebrige_23 = ifelse(partynameDE == "Übrige", partei_staerke, 0),
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
  dplyr::select(-partynameDE , -partynameFR, -partei_staerke, -letzte_wahl_partei_staerke) %>%
  group_by(municipality, municipalityId) %>% 
  dplyr::summarize(across(CSP_23:Uebrige_19, ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>% 
  arrange(municipalityId) 

election2023 <- election2023 %>%
  mutate(municipalityId = str_pad(as.character(municipalityId), width = 4, side = "left", pad = "0"))

election2023


### DATASET 2: Population Numbers (Swiss vs. Non-Swiss Population)

swisspop <- import2 %>%
  dplyr::select(c = 3, i = 9, j = 10) 

swisspop <- swisspop %>%
  mutate(row_group = rep(1:(n() / 2), each = 2, length.out = n())) %>%  
  group_by(row_group) %>%
  summarise(
    municipalityId = first(c),
    population = sum(as.numeric(i), na.rm = TRUE), 
    swisspop_num = paste(j, collapse = " ")  
  ) %>%
  ungroup() %>%
  dplyr::select(-row_group) %>%
  mutate(swisspop_num = sapply(strsplit(swisspop_num, " "), function(x) {
    nums <- gsub("[^0-9.]", "", x)
    first_num <- nums[nums != ""][1]
    as.numeric(first_num)
  })) %>%
  filter(nchar(municipalityId) == 4 & municipalityId != "8001") %>%
  mutate(
    nswisspop_num = population - swisspop_num,
    nswisspop_pct = round((nswisspop_num / population) * 100, 2)
  )
swisspop

### DATASET 3: Education 


education <- import3 %>%
  dplyr::select(districtId = 1, Kanton = 2, districtName = 3, edupop_num = 4, edulow_num = 6, edusec_num = 8, eduter_num = 10)

education <- education %>%
  mutate(
    edupop_num = round(as.numeric(edupop_num)),
    edulow_num = round(as.numeric(edulow_num)),
    edusec_num = round(as.numeric(edusec_num)),
    eduter_num = round(as.numeric(eduter_num))
  ) %>%
  mutate(
    edulow_pct = round((edulow_num / edupop_num) * 100, 0),
    edusec_pct = round((edusec_num / edupop_num) * 100, 0),
    eduter_pct = round((eduter_num / edupop_num) * 100, 0)
  )

# edulow = low education (compulsory school)
# edusec = secondary education (professional college)
# eduter = tertiary education (unviersity education)

### DATASET 4: Citizenship aquisition

import4 <- import4[, !is.na(colnames(import4))]
import4 <- import4[, 1:5]
column_names <- c(
  "ID", "municipalityId", "citizenship", "sex", "naturalization_num"
)

colnames(import4) <- column_names

citizenship <- import4 %>%
  dplyr::select(-ID, -citizenship, -sex) %>%
  filter(grepl("^\\.\\.\\.\\.\\.\\.", municipalityId)) %>%
  mutate(
    municipalityId = substr(municipalityId, 7, 10)
  )

print(citizenship)

### DATASET 5: Age distribution
# agequota_pct: statistical measurement for aged population in relation to middle age (20-65) population

age <- import5 %>%
  rename(municipality_info = 1) %>% # Rename column1 to a temporary name
  mutate(
    municipalityId = substr(municipality_info, 7, 10) # Extract only municipality ID
  ) %>%
  dplyr::select(
    municipalityId, 
    agepop_num = 2, 
    everything() # Keep all other columns
  ) %>%
  mutate(
    age2065_num = rowSums(dplyr::select(., 23:68), na.rm = TRUE),
    age66plus_num = rowSums(dplyr::select(., 69:103), na.rm = TRUE)
  ) %>%
  mutate(
    agequota_pct = round((age66plus_num / age2065_num) * 100, 2) # Percentage calculation
  ) %>%
  dplyr::select(municipalityId, agepop_num, age2065_num, age66plus_num, agequota_pct)  # Keep only relevant columns

age

### DATASET 6: some income or wealth metric 

income <- import6[, 1:4]
column_names_inc <- c("municipalityId", "municipalityName", "incomePerCommune", "incomePerCapita")
colnames(income) <- column_names_inc

income <- income %>%
  mutate(municipalityId = str_pad(as.character(municipalityId), width = 4, side = "left", pad = "0"))

income <- income %>%
  mutate(incomePerCapita = round(as.numeric(incomePerCapita))) %>%
  dplyr::select(municipalityId, incomePerCapita)

income

### DATASET 7: Overview of Municipality, Distric, Canton for improved matching

municipaldata <- import7 %>%
  dplyr::select("Kanton",	"Bezirks-nummer",	"Bezirksname",	"BFS Gde-nummer",	"Gemeindename") %>%   
  rename(districtId = "Bezirks-nummer")    %>%  
  rename(districtName = "Bezirksname")    %>%  
  rename(municipality = "Gemeindename")    %>%  
  rename(municipalityId = "BFS Gde-nummer") 

municipaldata <- municipaldata %>%
  mutate(municipalityId = str_pad(as.character(municipalityId), width = 4, side = "left", pad = "0"))
municipaldata

### JOINING THE DATASETS

combined_data <- election2023 %>%
  left_join(swisspop %>% dplyr::select(municipalityId, municipalityId, population, swisspop_num, nswisspop_num, nswisspop_pct), by = "municipalityId") %>%
  left_join(citizenship %>% dplyr::select(municipalityId, naturalization_num), by = "municipalityId") %>%
  left_join(municipaldata %>% dplyr::select(municipalityId, Kanton, districtId, districtName), by = "municipalityId") %>%
  left_join(education %>% dplyr::select(districtId, edupop_num, edulow_num, edusec_num, eduter_num, edulow_pct, edusec_pct, eduter_pct), by = "districtId") %>%
  left_join(age %>% dplyr::select(municipalityId, agepop_num, age2065_num, age66plus_num, agequota_pct), by = "municipalityId") %>%
  left_join(income %>% dplyr::select(municipalityId, incomePerCapita), by = "municipalityId") 


combined_data <- combined_data %>%
  filter(nchar(municipalityId) == 4,
         !is.na(districtId),  # Remove rows where districtId is NA
         districtId != "" 
         )


## SAVE COMBINED DATA IN DATATABLE AS CSV

if (file.exists("Data/datatable.csv")) {
  file.remove("Data/datatable.csv")
}
write.csv(combined_data, "Data/datatable.csv", row.names = FALSE)

