library(readxl)
library(dplyr)
library(stringr)

import1 <- read.csv("sd-t-17.02-NRW2023-parteien-appendix.csv",
                   header = TRUE,
                   sep = ";")


import2 <- read_excel("px-x-0102010000_104_20250127-155044.xlsx", skip = 2)

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
  select(-row_group)  

swisspop <- swisspop %>%
  filter(nchar(municipalityId) == 4 & municipalityId != "8001")  

print(swisspop)



#head(import)

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

combined_data <- election2023 %>%
  left_join(swisspop, by = "municipalityId")

combined_data


