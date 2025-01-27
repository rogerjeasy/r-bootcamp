import <- read.csv("sd-t-17.02-NRW2023-parteien-appendix.csv",
                   header = TRUE,
                   sep = ";")
#head(import)

election2023 <- import %>%
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

election2023