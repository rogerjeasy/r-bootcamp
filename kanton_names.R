canton_symbols <- tibble(
  KTNAME = c(
    "Zürich", "Bern / Berne", "Luzern", "Uri", "Schwyz", "Obwalden",
    "Nidwalden", "Glarus", "Zug", "Fribourg / Freiburg", "Solothurn",
    "Basel-Stadt", "Basel-Landschaft", "Schaffhausen", "Appenzell Ausserrhoden",
    "Appenzell Innerrhoden", "St. Gallen", "Graubünden / Grigioni / Grischun",
    "Aargau", "Thurgau", "Ticino", "Vaud", "Valais / Wallis", "Neuchâtel",
    "Genève", "Jura"
  ),
  KTN_SYMB = c(
    "ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR", "SO",
    "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD",
    "VS", "NE", "GE", "JU"
  )
)
kanton_name_with_symb <- data.frame(canton_symbols)
write.csv(kanton_name_with_symb, "Data/kanton_names.csv", row.names = FALSE)
