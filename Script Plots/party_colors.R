# Creating a dataframe with party names and their corresponding colors
party_colors <- data.frame(
  Party = c("SVP", "SP", "FDP", "Mitte", "GRUENE", "GLP",
            "EVP", "Lega", "CSP", "PdA_Sol", "MCR", "Sol", "EDU",
            "CSP", "AL-ZH", "AL-BE"),
  Color = c("#008000", "#BB0000", "#00529F", "#E39D2B", "#83AD20", "#C4D600",
            "#FFD700", "#4876FF", "#8B0000", "#FF4500", "#DAA520", "#DC143C",
            "#C71585", "#008B8B", "#FF1493", "#800000"),
  stringsAsFactors = FALSE
)

print(party_colors)

# Save as a CSV file
write.csv(party_colors, "party_colors.csv", row.names = FALSE)