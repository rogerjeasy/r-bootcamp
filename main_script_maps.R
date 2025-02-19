# Set working directory if needed
setwd("C:/Users/rogej/Documents/hslu/courses/bootcamp/r-bootcamp")

# Get the path to the Scripts_Maps folder
maps_folder <- file.path(getwd(), "Script_Maps")

# Function to safely source scripts with error handling
run_script <- function(script_name) {
  script_path <- file.path(maps_folder, script_name)
  tryCatch({
    message(paste("Running", script_name, "..."))
    source(script_path)
    message(paste("Successfully completed", script_name))
  }, error = function(e) {
    message(paste("Error in", script_name, ":", e$message))
  }, warning = function(w) {
    message(paste("Warning in", script_name, ":", w$message))
  })
}

# Verify the scripts exist
script_files <- list.files(maps_folder, pattern = "\\.R$")
message("Found the following scripts in folder:")
print(script_files)

# Run each script in sequence
run_script("map_municipality_results.R")
run_script("map_canton_results.R")
run_script("demographic_map.R")

message("All mapping scripts have been executed")