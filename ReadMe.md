# Setting Up Dependencies for the Project

To ensure all collaborators have the required R packages for this project, we use a `requirements.R` script. Follow the steps below to set up the dependencies.

## Steps to Set Up Dependencies

### 1. `requirements.R` Script
The `requirements.R` script lists all the required packages and installs any missing ones. Below is the content of the script:

```r
required_packages <- c("dplyr", "ggplot2", "tidyr", "renv")

for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
    }
}
```

### 2. Run `requirements.R`
Run the `requirements.R` script in your R console to install the required packages. This ensures that you have all the necessary packages to run the project.

```r
source("requirements.R")
```