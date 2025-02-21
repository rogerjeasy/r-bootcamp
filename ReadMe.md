# Mapping Demographic Composition as a Predictor of Far-Right Success in the 2023 Swiss Federal Election

For detailed project documentation, code examples, and in-depth analysis, please visit our [📚 Documentation](https://github.com/rogerjeasy/r-bootcamp/tree/main/Documentation). This comprehensive guide includes detailed explanations of methodologies, interactive visualizations, and complete analysis results.

## Table of Contents
- [Overview](#overview)
- [Setup](#setup)
- [Data Sources](#data-sources)
- [Project Structure](#project-structure)
- [Features](#features)
- [Usage](#usage)
- [Contributing](#contributing)
- [Contact](#contact)
- [License](#license)

## Overview

This research project investigates the relationship between regional demographic composition and voting behavior in the context of the 2023 Swiss federal election, with a particular focus on mapping using interactive visualization techniques.

### Research Objectives
- Analyze demographic predictors of far-right electoral success
- Map geographic distribution of voting patterns
- Explore correlations between demographic factors and party support

## Setup

### Dependencies

1. Create and run `requirements.R`:

```R
required_packages <- c(
    "leaflet",     
    "sf",           
    "dplyr",        
    "ggplot2",     
    "tidyr",        
    "raster",       
    "magrittr",     
    "htmltools",    
    "plotly",       
    "readr"         
)

# Install missing packages
for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
    }
}
```

2. Run the setup:

```R
source("requirements.R")
```

## Data Sources

Data from Swiss Federal Office of Statistics (BFS):

| Dataset | Filename | Description |
|---------|----------|-------------|
| Election Results | `sd-t-17.02-NRW2023-parteien-appendix.csv` | 2023 Federal election results |
| Citizenship | `px-x-0102010000_104_20250127-155044.xlsx` | Population demographics |
| Education | `su-e-40.02.15.08.05-2022.xlsx` | Educational attainment |
| Age Distribution | `su-d-01.02.03.06.xlsx` | Population age structure |

## Project Structure

The project follows this structure:
```
r-bootcamp/
├── 📂 Data/
│   ├── datatable.csv
│   ├── party_colors.csv
│   └── kanton_names.csv
│   └── ...
├── 📂 Shapefiles/
│   ├── g2k23.shp
│   ├── g2s23.shp
│   └── g2l23.shp
├── 📂 Script Plots/
│   ├── stat_models.R
│   ├── boxplots.R
│   └── scatterplots_barplots.R
├── 📂 Script_Maps/
│   ├── visualizations_box_plot.R
│   ├── demographic_map.R
│   ├── map_canton_results.R
│   └── map_municipality_results.R
│    └── ...
│
└── 📂 Documentation/
│    └── 📂 Plots/
│    │    ├── boxplot_cantons.rds
│    │    ├── plot_SVPvsPopSize.rds
│    │    └── regr_scatter.rds
│    │    └── ...
│    └── Documentation.Rmd
│    └── Documentation.html
│    └── style.css
│    └── ...
└── requirements.R
└── main_script_maps.R
└── Elections.R
└── README.md

```

## Features

### 1. Electoral Analysis Capabilities

- **Regional Analysis**
  - Canton-level electoral patterns
  - Municipality-level voting behavior
  - Urban-rural voting divisions
  - Language region comparisons

- **Party Performance Tracking**
  - Vote share visualization
  - Party stronghold identification
  - Electoral trend analysis 2019-2023
  - Regional party dominance mapping

- **Demographic Correlation Analysis**
  - Education level impact
  - Income distribution effects
  - Age demographic influences
  - Migration pattern correlations

### 2. Interactive Maps

The project implements multiple mapping layers:

1. Geographic Visualization:
   - Municipality boundaries (2,113 municipalities)
   - Canton boundaries
   - Lakes and geographical features
   - Country outline
   - Population density representations

2. Interactive Elements:
   - Hover tooltips with detailed statistics
   - Color-coded regions by party dominance
   - Zoom/pan capabilities
   - Pop-up information windows
   - Dynamic data filtering

### 3. Statistical Analysis Tools

- **Demographic Indicators**
  - Non-Swiss population percentage
  - Citizenship acquisition rates
  - Income per capita analysis
  - Education level distribution
  - Age ratio calculations

- **Correlation Analysis**
  - Weighted and unweighted correlations
  - Multi-factor regression analysis
  - Regional variation studies
  - Demographic predictor identification

### 4. Data Processing Capabilities

- **Data Integration**
  - Multiple source harmonization
  - Geographic data transformation
  - Electoral data processing
  - Demographic data normalization

- **Quality Control**
  - Data consistency checks
  - Municipality matching validation
  - Population weighting adjustments
  - Statistical reliability testing

### 5. Visualization Components

- **Electoral Maps**
  - Winner representation maps
  - Vote share distribution
  - Party strength indicators

- **Statistical Plots**
  - Correlation scatter plots
  - Demographic distribution charts
  - Education level comparisons
  - Income distribution visualizations

## Usage

1. Clone the repository:
```bash
git clone https://github.com/rogerjeasy/r-bootcamp.git
```

2. Install dependencies:
```R
source("requirements.R")
```

There are two ways to run the analysis:

### Method 1: Running Main Scripts

1. Run complete analysis:
```R
source("Elections.R")
```

2. Generate all map visualizations:
```R
source("main_script_maps.R")
```

### Method 2: Running Individual Analysis Components

You can run specific analysis components separately using the scripts in dedicated folders:

1. Statistical Analysis Scripts (`Script Plots/`):
   ```R
   # Run statistical models
   source("r-bootcamp/Script Plots/stat_models.R")
   
   # Generate boxplots
   source("r-bootcamp/Script Plots/boxplots.R")
   
   # Create scatter and bar plots
   source("r-bootcamp/Script Plots/scatterplots_barplots.R")
   ```

2. Map Generation Scripts (`Script_Maps/`):
   ```R
   # Generate demographic maps
   source("r-bootcamp/Script_Maps/demographic_map.R")
   
   # Create canton-level result maps
   source("r-bootcamp/Script_Maps/map_canton_results.R")
   
   # Generate municipality-level visualizations
   source("r-bootcamp/Script_Maps/map_municipality_results.R")
   
   # Create box plot visualizations
   source("r-bootcamp/Script_Maps/visualizations_box_plot.R")
   ```

All generated plots and visualizations will be saved in the `Documentation/Plots/` directory.

## Contributing

1. Fork the repository
2. Create feature branch (`git checkout -b feature/improvement`)
3. Commit changes (`git commit -am 'Add improvement'`)
4. Push to branch (`git push origin feature/improvement`)
5. Submit Pull Request

### Code Style
- Follow tidyverse style guide
- Include tests for new features

## Contact

- Bavibidila, R.
- Jevdenic, R.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

---

_Project developed as part of electoral analysis research at Swiss Federal Statistical Office (BFS), 2024._