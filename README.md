# SeriARC - Archaeological Seriation and Correspondence Analysis

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18498106.svg)](https://doi.org/10.5281/zenodo.18498106)

**SeriARC** is an interactive R/Shiny application for archaeological seriation, correspondence analysis, and Bayesian radiocarbon chronology modeling. It integrates compositional data analysis (artifact assemblages) with absolute dating (¹⁴C) to create robust chronological sequences for archaeological sites.

## Features

- **Correspondence Analysis (CA)** with detrended options and outlier detection
- **Seriation** with multiple transformation methods (relative, binary, log)
- **Clustering** (K-means, hierarchical, fuzzy, GMM) with comprehensive diagnostics
- **Radiocarbon Calibration** using IntCal20
- **OxCal Integration** for Bayesian sequence modeling (local installations only)
- **Interactive Visualization** with plotly, ggplot2, and leaflet maps
- **Bilingual Interface** (German/English)
- **Publication-Quality Exports** (PNG, SVG, PDF, Excel)

## Installation

### Requirements

- R ≥ 4.0.0
- Required packages are listed in `renv.lock`

### Local Installation

```r
# Clone repository
git clone https://github.com/Archaeoscan/SeriARC.git
cd SeriARC

# Restore package environment (recommended)
install.packages("renv")
renv::restore()

# Or install packages manually
install.packages(c("shiny", "shinydashboard", "DT", "plotly", 
                   "FactoMineR", "ggplot2", "leaflet", "readxl", 
                   "writexl", "cluster", "khroma"))

# Start application
shiny::runApp()
```

### Optional: OxCal Integration

For full OxCal functionality (local installations only):

1. Install OxCal: https://c14.arch.ox.ac.uk/oxcal.html
2. Install oxcAAR: `install.packages("oxcAAR")`
3. Set path: `Sys.setenv(OXCAL_PATH = "path/to/OxCal.exe")`

## Execution Modes

SeriARC supports two execution modes:

### 1. Full Local Installation
- All features including OxCal integration
- Requires local OxCal installation
- Recommended for intensive analysis work

### 2. Cloud/Web Deployment
- Runs on shinyapps.io
- URL: https://archaeoscan.shinyapps.io/SeriARC-v-1-0-0
- OxCal modules automatically disabled
- CQL code can be exported and run in OxCal separately

## Usage

1. **Import Data**: Load Excel/CSV files with archaeological assemblages
2. **Correspondence Analysis**: Visualize chronological patterns
3. **14C Integration**: Add radiocarbon dates for absolute chronology
4. **Export Results**: Generate publication-quality figures and reports

### Example Dataset

The application includes the **Michelsberg Culture** dataset from the `archdata` package for testing and demonstration purposes.

## Documentation

SeriARC provides extensive **in-app help texts, tooltips, and explanatory modals** for all features. The interface is designed to be self-explanatory for archaeologists without programming experience.

## Testing

Due to the highly interactive nature of the Shiny application, automated unit testing of the full user interface is not practical. The application was tested manually using the included example dataset, verifying all core workflows and visual outputs in both supported languages (German/English).

## Citation

If you use SeriARC in your research, please cite:

```bibtex
@software{meixner2025seriarc,
  author = {Meixner, Daniel},
  title = {SeriARC: Archaeological Seriation and Correspondence Analysis},
  year = {2025},
  version = {1.0.0},
  url = {https://github.com/Archaeoscan/SeriARC},
  doi = {10.5281/zenodo.18498106}
}
```

## License

GPL-3.0 - see [LICENSE](LICENSE) file for details.

## Author

**Daniel Meixner**  
University of Regensburg  
Daniel.Meixner@geschichte.uni-regensburg.de

## Acknowledgments

- IntCal20 calibration curve (Reimer et al. 2020)
- Michelsberg dataset from `archdata` package

