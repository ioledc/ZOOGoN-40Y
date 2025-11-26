# ZooGoN

**Gulf of Naples - 40 Years of Zooplankton Biodiversity Assessment**

ZooGoN standardizes taxonomic names in Mediterranean zooplankton
datasets spanning four decades (1984-2024) from the LTER-MareChiara
station in the Gulf of Naples. This R package is part of the
[**DTO-BioFlow project**](https://dto-bioflow.eu) (Digital Twin Ocean -
Biodiversity Flow Integration) under the EU Horizon Mission “Restore our
Ocean & Waters by 2030”.

## Project Context

This package processes the most comprehensive long-term zooplankton
dataset from the Western Mediterranean Sea, including:

- 📊 **1,506 zooplankton samples** (1984-2024)  
- 🦐 **148 copepod species** + 61 other taxa
- 🌍 Integration with **European Digital Twin of the Ocean**

## Key Features

- **Taxonomic Standardization**: Robust genus-species extraction from
  complex taxonomic strings
- **Species Complex Handling**: Converts multi-species entries
- **Family-level Processing**: Creates standardized names from family
  entries
- **Darwin Core Compliance**: Converts datasets to international
  biodiversity standards
- **FAIR Data Principles**: Ensures Findable, Accessible, Interoperable,
  Reusable data
- **EMODnet Biology Integration**: Quality-controlled data publication
  workflow

## Installation

You can install the development version of ZooGoN from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ioledc/ZOOGoN-40Y")
```

## Usage

### Basic Taxonomic Standardization

``` r
library(ZooGoN)

# Example taxonomic names from Gulf of Naples zooplankton samples
taxa_examples <- c(
  "Sardinella+Sardinops",                    # Species complex
  "Clupeidae n.i.",                          # Family level  
  "Engraulis - group",                       # Higher group
  "Lutjanus (Paradies) argentimaculatus (Forsskål, 1775)",  # Full binomial
  "Chiridius poppei Giesbrecht, 1893"       # Standard binomial
)

# Standardize taxonomic names
standardized <- extract_genus_species(taxa_examples)
print(standardized)

# Expected output:
#   original_name                                          genus_species
#   <chr>                                                  <chr>        
# 1 Sardinella+Sardinops                                   Sardinella spp
# 2 Clupeidae n.i.                                         Clupegenus sp
# 3 Engraulis - group                                      Engraulis indet
# 4 Lutjanus (Paradies) argentimaculatus (Forsskål, 1775) Lutjanus argentimaculatus
# 5 Chiridius poppei Giesbrecht, 1893                     Chiridius poppei
```

### Complete Data Processing Workflow

The package provides a comprehensive function that processes the entire
LTER-MareChiara dataset from raw Excel files to Darwin Core format:

``` r
# Complete workflow in a single function call
processed_data <- process_lter_data(
  zoo_data_path = "data/lter_zoo_84_13.xlsx",
  ids_data_path = "data/ids.xlsx", 
  worms_validation = TRUE,
  output_format = "list",
  verbose = TRUE
)

# Access Darwin Core formatted results
event_table <- processed_data$event
occurrence_table <- processed_data$occurrence
measurements <- processed_data$emof
```

This integrated workflow includes: - **Taxonomic standardization** using
`extract_genus_species()` - **WoRMS validation** for taxonomic accuracy
(optional) - **Darwin Core formatting** with Event, Occurrence, and eMoF
tables - **Geographic metadata** for LTER-MareChiara station - **Quality
control** and data validation - **CSV export** capabilities

### Direct CSV Export

``` r
# Export processed data directly to CSV files
process_lter_data(
  output_format = "csv",
  output_dir = "processed_data/darwin_core"
)
```

## Dataset Overview

The LTER-MareChiara zooplankton dataset represents one of the longest
continuous time series in the Mediterranean Sea:

| Period    | Frequency      | Samples | Net Type                    | Fixation          |
|-----------|----------------|---------|-----------------------------|-------------------|
| 1984-1990 | Biweekly       | 156     | Indian Ocean (200μm, 113cm) | Formaldehyde 2-4% |
| 1991-1994 | *Interruption* | \-      | \-                          | \-                |
| 1995-2015 | Weekly         | 1,092   | Indian Ocean (200μm, 113cm) | Formaldehyde 2-4% |
| 2016-2024 | Weekly         | 258     | WP2 (200μm, 70cm)           | Ethanol 96%       |

**Total: 1,506 samples • 148 copepod species • 61 other taxa**

## Data Standards & Compliance

ZooGoN ensures compatibility with international biodiversity data
standards:

- **🗂️ Darwin Core Archive**: International standard for biodiversity
  data
- **🌐 WoRMS Integration**: World Register of Marine Species taxonomic
  validation  
- **📊 BODC NERC Vocabulary**: Standardized measurement terminology
- **⚡ EMODnet Biology**: European marine biodiversity data
  infrastructure
- **🏷️ ISO19115 Metadata**: International metadata standards
- **📄 FAIR Principles**: Findable, Accessible, Interoperable, Reusable
  data

## Contributing

This package is part of the DTO-BioFlow project timeline (2025-2026):

- **May 2025**: Project initiation, Paris workshop
- **August 2025**: First interim report, EMODnet training completion
- **December 2025**: Second interim report
- **April 2026**: Final deliverables and EMODnet Biology publication

## Citation

``` r
citation("ZooGoN")
```

## Funding

This work is supported by the DTO-BioFlow project
(HORIZON-MISS-2022-OCEAN-01-07) under the EU Mission “Restore our Ocean
& Waters by 2030” through a Financial Support to Third Parties (FSTP)
grant of €60,000.

## Contact

- **Principal Investigator**: Iole Di Capua (<iole.dicapua@szn.it>)

**Institution**: Stazione Zoologica Anton Dohrn, Naples, Italy

**ORCID**: [0000-0003-2959-8977](https://orcid.org/0000-0003-2959-8977)

- **Principal Analyst**: Lorenzo Longobardi
  (<lorenzo.longobardi@gmail.com>)

**Institution**: WorldFish

**ORCID**: [0000-0003-2959-8977](https://orcid.org/0000-0003-3126-7341)

- ## Acknowledgments

- LTER-MareChiara research station

- DTO-BioFlow project consortium

- EMODnet Biology data infrastructure

- European Digital Twin of the Ocean initiative
