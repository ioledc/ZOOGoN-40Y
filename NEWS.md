# ZooGoN 1.0.0

## Major Release - Comprehensive Data Processing Pipeline

This major release introduces a complete data processing workflow for the LTER-MareChiara zooplankton dataset, transforming ZooGoN from a basic taxonomic standardization tool into a comprehensive biodiversity data processing package.

### New Features

* **`process_lter_data()` Function**: Complete integrated workflow from raw Excel files to Darwin Core format
* **WoRMS Taxonomic Validation**: Optional integration with World Register of Marine Species for taxonomic accuracy
* **Flexible Output Options**: Support for both R list objects and direct CSV export
* **Enhanced Error Handling**: Comprehensive validation and graceful error recovery
* **Geographic Metadata Integration**: Automatic LTER-MareChiara station coordinates and sampling information
* **Processing Metadata**: Detailed workflow information and data quality metrics

### Technical Improvements

* **Enhanced Dependencies**: Added `readr` and `worrms` packages for improved functionality
* **Comprehensive Documentation**: Extensive function documentation with examples and parameter descriptions
* **Quality Control**: Built-in data validation and processing verification
* **Performance Optimization**: Efficient handling of large taxonomic datasets (40 years of data)

### Data Processing Capabilities

* **Complete Workflow**: Single function handles entire Excel → Darwin Core pipeline
* **Taxonomic Standardization**: Enhanced `extract_genus_species()` integration with WoRMS validation
* **Darwin Core Extensions**: Automatic generation of Event, Occurrence, and eMoF tables
* **Temporal Data Handling**: Robust Excel date conversion and temporal metadata management
* **Sample Identification**: Automated mapping between dates and standardized sample IDs

### Documentation Updates

* **Enhanced Vignettes**: Updated data processing workflow to demonstrate `process_lter_data()`
* **README Improvements**: Showcases new comprehensive processing capabilities
* **CLAUDE.md Updates**: Reflects new package architecture and main functions
* **Function Examples**: Comprehensive usage examples for all processing scenarios

### Breaking Changes

* **Package Version**: Updated to 1.0.0 to reflect major functionality additions
* **Enhanced Dependencies**: New required packages for full functionality

### Performance

* **Large Dataset Support**: Efficiently processes 1,506 samples with 148+ taxa
* **Memory Management**: Optimized for 40-year temporal datasets
* **Optional Processing**: WoRMS validation can be disabled for faster execution

# ZooGoN 0.1.0

## Initial Release

This is the initial release of ZooGoN as part of the DTO-BioFlow project (Gulf of Naples - 40 Years of Zooplankton Biodiversity Assessment).

### Features

* **Taxonomic Standardization**: Core `extract_genus_species()` function for standardizing Mediterranean zooplankton taxonomic names
* **Darwin Core Compliance**: Full support for converting LTER-MareChiara data to Darwin Core Archive format
* **FAIR Data Principles**: Ensures Findable, Accessible, Interoperable, Reusable data output
* **EMODnet Biology Integration**: Quality-controlled data ready for European marine biodiversity infrastructure

### Dataset Coverage

* **Temporal Scope**: 1984-2024 (40 years of continuous monitoring)
* **Sample Size**: 1,506 zooplankton samples from Gulf of Naples
* **Taxonomic Diversity**: 148 copepod species + 61 other zooplankton taxa
* **Geographic Coverage**: LTER-MareChiara station (40°81'N, 14°25'E), Tyrrhenian Sea

### Documentation

* Comprehensive package documentation with `pkgdown` website
* Data processing workflow vignette
* Darwin Core integration guide
* EMODnet Biology compliance documentation

### Project Context

This package represents the technical "workbase" for the ZOOGoN-40Y project, funded by a €60,000 DTO-BioFlow FSTP grant under the EU Horizon Mission "Restore our Ocean & Waters by 2030". The work is conducted by Stazione Zoologica Anton Dohrn (Naples, Italy) as part of the broader Digital Twin of the Ocean initiative.

### Standards Compliance

* **Darwin Core Archive**: International biodiversity data standard
* **WoRMS Integration**: World Register of Marine Species validation
* **BODC NERC Vocabulary**: Standardized measurement terminology  
* **ISO19115 Metadata**: International metadata standards
* **Creative Commons Licensing**: CC-BY-NC for open science

### Acknowledgments

This work builds on 40 years of dedicated zooplankton monitoring at the LTER-MareChiara research station and contributes to the European Digital Twin of the Ocean for enhanced marine ecosystem understanding and management.

# ZooGoN 0.0.1

- Initial commit
