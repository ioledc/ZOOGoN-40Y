# Convert legacy LTER-MareChiara zooplankton data to Darwin Core format

This function converts preprocessed legacy zooplankton datasets from the
LTER-MareChiara station into Darwin Core-compliant format for EMODnet
Biology publication and Digital Twin Ocean integration. The function
processes standardized parquet files containing taxonomically validated
data with WoRMS LSIDs (Life Science Identifiers).

## Usage

``` r
raw_to_dc(verbose = TRUE)
```

## Arguments

- verbose:

  Logical. Whether to print processing messages. Default is TRUE.

## Value

A list containing Darwin Core formatted tables and metadata:

- event:

  Event extension table with sampling event information (eventID,
  eventDate, geographic coordinates, sampling protocol)

- occurrence:

  Occurrence extension table with species occurrence data (eventID,
  occurrenceID, scientificName, scientificNameID, occurrenceStatus)

- emof:

  Extended Measurement or Fact (eMoF) table with measurements
  (occurrenceID, measurementType, measurementValue, measurementTypeID,
  measurementValueID, measurementUnitID) including individual counts,
  sex, and life stage information

- raw_data:

  Original preprocessed data before Darwin Core conversion

- processing_info:

  List with processing metadata (processing_date, total_events,
  total_occurrences, total_measurements, date_range, unique_taxa)

- metadata:

  Tibble with dataset-level metadata (title, contact, institution,
  license, project information)

## Details

**Input Data Format:**

The function expects preprocessed legacy data in parquet format with the
following structure:

- `eventID`: Unique sampling event identifier (e.g., "mc_1", "mc_2")

- `eventDate`: Sampling date in Date format (YYYY-MM-DD)

- `scientificname`: Full scientific name with WoRMS validation

- `lsid`: WoRMS Life Science Identifier URN (e.g.,
  "urn:lsid:marinespecies.org:taxname:104251")

- `individualCount`: Abundance measurement (ind/m³)

- `lifeStage`: Life stage code ("f" = female, "m" = male, "j" =
  juvenile, "fm" = both sexes, "fmj" = all stages)

**Current Implementation:**

Currently processes the file `McZoo_84-13.parquet` (1984-2013 data).
Future versions will support additional legacy files
(`McZoo_16.parquet`, `McZoo_17.parquet`, etc.) following the same
standardized format.

**Darwin Core Conversion:**

The function creates three Darwin Core extension tables following OBIS
(Ocean Biodiversity Information System) standards:

- **Event Extension**: Core table with sampling event metadata including
  temporal (eventDate) and spatial information (decimalLatitude,
  decimalLongitude, locality, waterBody, depth ranges, samplingProtocol)

- **Occurrence Extension**: Links to Event via eventID, contains species
  occurrences with scientificName, scientificNameID (WoRMS LSID), and
  occurrenceStatus (present/absent based on individualCount \> 0)

- **eMoF Extension**: Extended Measurement or Fact table linked via
  occurrenceID, containing quantitative measurements with standardized
  vocabulary terms from BODC NERC Vocabulary Server (NVS):

  - Individual counts with NERC P06 units (ind/m³)

  - Sex information (P01/ENTSEX01) with S10 controlled vocabulary

  - Life stage information (P01/LSTAGE01) with S11 controlled vocabulary

**Geographic Information:**

LTER-MareChiara station coordinates are automatically assigned to all
events:

- Latitude: 40.81°N

- Longitude: 14.25°E

- Locality: LTER-MareChiara station

- Water Body: Mediterranean Sea (Gulf of Naples, Tyrrhenian Sea)

- Depth range: 0-50 meters (vertical tow)

- Country: Italy (Campania region)

**Data Standards:**

- Taxonomic identifiers: WoRMS LSIDs

- Measurement vocabulary: BODC NERC Vocabulary Server (NVS)

- Format: Darwin Core Archive structure

- Compliance: OBIS schema and EMODnet Biology requirements

## See also

- Darwin Core standard: <https://dwc.tdwg.org/>

- OBIS Manual for Darwin Core:
  <https://manual.obis.org/darwin_core.html>

- EMODnet Biology: <https://www.emodnet-biology.eu/>

- LTER-MareChiara:
  <https://deims.org/0b87459a-da3c-45af-a3e1-cb1508519411>

- WoRMS (World Register of Marine Species):
  <https://www.marinespecies.org/>

- BODC NERC Vocabulary Server: <https://vocab.nerc.ac.uk/>

## Examples

``` r
if (FALSE) { # \dontrun{
# Process legacy data to Darwin Core format
dc_data <- raw_to_dc()

# Access individual Darwin Core extension tables
events <- dc_data$event
occurrences <- dc_data$occurrence
measurements <- dc_data$emof

# View processing summary
dc_data$processing_info

# Access metadata
dc_data$metadata

# Silent processing (no console messages)
dc_data <- raw_to_dc(verbose = FALSE)
} # }
```
