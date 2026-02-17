# Build occurrence-level eMoF table

Converts occurrence attributes (abundance, sex, life stage) to Darwin
Core eMoF format. Recodes legacy life stage/sex codes to controlled
vocabularies (NERC P01/S10/S11) and assigns units for abundance
(ind/m³).

## Usage

``` r
build_emof_occurrence(data = NULL)
```

## Arguments

- data:

  A data frame containing at least `eventID`, `eventDate`,
  `occurrenceID`, `individualCount`, and `lifeStage`.

## Value

A tibble in eMoF format with one row per occurrence-level measurement
and the columns: `eventID`, `occurrenceID`, `eventDate`,
`measurementType`, `measurementTypeID`, `measurementValue`,
`measurementValueID`, `measurementUnit`, `measurementUnitID`.
