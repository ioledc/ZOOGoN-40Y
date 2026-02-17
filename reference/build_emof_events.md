# Build event-level eMoF sampling metadata

Creates one set of sampling metadata per event, including sampling
protocol, instrument type, net mouth area and diameter. Instrument and
geometry depend on sampling period (pre/post 2016-02-18).

## Usage

``` r
build_emof_events(data = NULL)
```

## Arguments

- data:

  A data frame containing at least `eventID` and `eventDate`.

## Value

A tibble in eMoF format with event-level measurements
(`occurrenceID = NA`) and controlled NERC vocabulary identifiers for
protocol (P01/SAMPPROT), instrument (P01/NMSPINST + L22), mouth area
(P01/MTHAREA1, m²) and mouth diameter (P01/DSAMPA01, m).
