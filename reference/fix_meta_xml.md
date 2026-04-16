# Patch a Darwin Core Archive's meta.xml for OBIS compatibility

Post-processes the meta.xml inside a DwC-A zip to fix two issues that
LivingNorwayR does not handle automatically:

1.  Corrects the eMoF `rowType` to the OBIS URI
    (`http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact`) if the
    legacy TDWG URI is found.

2.  Adds `<field>` mappings for columns present in the CSV files but
    absent from meta.xml – specifically `eventType` (Event core) and
    `eventDate` (eMoF extension), which LivingNorwayR's term database
    does not recognise.

The zip is modified in place; a backup is removed on success.

## Usage

``` r
fix_meta_xml(zip_file)
```

## Arguments

- zip_file:

  Path to the DwC-A zip file.

## Value

Invisibly returns `zip_file`.
