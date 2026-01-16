# imdtools 📦

Tools for Indices of Multiple Deprivation (IMD) data in R.

## Installation and usage

To install:

```r
pak::pak("The-Strategy-Unit/imdtools")
```

To generate a lookup table of IMD2025 ranks and deciles for all English LSOAs:

```r
library(imdtools)
get_imd_lookup()
```

This table is created from "File 1" on the [GOV.UK IMD2025 webpage][govimd].

[govimd]: https://www.gov.uk/government/statistics/english-indices-of-deprivation-2025

To generate a table of the LSOA-level transformed scores for each of the 7
IMD domains:

```r
get_transformed_scores()
```

This table is created from "File 9" on the [GOV.UK IMD2025 webpage][govimd].

Further functions may be added in future if they are thought to be helpful.

## Problems

Please use GitHub Issues to report any problems or make suggestions for further
development.
