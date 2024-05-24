# Spatial Variation of Changes in Extreme Discharge Seasonality across the Northeastern United States

## Overview
This repository contains the R code and data used in the analysis for the paper "Spatial Variation of Changes in Extreme Discharge Seasonality across the Northeastern United States" 

## Author
- **Owen Richardson**
  - Affiliation: Dartmouth College Undergraduate, Class of 2024

## Date
- May 9, 2024

## Usage
To run the analysis, ensure that you have R installed on your machine, and execute the scripts provided in the repository. Data files are loaded automatically by the scripts.
Additional files required to complete the analysis are attached.

## Files
- `EQ_seasonality_NE.R`: The main R script file used to perform the analysis.
- `complete_record_NE_1901.Rds`: The dataset containing the aggregated precipitation records. Access to the raw precipitation data can be found at ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/
- `ne_10m_lakes`: Lakes shapefile, available at https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-lakes/
- `HCDNgages.csv`: List of site numbers for all USGS HCDN stream gages
- `QTable.csv`: 2 year flood (Q2) for each of the HCDN gages, determined using the Log-Pearson Type III distribution fit to annual instantaneous peak discharges.
- `ghcnd_stations.csv`: GHCN Daily stations, used for precipitation analysis.

## Raw Data
GHCNd station precipitation data are available at ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/. Stream gauging records are available at https://waterdata.usgs.gov/nwis (USGS). 

## Contact
For any additional questions or requests, please contact Owen Richardson at owen.richardson@colostate.edu
