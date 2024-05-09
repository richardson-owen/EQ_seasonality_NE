# Spatial Variation of Changes in Extreme Discharge Seasonality across the Northeastern United States

## Overview
This repository contains the R code and data used in the analysis for the paper "Spatial Variation of Changes in Extreme Discharge Seasonality across the Northeastern United States" 
The study investigates the spatial variation of changes in extreme discharge seasonality across the Northeastern United States, focusing on precipitation and streamflow data.

## Author
- **Owen Richardson**
  - Affiliation: Dartmouth College Undergraduate, Class of 2024

## Date
- May 9, 2024

## Data
The analysis utilizes a complete record of precipitation from the dataset `complete_record_NE_1901.Rds`, which can be used instead of the raw precipitation data. 
The discharge data is loaded within the code, allowing for streamlined analysis.

### Raw Data Access
Access to the raw precipitation data can be found here (ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/)

## Usage
To run the analysis, ensure that you have R installed on your machine, and execute the scripts provided in the repository. Data files are loaded automatically by the scripts.
Additional files required to complete the analysis are attached.

## Files
- `complete_record_NE_1901.Rds`: The dataset containing the aggregated precipitation records.
- `EQ_seasonality_NE.R`: The main R script file used to perform the analysis.
- `ne_10m_lakes`: Northeastern US lakes shapefile, available here (https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-lakes/)
- `HCDNgages.csv`: List of site numbers for all USGS HCDN stream gages
- `QTable.csv`: 2 year flood (Q2) for each of the HCDN gages, determined using the Log-Pearson Type III distribution fit to annual instantaneous peak discharges.

## Contact
For any additional questions or requests, please contact Owen Richardson at richardson.owenhugo@gmail.com
