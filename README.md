# HEED: Nepal Solar Streetlights

## Data repositories

The raw system data for all streetlights is available at the project Data Portal (http://data-portal-heed.s3-website.eu-west-2.amazonaws.com/sensor/163) under Systems and Sensors: SL Nepal- All. The portal is a one-stop-shop for the raw data collected through project's surveys, sensor and energy monitoring systems and photo reportages. The registration on the portal is free and easy.

The system data used for performance analysis for the paper titled 'Analysis of standalone solar streetlights for improved energy access in displaced settlements' is deposited on Zenodo https://zenodo.org/record/3947993#.XxBLRJNKiuU

## Analysis scripts

The below scripts are used for analysis for the paper titled 'Analysis of standalone solar streetlights for improved energy access in displaced settlements'

* nepal_sl_stitching_3Apr.R - to stitch together raw data for selected variables from July 2019 to March 2019
* nepal_preprocessing_17Jun.R - to preprocess raw data, analyse yield, convert data into hourly means and evaluate the suitability of Seas-based imputation
* nepal_imputation_18Jun.R - to convert data into hourly means and impute missing data using Seas. Corrections using rule based approach are applied to evaluate demand and actual load
* nepal_analysis_2Jul.R - to analyse corrected hourly data and generate plots for the paper
* nepal_data_upload.R - to generate files for upload on Zenodo