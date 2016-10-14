### Version 1.2.0 (2016-10-14)

Minor update with the following changes:

* The function  ghfd_get_HF_data now allows for partial matching of asset names and also the download of all assets available in ftp files
* Function ghfd_get_available_tickers_from_ftp also returns the type of market in data.frame 

### Version 1.1.0 (2016-08-15)

Major update from initial version with the following changes:

* The function for finding tickers in the ftp now looks for the closest date in the case that the actual date is missing from the ftp
* The function for finding tickers now returns a dataframe with the tickers and number of trades
* Added control for bad files
* The output for raw and agg type of output were revised
* The vignette is revised

### Version 1.0.0 - First commit (2016-07-21)
