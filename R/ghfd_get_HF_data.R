#' Downloads and aggregates high frequency trading data directly from the Bovespa ftp
#'
#' This function downloads zip files containing trades from Bovespa's ftp (ftp://ftp.bmf.com.br/MarketData/) and imports it into R.
#' See the vignette and examples for more details on how to use the function.
#'
#' @param my.assets The tickers (symbols) of the derised assets to import data (e.g. c('PETR4', 'VALE5'))
#' @param type.market The type of market to download data from ('equity', 'options', 'BMF' )
#' @param first.date The first date of the imported data (Date class)
#' @param last.date  The last date of the imported data (Date class)
#' @param first.time The first intraday period to import the data. All trades before this time of day are ignored. As character, e.g. '10:00:00'.
#' @param last.time The last intraday period to import the data. All trades after this time of day are ignored. As character, e.g. '18:00:00'.
#' @param type.output Defines the type of output of the data. The choice 'agg' outputs aggregated data for time intervals defined in agg.diff.
#'        The choice 'raw' outputs the raw, tick by tick, data from the zip files.
#' @param agg.diff The time interval used in the aggregation of data. Only used for type.output='agg'. It should contain a integer followed by a time unit ('sec' or 'secs', 'min' or 'mins', 'hour' or 'hours', 'day' or 'days').
#'        Example: agg.diff = '15 mins', agg.diff = '1 hour'.
#' @param dl.dir The folder to download the zip files
#' @param max.dl.tries Maximum attempts to download the files from ftp
#' @param clean.files Should the files be removed after reading it? (TRUE or FALSE)
#'
#' @return A dataframe with the financial data (raw (tick by tick) or aggregated)
#' @export
#'
#' @examples
#'
#' my.assets <- 'ABEVA69'
#' type.market <- 'options'
#' first.date <- as.Date('2015-12-29')
#' last.date <- as.Date('2015-12-29')
#'
#' \dontrun{
#' df.out <- ghfd_get_HF_data(my.assets, type.market, first.date,  last.date)
#' }
ghfd_get_HF_data <- function(my.assets,
                             type.market,
                             first.date,
                             last.date,
                             first.time = '10:00:00',
                             last.time = '17:00:00',
                             type.output = 'agg',
                             agg.diff = '15 min',
                             dl.dir = 'ftp files',
                             max.dl.tries = 10,
                             clean.files = FALSE) {
  # check for internet

  test.internet <- curl::has_internet()

  if (!test.internet) {
    stop('No internet connection found...')
  }

  # check date class
  if (class(first.date) != 'Date') {
    stop('ERROR: Input first.date should be of class Date')
  }

  if (class(last.date) != 'Date') {
    stop('ERROR: Input first.date should be of class Date')
  }

  # check type.output
  possible.names <- c('agg', 'raw')

  idx <- type.output %in% possible.names

  if (!any(idx)) {
    stop(paste(
      c(
        'Input type.output is not valid. It should be one of the following: ',
        possible.names
      ),
      collapse = ', '
    ))
  }
  # check type.market
  possible.names <- c('equity', 'options', 'BMF')

  idx <- type.market %in% possible.names

  if (!any(idx)) {
    stop(paste(
      c(
        'Input type.market is not valid. It should be one of the following: ',
        possible.names
      ),
      collapse = ', '
    ))
  }

  # check clean.files

  if (!is.logical(clean.files )){
    stop('ERROR: Input clean.files should be a logical')
  }

  # check first/last time input

  test.date <- as.POSIXct(paste0('2016-01-01', first.time, ' BRT'), format = '%Y-%m-%d %H:%M:%S')
  if (is.na(test.date)){
    stop(paste0('ERROR: Cant convert objet start.time (',first.time,') to a POSIXct time class.' ))
  }

  test.date <- as.POSIXct(paste0('2016-01-01', last.time, ' BRT'), format = '%Y-%m-%d %H:%M:%S')
  if (is.na(test.date)){
    stop(paste0('ERROR: Cant convert objet last.time (', last.time ,') to a POSIXct time class.' ))
  }

  # check agg.diff input
  possible.char <- c('sec', 'secs', 'min','mins','hour','hours','day','days')

  test.char <- stringr::str_detect(agg.diff, possible.char)

  if (!any(test.char)){
    stop(paste0('ERROR: Input agg.diff (',agg.diff,') should have one of the following strings: ',
               paste(possible.char, collapse = ', ')))
  }

  # create directory

  if (!dir.exists(dl.dir)) {
    dir.create(dl.dir)
  }


  # set ftp site
  if (type.market == 'equity')
    my.ftp <- "ftp://ftp.bmf.com.br/marketdata/Bovespa-Vista/"
  if (type.market == 'options')
    my.ftp <- "ftp://ftp.bmf.com.br/MarketData/Bovespa-Opcoes/"
  if (type.market == 'BMF')
    my.ftp <- "ftp://ftp.bmf.com.br/marketdata/BMF/"

  # first msgs

  cat('\nRunning ghfd_get_HF_Data() for:')
  cat('\n   type.market =', type.market)
  cat('\n   my.assets =', paste0(my.assets, collapse = ', '))
  cat('\n   type.output =', type.output)
  if (type.output=='agg') cat('\n      agg.diff =', agg.diff)

  # get contents
  df.ftp <- ghfd_get_ftp_contents(type.market = type.market)

  cat('\n   Found ', nrow(df.ftp), ' files in ftp')
  cat('\n   First Date in ftp: ', as.character(df.ftp$dates[1]))
  cat('\n   Last Date in ftp:  ', as.character(df.ftp$dates[nrow(df.ftp)]))

  # filter files to dl
  idx <- (df.ftp$dates >= first.date) & (df.ftp$dates <= last.date)
  files.to.dl <- df.ftp$files[idx]

  if (length(files.to.dl) == 0) {
    stop(
      'ERROR: No files in ftp match the interval given by first.date and last.date (you should check your dates). Returning empty data.frame'
    )
  }

  my.links <- paste0(my.ftp, files.to.dl)

  df.out <- data.frame()
  for (i.f in seq_along(my.links)) {
    my.url <- my.links[i.f]
    out.file <- paste0(dl.dir, '/', files.to.dl[i.f])

    cat(paste0(
      '\nDownloading ',
      out.file,
      ' (',
      i.f,
      '|',
      length(my.links),
      ')'
    ))

    ghfd_download_file(my.url, out.file, max.dl.tries)

    cat(paste('\n   -> Reading files'))

    my.df <- ghfd_read_file(out.file,
                            my.assets = my.assets,
                            first.time = first.time,
                            last.time = last.time,
                            type.output = type.output,
                            agg.diff = agg.diff)


    df.out <- rbind(df.out, as.data.frame(my.df))

    # clean up some memory
    rm('my.df')

    # clean up files?
    if (clean.files) {
      cat(paste('\n   -> Deleting downloaded files'))
      file.remove(out.file)
    }
  }


  if (nrow(df.out) == 0) {
    stop(
      paste(
        'Warning: No data found in files for assets',
        paste(my.assets, collapse = ', '),
        'You should check your dates and assets.'
      )
    )
  }

  return(df.out)
}
