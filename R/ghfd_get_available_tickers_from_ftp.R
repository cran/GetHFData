#' Function to get available tickers from ftp
#'
#' This function will read the Bovespa ftp for a given market/date and output
#' a numeric vector where the names of the elements represents the different tickers
#' and the numeric values as the number of trades for each ticker
#'
#' @param my.date A single date to check tickers in ftp (e.g. '2015-11-03')
#' @inheritParams ghfd_get_HF_data
#'
#' @return A vector with the number of trades for each ticker found in file
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  available.tickers <- ghfd_get_available_tickers_from_ftp(my.date = '2015-11-03',
#'  type.market = 'BMF')
#'
#'  print(available.tickers)
#' }
ghfd_get_available_tickers_from_ftp <- function(my.date = '2015-11-03',
                                                type.market = 'equity',
                                                dl.dir = 'ftp files',
                                                max.dl.tries = 10){

  if (length(my.date)!=1){
    stop('ERROR: input my.date should have length 1')
  }

  # check date class
  my.date <- as.Date(my.date)
  if (class(my.date) != 'Date') {
    stop('ERROR: Input first.date should be of class Date')
  }

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

  # get contents
  df.ftp <- ghfd_get_ftp_contents(type.market = type.market)

  idx <- df.ftp$dates == my.date
  files.to.dl <- df.ftp$files[idx]

  my.links <- paste0(my.ftp, files.to.dl)

  my.url <- my.links[1]
  out.file <- paste0(dl.dir, '/', files.to.dl[1])

  ghfd_download_file(my.url, out.file, max.dl.tries)

  suppressWarnings(
    my.df <- readr::read_csv2(file = out.file, skip = 1, progress = F, col_names = F)
  )

  out <- sort(table(my.df$X2), decreasing = T)

  return(out)

}

