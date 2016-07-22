#' Reads zip file downloaded from Bovespa ftp
#'
#' @param out.file Name of zip file
#' @inheritParams ghfd_get_HF_data
#'
#' @return A dataframe with the raw (tick by tick) dataset
#' @export
#'
#' @examples
#'
#' my.assets <- c('ABEVA20', 'PETRL78')
#'
#' ## getting data from local file (in practice it would be downloaded from ftp)
#' out.file <- system.file("extdata", 'NEG_OPCOES_20151126.zip', package = "GetHFData")
#'
#' df.out <- ghfd_read_file(out.file, my.assets)
#' print(head(df.out))
ghfd_read_file <- function(out.file,
                           my.assets,
                           first.time = '10:00:00',
                           last.time = '17:00:00',
                           type.output = 'agg',
                           agg.diff = '15 min'){

  # ERROR CHECKING

  col.names <- c('SessionDate','InstrumentSymbol','TradeNumber', 'TradePrice', 'TradedQuantity','Tradetime',
                 'TradeIndicator', 'BuyOrderDate', 'SequentialBuyOrderNumber','SecondaryOrderID',
                 'AggressorBuyOrderIndicator','SellOrderDate','SequentialSellOrderNumber','SecondaryOrderID2',
                 'AggressorSellOrde Indicator','CrossTradeIndicator', 'BuyMember','SellMember')


  # fix for "no visible binding" NOTE from CHECK
  #utils::suppressForeignCheck(col.names)

  InstrumentSymbol <- TradeIndicator <- SessionDate <- TradePrice <-  NULL
  TradedQuantity <- Tradetime <-  AggressorBuyOrderIndicator <- TradeDateTime <- NULL
  last.price <- TradeSign <- NULL

  # read data (warning are from last line)
  suppressWarnings(
    my.df <- readr::read_csv2(file = out.file, skip = 1, col_names = col.names, progress = F)
  )

  cat(paste(' - Imported ', nrow(my.df), 'lines,',length(unique(my.df$InstrumentSymbol)),'unique tickers'))
  cat(paste('\n   -> Processing file'))

  # filter raw data for assets and cancelled trades
  my.df <- dplyr::filter(my.df,
                         InstrumentSymbol %in% my.assets,
                         TradeIndicator ==1)

  # keep only columns with interesting information
  my.df <- dplyr::select(my.df,
                         SessionDate, InstrumentSymbol,TradePrice,TradedQuantity,Tradetime,AggressorBuyOrderIndicator)

  # Set objet classes correct
  my.df$TradePrice <- as.numeric(my.df$TradePrice)
  my.df$TradedQuantity <- as.numeric(my.df$TradedQuantity)
  my.df$SessionDate <- as.Date(my.df$SessionDate)
  my.df$TradeDateTime <- lubridate::ymd_hms(paste(my.df$SessionDate,my.df$Tradetime),
                                        tz = "America/Sao_Paulo")


  # trading signs
  my.df$TradeSign <-   (my.df$AggressorBuyOrderIndicator ==1)*1 - (my.df$AggressorBuyOrderIndicator ==2)*1
  my.df$AggressorBuyOrderIndicator <- NULL

  cat(paste0(' - Found ', nrow(my.df), ' lines, ',
             length(unique(my.df$InstrumentSymbol)),' unique tickers'))


  # remove day times outside period from first.time and last.time

  unique.dates <- unique(my.df$SessionDate)

  first.time <-
    as.POSIXct(paste0(unique.dates[1], ' ', first.time, ' BRT'))
  last.time <-
    as.POSIXct(paste0(unique.dates[1], ' ', last.time, ' BRT'))

  idx <- ( my.df$TradeDateTime >= first.time)&( my.df$TradeDateTime <= last.time)

  my.df <- my.df[idx, ]

  if (type.output == 'raw') {
    return(my.df)

  } else {
    # Aggregate the data

    my.seq <- seq(from = first.time, to = last.time, by = agg.diff)

    my.breaks <- paste(as.character(unique.dates),
                       rep(format(my.seq, '%H:%M:%S'),
                           length(unique.dates)),
                       'BRT')

    my.grid <-
      expand.grid(format(my.seq, '%H:%M:%S'), as.character(unique.dates))

    my.breaks <-
      as.POSIXct(paste(my.grid[[2]], my.grid[[1]], 'BRT'))

    my.df$TradeDateTime <- cut(my.df$TradeDateTime, breaks = my.breaks)

    # dplyr magic!

    my.G <- dplyr::group_by(my.df, InstrumentSymbol, SessionDate, TradeDateTime)

    t.out <- dplyr::summarise(my.G,
                              n.trades = length(TradePrice),
                              last.price     = TradePrice[length(TradePrice)],
                              weighted.price = sum(TradePrice*(TradedQuantity*TradePrice)/sum(TradedQuantity*TradePrice)),
                              period.ret     = last.price/TradePrice[1] -1,
                              period.ret.volat = stats::sd(TradePrice[2:length(TradePrice)]/TradePrice[1:length(TradePrice)-1] -1, na.rm = T),
                              sum.qtd        = sum(TradedQuantity),
                              sum.vol        = sum(TradedQuantity*TradePrice),
                              n.buys         = sum(TradeSign == 1),
                              n.sells        = sum(TradeSign == -1)  )

    t.out$Tradetime <- format(as.POSIXct(t.out$TradeDateTime), '%H:%M:%S')

    t.out$TradeDateTime <- as.POSIXct(t.out$TradeDateTime, tz = 'America/Sao_Paulo')

    # remove NA (trades outside of time interval)
    t.out <- stats::na.omit(t.out)

    return(t.out)
  }


}

