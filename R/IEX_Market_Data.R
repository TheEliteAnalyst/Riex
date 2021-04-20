#' Retrieves quote, bids, asks, trades and system event for each Stock Symbol
#'
#' For more details, visit:\url{https://iexcloud.io/docs/api/#book}
#' @seealso Investors Exchange `IEX` developer guide \url{https://iexcloud.io/docs/api/}
#' @seealso Investors Exchange Group (IEX Group) offers flexible and salable pricing.\url{https://iexcloud.io/pricing/}
#' @seealso View Investors Exchange Group (IEX Group) terms of use and subscription levels.\url{https://iexcloud.io/terms/}
#' @seealso Package `iexcloudR`\url{https://github.com/schardtbc/iexcloudR}
#' @rawNamespace import (dplyr, except=c(first,last,filter,lag))
#' @import ggplot2
#' @import httr
#' @import purrr
#' @import rjson
#' @import stringr
#' @import urltools
#' @import xts
#' @import zoo
#' @import TTR
#' @import quantmod
#' @importFrom graphics text
#' @importFrom tidyr unnest
#' @importFrom tibble as_tibble
#' @author Myriam Ibrahim
# --------------------- Book ---------------------#
#' @param iex_sk is a character vector that include IEX Cloud API Secret Token
#' @param x A charcter vector that can include one or multiple  Tickers / Stocks Symbols
#' @details for function like book(), only one value wil be accepted per request
#' @return Data frame with stock(s) quote data
#' @examples
#' \donttest{
#'   iex.book("TSLA")
#' }
#' @export

iex.book <- function (x, iex_sk){
  stock.quote.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  url_stocks_list <- paste0(url_prefix, "/ref-data/symbols?token=",iex_sk)
  req <- httr::GET(url_stocks_list)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  stocks_list <- rjson::fromJSON(file=url_stocks_list)
  stocks_list <- sapply(stocks_list, "[[", "symbol")
  stocks_list <- as.list(stocks_list)
  if (toupper(x) %in% stocks_list == TRUE) print("Stock Info is available in IEX") else warning ("Stock Info is not available in IEX")
  x_encoded <- url_encode(paste(toupper(x), collapse=", "))
  url = paste0(url_prefix, "/stock/",x_encoded,"/book?token=",iex_sk)
  url.info <- httr::GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")) warning("No output to parse.") else stock.book <- rjson::fromJSON(url.content)
  stock.quote.df <- do.call(rbind, stock.book[[1]]) %>%
    as.data.frame
  stock.quote.df <- cbind(Field = rownames(stock.quote.df), stock.quote.df)
  rownames(stock.quote.df) <- 1:nrow(stock.quote.df)
  names(stock.quote.df)[names(stock.quote.df) =='V1'] <- 'Value'
return(stock.quote.df)
}

# --------------------- Chart ---------------------#
#' Returns adjusted and unadjusted historical data for up to 15 years. Useful for building charts.
#'
#' For more details, visit:\url{https://iexcloud.io/docs/api/#historical-prices}
#' @seealso Investors Exchange `IEX` developer guide \url{https://iexcloud.io/docs/api/}
#' @seealso Investors Exchange Group (IEX Group) offers flexible and salable pricing.\url{https://iexcloud.io/pricing/}
#' @seealso View Investors Exchange Group (IEX Group) terms of use and subscription levels.\url{https://iexcloud.io/terms/}
#' @seealso Package `iexcloudR`\url{https://github.com/schardtbc/iexcloudR}
#' @author Myriam Ibrahim
#' @param r An alpha numeric object that represents the time range selected.
#' @note  r takes only of of the values accepted IN API request. {"5y", "2y", "1y", "ytd", "6m", "3m", "1m", "1d"}
#' @inheritParams iex.book
#' @return Data frame that includes stock (s) financial data
#' @examples
#' \donttest{
#'   iex.chart("TSLA", "1y")
#' }
#' @export
iex.chart <- function(x,r, iex_sk){
  Open <- High <- Low <- Close <- Volume <- NULL
  url_prefix <- "https://cloud.iexapis.com/stable"
  url_stocks_list <- paste0(url_prefix, "/ref-data/symbols?token=",iex_sk)
  req <- httr::GET(url_stocks_list)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  stocks_list <- rjson::fromJSON(file=url_stocks_list)
  stocks_list <- sapply(stocks_list, "[[", "symbol")
  stocks_list <- as.list(stocks_list)
  if (toupper(x) %in% stocks_list == TRUE) print("Stock Info is available in IEX") else warning ("Stock Info is not available in IEX")
  if (tolower(r) %in% c("5y", "2y", "1y", "ytd", "6m", "3m", "1m", "1d")){
    print (paste0("Range selected is :", tolower(r)))
  }else{
    print ("Value incorrect. Select from valid values: 5y, 2y, 1y, ytd, 6m, 3m, 1m, 1d")
  }
  x_encoded <- url_encode(paste(toupper(x), collapse=", "))
  url = paste0(url_prefix, "/stock/",x_encoded,"/chart/", r,"?token=",iex_sk)
  url.info <- GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")) warning("No output to parse.") else stock.chart <- rjson::fromJSON(url.content)
  l  <- assign(paste0(x, "_chart"),stock.chart)
  stock.chart.df <- do.call(rbind, l) %>%
    as.data.frame
  assign(paste0(x, "_chart_df"),stock.chart.df)
  stock.chart.df$date <- as.Date(as.character(stock.chart.df$date), format='%Y-%m-%d')
  colnames(stock.chart.df)[which(colnames(stock.chart.df) %in% c("close","high","low", "open","volume") )] <- c("Close","High","Low", "Open","Volume")
  stock.chart.df <- stock.chart.df[ , c("date", "Open","High", "Low", "Close", "Volume")]
  stock.chart.ts <- tidyr::unnest(stock.chart.df[, (colnames(stock.chart.df) %in% c("date", "Open","High", "Low", "Close", "Volume"))], cols = c(Open, High, Low, Close, Volume))
  stock.chart.ts <- xts(stock.chart.ts[,-1], order.by = stock.chart.ts$date)

return(stock.chart.ts)

}

# --------------------- Intraday ---------------------#
#' Returns 1 minute bar data where open, high, low, and close are per minute. For latest stock price use the iex.quote function
#'
#' For more details, visit:\url{https://iexcloud.io/docs/api/#historical-prices}
#' @seealso Investors Exchange `IEX` developer guide \url{https://iexcloud.io/docs/api/}
#' @seealso Investors Exchange Group (IEX Group) offers flexible and salable pricing.\url{https://iexcloud.io/pricing/}
#' @seealso View Investors Exchange Group (IEX Group) terms of use and subscription levels.\url{https://iexcloud.io/terms/}
#' @seealso Package `iexcloudR`\url{https://github.com/schardtbc/iexcloudR}
#' @author Myriam Ibrahim
#' @inheritParams iex.book
#' @return Data frame that includes stock (s) financial data
#' @examples
#' \donttest{
#'   iex.intraday ("TSLA", sk)
#' }
#' @export
iex.intraday <- function(x,iex_sk){
  url_prefix <- "https://cloud.iexapis.com/stable"
  url_stocks_list <- paste0(url_prefix, "/ref-data/symbols?token=",iex_sk)
  req <- httr::GET(url_stocks_list)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  stocks_list <- rjson::fromJSON(file=url_stocks_list)
  stocks_list <- sapply(stocks_list, "[[", "symbol")
  stocks_list <- as.list(stocks_list)
  if (toupper(x) %in% stocks_list == TRUE) print("Stock Info is available in IEX") else warning ("Stock Info is not available in IEX")
  x_encoded <- url_encode(paste(toupper(x), collapse=", "))
  url = paste0(url_prefix, "/stock/",x_encoded,"/intraday-prices?token=",iex_sk)
  url.info <- GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")) warning("No output to parse.") else stock.intraday <- rjson::fromJSON(url.content)
  assign(paste0(x, "_intraday"),stock.intraday)
  stock.intraday.df <- do.call(rbind, stock.intraday) %>%
    as.data.frame
  stock.intraday.df$date <- as.Date(as.character(stock.intraday.df$date), format='%Y-%m-%d')
  stock.intraday.df$datetime <- as.POSIXct(paste(stock.intraday.df$date, stock.intraday.df$minute), format="%Y-%m-%d %H:%M")
  stock.intraday.df <- stock.intraday.df[,c(which(colnames(stock.intraday.df)=="datetime"),which(colnames(stock.intraday.df)!="datetime"))]
  #stock.intraday.df <- stock.intraday.df %>% select(datetime, everything())
  stock.intraday.ts <- stock.intraday.df[, c("datetime", "high", "low", "average", "volume", "notional", "numberOfTrades")]
  stock.intraday.ts <- zoo(stock.intraday.ts[,-1],order.by=stock.intraday.df$datetime)
  return(stock.intraday.ts)

}

# --------------------- Company ---------------------#
#' Returns company's key info. For example: Industry, CEO, website, number of employees, ..etc.
#'
#' For more details, visit:\url{https://iexcloud.io/docs/api/#company}
#' @seealso Investors Exchange `IEX` developer guide \url{https://iexcloud.io/docs/api/}
#' @seealso Investors Exchange Group (IEX Group) offers flexible and salable pricing.\url{https://iexcloud.io/pricing/}
#' @seealso View Investors Exchange Group (IEX Group) terms of use and subscription levels.\url{https://iexcloud.io/terms/}
#' @seealso Package `iexcloudR`\url{https://github.com/schardtbc/iexcloudR}
#' @author Myriam Ibrahim
#' @inheritParams iex.book
#' @return data frame with company summary data
#' @examples
#' \donttest{
#'   iex.company("TSLA")
#' }
#' @export
iex.company <- function(x = 'TSLA', iex_sk){
  stock.company.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  url_stocks_list <- paste0(url_prefix, "/ref-data/symbols?token=",iex_sk)
  req <- httr::GET(url_stocks_list)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  stocks_list <- rjson::fromJSON(file=url_stocks_list)
  stocks_list <- sapply(stocks_list, "[[", "symbol")
  stocks_list <- as.list(stocks_list)
  if (toupper(x) %in% stocks_list == TRUE) print("Stock Info is available in IEX") else stop ("Stock Info is not available in IEX")
  x_encoded <- url_encode(paste(toupper(x), collapse=", "))
  url = paste0(url_prefix, "/stock/",x_encoded,"/company?token=",iex_sk)
  url.info <- httr::GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")){
    warning("No output to parse.")
  } else {
    stock.company <- rjson::fromJSON(url.content)
  }
  stock.company[lengths(stock.company) == 0] <- NA
  stock.company.df <- tibble::as_tibble(stock.company)
  stock.company.df <- stock.company.df %>%
                        dplyr::select(-("tags")) %>%
                        dplyr::distinct()
  return(stock.company.df)

}

# --------------------- Crypto --------------------- #
#' Returns quote for Crypto Currencies supported by the IEX Cloud API.
#'
#' For more details, visit:\url{https://iexcloud.io/docs/api/#crypto}
#' @seealso Investors Exchange `IEX` developer guide \url{https://iexcloud.io/docs/api/}
#' @seealso Investors Exchange Group (IEX Group) offers flexible and salable pricing.\url{https://iexcloud.io/pricing/}
#' @seealso View Investors Exchange Group (IEX Group) terms of use and subscription levels.\url{https://iexcloud.io/terms/}
#' @seealso Package `iexcloudR`\url{https://github.com/schardtbc/iexcloudR}
#' @author Myriam Ibrahim
#' @inheritParams iex.book
#' @return Data frame that include Crypto currency financial details
#' To retrieve list of 18 Cyrpto Currency Symbols available in IEX as of April 2019.
#' @examples
#' \donttest{
#'   data(package = "Riex")
#'   CrytoSymbols <- crypto_symbols
#' }
#' @export
crypto <- function(x, iex_sk){
  crypto.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  x_encoded <- url_encode(paste(toupper(x), collapse=", "))
  url = paste0(url_prefix, "/crypto/",x_encoded,"/quote?token=",iex_sk)
  req <- httr::GET(url)
  req_status <- req$status_code
  if (req_status != 200) {
  stop ("Connection Failed to IEX Cloud API")
  } else {
  url.info <- httr::GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  }
  if (identical(text, "")) warning("No output to parse.") else crypto <- rjson::fromJSON(url.content)
  crypto.df <- do.call(rbind, crypto) %>%
    as.data.frame

  colnames(crypto.df) = "Values"

  return(crypto.df)
}

# --------------------- Earnings ---------------------#
#' Earnings data for a given company including the actual EPS, consensus, and fiscal period.
#' Earnings are available quarterly (last 4 quarters)
#'
#' For more details, visit:\url{https://iexcloud.io/docs/api/#earnings}
#' @seealso Investors Exchange `IEX` developer guide \url{https://iexcloud.io/docs/api/}
#' @seealso Investors Exchange Group (IEX Group) offers flexible and salable pricing.\url{https://iexcloud.io/pricing/}
#' @seealso View Investors Exchange Group (IEX Group) terms of use and subscription levels.\url{https://iexcloud.io/terms/}
#' @seealso Package `iexcloudR`\url{https://github.com/schardtbc/iexcloudR}
#' @author Myriam Ibrahim
#' @inheritParams iex.book
#' @return Data frame that include earnings
#' @examples
#' \donttest{
#'   iex.earnings("TSLA")
#' }
#' @export
iex.earnings <- function(x, iex_sk){
  stock.earnings.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  url_stocks_list <- paste0(url_prefix, "/ref-data/symbols?token=",iex_sk)
  req <- httr::GET(url_stocks_list)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  stocks_list <- rjson::fromJSON(file=url_stocks_list)
  stocks_list <- sapply(stocks_list, "[[", "symbol")
  stocks_list <- as.list(stocks_list)
  if (toupper(x) %in% stocks_list == TRUE) print("Stock Info is available in IEX") else warning ("Stock Info is not available in IEX")
  x_encoded <- urltools::url_encode(paste(toupper(x), collapse=", "))
  url = paste0(url_prefix, "/stock/",x_encoded,"/earnings?token=",iex_sk)
  url.info <- httr::GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")){
    warning("No output to parse.")
    stock.earnings.df <- "Null"

  } else if (identical(url.content, "The requested data requires permission to access.")){
    message("The requested data requires permission to access.check IEX Subscriptions and Pricing @ 'https://www.iexcloud.io/pricing'")
    stock.earnings.df <- "Null"

  } else {
    stock.earnings <- rjson::fromJSON(url.content)
    stock.earnings.list <- lapply(rapply(stock.earnings, enquote, how="unlist"), eval)
    stock.earnings.df <- as.data.frame(stock.earnings.list)

  }

  return (stock.earnings.df)
}

#--------------------- Financials ---------------------#
#' `IEX` financials
#'
#' Returns income statement, balance sheet, and cash flow data from the most recent reported quarter.
#' For more details, visit:\url{https://iexcloud.io/docs/api/#financials}
#' @seealso Investors Exchange `IEX` developer guide \url{https://iexcloud.io/docs/api/}
#' @seealso Investors Exchange Group (IEX Group) offers flexible and salable pricing.\url{https://iexcloud.io/pricing/}
#' @seealso View Investors Exchange Group (IEX Group) terms of use and subscription levels.\url{https://iexcloud.io/terms/}
#' @seealso Package `iexcloudR`\url{https://github.com/schardtbc/iexcloudR}
#' @author Myriam Ibrahim
#' @inheritParams iex.book
#' @return Data frame with stock(s) financial data with opton of selecting quartlery or annually
#' @examples
#' \donttest{
#'   iex.cash.flow("TSLA", iex_sk)
#'  }
#' @name financials
NULL
#
#' @rdname financials
#' @export
iex.cash.flow <- function(x,iex_sk){
  stock.cash.flow.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  url_stocks_list <- paste0(url_prefix, "/ref-data/symbols?token=",iex_sk)
  req <- httr::GET(url_stocks_list)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  stocks_list <- rjson::fromJSON(file=url_stocks_list)
  stocks_list <- sapply(stocks_list, "[[", "symbol")
  stocks_list <- as.list(stocks_list)
  if (toupper(x) %in% stocks_list == TRUE) print("Stock Info is available in IEX") else warning ("Stock Info is not available in IEX")
  x_encoded <- urltools::url_encode(paste(toupper(x), collapse=", "))
  url = paste0(url_prefix, "/stock/",x_encoded,"/cash-flow?token=",iex_sk)
  url.info <- httr::GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")){
    warning("No output to parse.")
    stock.cash.flow.df <- "Null"

  } else if (identical(url.content, "The requested data is not available to free tier accounts. Please upgrade for access to this data.")){
    message("The requested data requires permission to access.check IEX Subscriptions and Pricing @ 'https://www.iexcloud.io/pricing'")
    stock.cash.flow.df <- "Null"

  } else {

    stock.cash.flow <- rjson::fromJSON(url.content)
    stock.cash.flow.list <- lapply(rapply(stock.cash.flow$cashflow, enquote, how="unlist"), eval)
    stock.cash.flow.df <- as.data.frame(stock.cash.flow.list)
    stock.cash.flow.df <- format(stock.cash.flow.df,  scientific=F)

  }

  return (stock.cash.flow.df)
}
#--------------------- Balance Sheet ---------------------#
#' @rdname financials
#' @export
iex.balance.sheet <- function(x,iex_sk){
  stock.balance.sheet.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  url_stocks_list <- paste0(url_prefix, "/ref-data/symbols?token=",iex_sk)
  req <- httr::GET(url_stocks_list)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  stocks_list <- rjson::fromJSON(file=url_stocks_list)
  stocks_list <- sapply(stocks_list, "[[", "symbol")
  stocks_list <- as.list(stocks_list)
  if (toupper(x) %in% stocks_list == TRUE) print("Stock Info is available in IEX") else warning ("Stock Info is not available in IEX")
  x_encoded <- urltools::url_encode(paste(toupper(x), collapse=", "))
  url = paste0(url_prefix, "/stock/",x_encoded,"/balance-sheet?token=",iex_sk)
  url.info <- httr::GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  #if (identical(text, "")) warning("No output to parse.")
  #else stock.balance.sheet <- rjson::fromJSON(url.content)
  if (identical(text, "")){
    warning("No output to parse.")
    stock.balance.sheet.df <- "Null"

  } else if (identical(url.content, "The requested data is not available to free tier accounts. Please upgrade for access to this data.")){
    message("The requested data requires permission to access.check IEX Subscriptions and Pricing @ 'https://www.iexcloud.io/pricing'")
    stock.balance.sheet.df <- "Null"

  } else {

    stock.balance.sheet <- rjson::fromJSON(url.content)
    stock.balance.sheet.list <- lapply(rapply(stock.balance.sheet$balancesheet, enquote, how="unlist"), eval)
    stock.balance.sheet.df <- as.data.frame(stock.balance.sheet.list)
    stock.balance.sheet.df <- format(stock.balance.sheet.df,  scientific=F)
  }

  return (stock.balance.sheet.df)
}
#--------------------- Income Statement ---------------------#
#' @rdname financials
#' @export
iex.income <- function(x,iex_sk){
  stock.income.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  url_stocks_list <- paste0(url_prefix, "/ref-data/symbols?token=",iex_sk)
  req <- httr::GET(url_stocks_list)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  stocks_list <- rjson::fromJSON(file=url_stocks_list)
  stocks_list <- sapply(stocks_list, "[[", "symbol")
  stocks_list <- as.list(stocks_list)
  if (toupper(x) %in% stocks_list == TRUE) print("Stock Info is available in IEX") else warning ("Stock Info is not available in IEX")
  x_encoded <- urltools::url_encode(paste(toupper(x), collapse=", "))
  url = paste0(url_prefix, "/stock/",x_encoded,"/income?token=",iex_sk)
  url.info <- httr::GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  #if (identical(text, "")) warning("No output to parse.")
  #else stock.income <- rjson::fromJSON(url.content)
  if (identical(text, "")){
    warning("No output to parse.")
    stock.income.df <- "Null"

  } else if (identical(url.content, "The requested data is not available to free tier accounts. Please upgrade for access to this data.")){
    message("The requested data requires permission to access.check IEX Subscriptions and Pricing @ 'https://www.iexcloud.io/pricing'")
    stock.income.df <- "Null"

  } else {
    stock.income <- rjson::fromJSON(url.content)
    stock.income.list <- lapply(rapply(stock.income$income, enquote, how="unlist"), eval)
    stock.income.df <- as.data.frame(stock.income.list)
    stock.income.df <- format(stock.income.df,  scientific=F)
  }

  return (stock.income.df)
}
#--------------------- Stats ---------------------#
#' Returns key stats per symbol - e.g. Market cap, 52 weeks high & low stock price
#'
#' For more details, visit:\url{https://iexcloud.io/docs/api/#key-stats}
#' @seealso Investors Exchange `IEX` developer guide \url{https://iexcloud.io/docs/api/}
#' @seealso Investors Exchange Group (IEX Group) offers flexible and salable pricing.\url{https://iexcloud.io/pricing/}
#' @seealso View Investors Exchange Group (IEX Group) terms of use and subscription levels.\url{https://iexcloud.io/terms/}
#' @seealso Package `iexcloudR`\url{https://github.com/schardtbc/iexcloudR}
#' @author Myriam Ibrahim
#' @inheritParams iex.book
#' @return Key  statistics
#' @examples
#' \donttest{
#'   iex.stats("TSLA")
#' }
#' @export
iex.stats <- function(x="GM", iex_sk){
  stock.stats.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  url_stocks_list <- paste0(url_prefix, "/ref-data/symbols?token=",iex_sk)
  req <- httr::GET(url_stocks_list)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  stocks_list <- rjson::fromJSON(file=url_stocks_list)
  stocks_list <- sapply(stocks_list, "[[", "symbol")
  stocks_list <- as.list(stocks_list)
  if (toupper(x) %in% stocks_list == TRUE) print("Stock Info is available in IEX") else warning ("Stock Info is not available in IEX")
  x_encoded <- url_encode(paste(toupper(x), collapse=", "))
  url = paste0(url_prefix, "/stock/",x_encoded,"/stats?token=",iex_sk)
  url.info <- httr::GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")) warning("No output to parse.") else stock.stats <- rjson::fromJSON(url.content)
  stock.stats.df <- do.call(rbind, stock.stats) %>%
    as.data.frame
  stock.stats.df<- cbind(Field = rownames(stock.stats.df), stock.stats.df)
  rownames(stock.stats.df) <- 1:nrow(stock.stats.df)
  names(stock.stats.df)[names(stock.stats.df) == 'V1'] <- 'Value'
return(stock.stats.df)

}

#--------------------- Lists - Most Active ---------------------#
#' Returns the quotes for the top 10 most active symbols. Data schedulued for intraday update.
#'
#' For more details, visit:\url{https://iexcloud.io/docs/api/#list}
#' @seealso Investors Exchange `IEX` developer guide \url{https://iexcloud.io/docs/api/}
#' @seealso Investors Exchange Group (IEX Group) offers flexible and salable pricing.\url{https://iexcloud.io/pricing/}
#' @seealso View Investors Exchange Group (IEX Group) terms of use and subscription levels.\url{https://iexcloud.io/terms/}
#' @seealso Package `iexcloudR`\url{https://github.com/schardtbc/iexcloudR}
#' @author Myriam Ibrahim
#' @param iex_sk is a character vector that include IEX Cloud API Secret Token
#' @return Most Active Stocks in IEX during the trading day
#' @export
iex.most.active <- function(iex_sk){
  most.active.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  url = paste0(url_prefix, "/stock/market/list/mostactive?token=",iex_sk)
  req <- httr::GET(url)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  url.info <- httr::GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")) warning("No output to parse.") else most_active <- rjson::fromJSON(url.content)
  most.active.df <- do.call(rbind, most_active) %>%
    as.data.frame
return(most.active.df)
}

# ---------------------- Logo ----------------------#
#' Returns url for company's logo based on Google standardized APIs.
#'
#' For more details, visit:\url{https://iexcloud.io/docs/api/#logo}
#' @seealso Investors Exchange `IEX` developer guide \url{https://iexcloud.io/docs/api/}
#' @seealso Investors Exchange Group (IEX Group) offers flexible and salable pricing.\url{https://iexcloud.io/pricing/}
#' @seealso View Investors Exchange Group (IEX Group) terms of use and subscription levels.\url{https://iexcloud.io/terms/}
#' @seealso Package `iexcloudR`\url{https://github.com/schardtbc/iexcloudR}
#' @author Myriam Ibrahim
#' @inheritParams iex.book
#' @return Company's Logo
#' @examples
#' \donttest{
#'   logo("GOOG")
#' }
#' @export
logo <- function(x, iex_sk){
  url_prefix <- "https://cloud.iexapis.com/stable"
  url_stocks_list <- paste0(url_prefix, "/ref-data/symbols?token=",iex_sk)
  req <- httr::GET(url_stocks_list)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  stocks_list <- rjson::fromJSON(file=url_stocks_list)
  stocks_list <- sapply(stocks_list, "[[", "symbol")
  stocks_list <- as.list(stocks_list)
  if (toupper(x) %in% stocks_list == TRUE) print("Stock Info is available in IEX") else warning ("Stock Info is not available in IEX")
  x_encoded <- url_encode(paste(toupper(x), collapse=", "))
  url = paste0(url_prefix, "/stock/",x_encoded,"/logo?token=",iex_sk)
  req <- httr::GET(url)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  url.info <- httr::GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")) warning("No output to parse.") else logo <- rjson::fromJSON(url.content)
return (logo$url)

}

#--------------------- Account and Usage ---------------------#
#' Account and usage details
#'
#' For more details, visit:\url{https://iexcloud.io/docs/api/#account}
#' @seealso Investors Exchange `IEX` developer guide \url{https://iexcloud.io/docs/api/}
#' @seealso Investors Exchange Group (IEX Group) offers flexible and salable pricing.\url{https://iexcloud.io/pricing/}
#' @seealso View Investors Exchange Group (IEX Group) terms of use and subscription levels.\url{https://iexcloud.io/terms/}
#' @seealso Package `iexcloudR`\url{https://github.com/schardtbc/iexcloudR}
#' @author Myriam Ibrahim
#' @param iex_sk is a character vector that include IEX Cloud API Secret Token
#' @return Most Active Stocks in IEX during the trading day
#' @export
iex.account <- function(iex_sk){
  iex.account.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  url = paste0(url_prefix, "/account/metadata?token=",iex_sk)
  req <- httr::GET(url)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  url.info <- httr::GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")) warning("No output to parse.") else iex_account <- rjson::fromJSON(url.content)
  iex.account.df <- do.call(rbind, iex_account) %>%
    as.data.frame
  iex.account.df<- cbind(Field = rownames(iex.account.df),  iex.account.df)
  rownames(iex.account.df) <- 1:nrow(iex.account.df)
  names(iex.account.df)[names(iex.account.df) == 'V1'] <- 'Value'
  iex.account.df <- format(iex.account.df,  scientific=F)
return( iex.account.df)

}

#--------------------- Usage ---------------------#
#' @param iex_sk is a character vector that include IEX Cloud API Secret Token
#' @rdname iex.account
#' @export
iex.usage <- function(iex_sk){
  iex.usage.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  url = paste0(url_prefix, "/account/usage?token=",iex_sk)
  req <- httr::GET(url)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  url.info <- httr::GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")) warning("No output to parse.") else iex_usage <- rjson::fromJSON(url.content)
  iex.usage.df <- do.call(rbind, iex_usage) %>%
    as.data.frame
  iex.usage.df<- cbind(Field = rownames(iex.usage.df),  iex.usage.df)
  rownames(iex.usage.df) <- 1:nrow(iex.usage.df)
  names(iex.usage.df)[names(iex.usage.df) == 'V1'] <- 'Value'
  iex.usage.df <- format(iex.usage.df,  scientific=F)
  iex.usage.df <- iex.usage.df[, !(colnames(iex.usage.df) %in% c("keyUsage", "dailyUsage")), drop = FALSE]
  return( iex.usage.df)

}

#--------------------- Key Usage ---------------------#
#' @param iex_sk is a character vector that include IEX Cloud API Secret Token
#' @rdname iex.account
#' @export
iex.key.usage<- function(iex_sk){
  iex.key.usage.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  url = paste0(url_prefix, "/account/usage?token=",iex_sk)
  req <- httr::GET(url)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  url.info <- httr::GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")) warning("No output to parse.") else iex_key_usage <- rjson::fromJSON(url.content)
  iex.key.usage.df <- do.call(rbind, iex_key_usage) %>%
    as.data.frame
  iex.key.usage.df <- lapply(rapply(iex.key.usage.df$keyUsage, enquote, how="unlist"), eval)
  iex.key.usage.df <- as.data.frame(iex.key.usage.df)
  iex.key.usage.df <- format(iex.key.usage.df,  scientific=F)
  return(iex.key.usage.df)

}

#--------------------- Daily Usage ----------------------#
#' @param iex_sk is a character vector that include IEX Cloud API Secret Token
#' @rdname iex.account
#' @export
iex.daily.usage<- function(iex_sk){
  iex.daily.usage.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  url = paste0(url_prefix, "/account/usage?token=",iex_sk)
  req <- httr::GET(url)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  url.info <- httr::GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")) warning("No output to parse.") else iex_daily_usage <- rjson::fromJSON(url.content)
  iex.daily.usage.df <- do.call(rbind, iex_daily_usage) %>%
    as.data.frame
  iex.daily.usage.df <- lapply(rapply(iex.daily.usage.df$dailyUsage, enquote, how="unlist"), eval)
  iex.daily.usage.df <- as.data.frame(iex.daily.usage.df)
  iex.daily.usage.df <- format(iex.daily.usage.df,  scientific=F)
  return(iex.daily.usage.df)

}

#--------------------- Monthly Usage ---------------------#
#' @param iex_sk is a character vector that include IEX Cloud API Secret Token
#' @rdname iex.account
#' @export
iex.monthly.usage<- function(iex_sk){
  iex.monthly.usage.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  url = paste0(url_prefix, "/account/usage?token=",iex_sk)
  req <- httr::GET(url)
  req_status <- req$status_code
  if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
  url.info <- httr::GET(url)
  url.content <- content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")) warning("No output to parse.") else iex_monthly_usage <- rjson::fromJSON(url.content)
  iex.monthly.usage.df <- do.call(rbind, iex_monthly_usage) %>%
    as.data.frame
  iex.monthly.usage.df <- lapply(rapply(iex.monthly.usage.df$monthlyUsage, enquote, how="unlist"), eval)
  iex.monthly.usage.df <- as.data.frame(iex.monthly.usage.df)
  iex.monthly.usage.df <- format(iex.monthly.usage.df,  scientific=F)
  colnames(iex.monthly.usage.df)[1] = "Monthly Usage"
  return(iex.monthly.usage.df)

}

#--------------------- Data File ---------------------#
#' `IEX` Crypto currency symbols
#'
#' A List of 18 Cyrpto Currency Symbols available via IEX API v1 as of April 26, 2019. List subject to update. Please visit IEX Site for most current list \url{https://iexcloud.io/docs/api/#reference-data}
#' IEX API version 1 will sunset non-Investors Exchange (IEX) data on June 1, 2019.
#'
#' @format A data frame with 18 rows and 1 variable:
#' \describe{
#'   \item{symbol}{symbol, in ticker format}
#'   ...
#' }
#' @source \url{https://iexcloud.io/docs/api/#cryptocurrency-symbols}
"crypto_symbols"


