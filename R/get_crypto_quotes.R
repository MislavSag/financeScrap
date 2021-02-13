#' Scrap crypto quotes data
#'
#' @param crypto_id Id of crypto asset
#' @param currency In which currency to show value of the crypto asset
#' @param interval Time interval; can be '1h' or '1d'
#' @param time_start date objecct, start of series
#' @param time_end date objecct, end of series
#' @return data.table of quotes and metadata
#' @export
get_crypto_quotes <- function(crypto_id = 1, currency = 'USD', interval = '1h',
                              time_start = Sys.Date() - 5, time_end = Sys.Date()) {
  # params
  url <- 'https://web-api.coinmarketcap.com/v1.1/cryptocurrency/quotes/historical'
  ua <- 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.150 Safari/537.36'

  # check if date interval is greater than 1 y. If yeas, make sequence
  if ((time_end - time_start) > 365) {
    time_start <- seq.Date(time_start, time_end, by = 200)
    time_end <- c(time_start[-1], time_end)
    time_start <- as.POSIXct(time_start)
    time_end <- as.POSIXct(time_end)
  }

  # requests
  market_data <- lapply(seq_along(time_start), function(x) {
    crypto_quotes <- httr::GET(
      url,
      query = list(
        convert = currency,
        format = 'chart_crypto_details',
        id = crypto_id,
        interval = interval,
        time_end = time_end[x],
        time_start = time_start[x]
      ),
      encode = 'json',
      httr::user_agent(ua)
    )
    con <- httr::content(crypto_quotes)
    if (length(con$data > 0) && length(con$data) != 6) {
      # convert to tidy DT
      market_data <- lapply(con$data, data.table::rbindlist)
      market_data <- data.table::rbindlist(market_data, idcol = TRUE)
      data.table::setnames(market_data,
                           colnames(market_data),
                           c('datetime', 'price', 'vol', 'market_cap'))
      market_data$crypto_id <- crypto_id
      market_data$currency <- currency
    } else {
      print(paste0('Skip some data for id ', crypto_id))
      return(NULL)
    }
    market_data
  })
  return(data.table::rbindlist(market_data))
}
