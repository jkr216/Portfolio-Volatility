
# Calculate component returns

componentReturns_df <- function(stock1, stock2, stock3, stock4, stock5, start_date){
  
  symbols <- c(stock1, stock2, stock3, stock4, stock5)
  
  prices <- 
    getSymbols(symbols, src = 'yahoo', from = start_date, 
               auto.assign = TRUE, warnings = FALSE) %>% 
    map(~Cl(get(.))) %>% 
    reduce(merge) %>%
    `colnames<-`(symbols)
  
  # generate daily return series for funds
  prices_monthly <- to.monthly(prices, indexAt = "first", OHLC = FALSE)
  returns <- na.omit(Return.calculate(prices_monthly, method = "log"))
  
  
  returns_df <- returns %>% 
    tk_tbl(preserve_index = TRUE) %>% 
    mutate(date = ymd(index)) %>% 
    select(-index) %>% 
    select(date, everything())
}


# Calculate rolling Portfolio Standard Deviation

rolling_portfolio_sd <- function(returns_df, start = 1, window = 6, weights){
  
  start_date <- returns_df$date[start]
  
  end_date <-  returns_df$date[c(start + window)]
  
  interval_to_use <- returns_df %>% filter(date >= start_date & date < end_date)
  
  returns_xts <- interval_to_use %>% tk_xts(date_var = date) 
  
  w <- weights
  
  results_as_xts <- StdDev(returns_xts, weights = w, portfolio_method = "single")
  results_as_xts <- round(results_as_xts, 4) * 100
  
  results_to_tibble <- tk_tbl(t(results_as_xts[,1])) %>% 
    mutate(date = ymd(end_date)) %>% 
    select(date, everything()) 
  
}