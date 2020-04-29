
# functions to be used in the app

# Calculate component returns

componentReturns_df <- function(stock1, stock2, stock3, start_date){
  
  symbols <- c(stock1, stock2, stock3)
  
  prices <- 
    getSymbols(symbols, src = 'yahoo', from = start_date, 
               auto.assign = TRUE, warnings = FALSE) %>% 
    map(~Cl(get(.))) %>% 
    reduce(merge) %>%
    `colnames<-`(symbols)
  
  # generate return series for funds
  prices_monthly <- to.monthly(prices, indexAt = "first", OHLC = FALSE)
  #returns <- na.omit(ROC(prices_monthly, 1, type = "continuous"))
  
  #prices_monthly <- to.monthly(prices, indexAt = "first", OHLC = FALSE)
  
  returns <- na.omit(Return.calculate(prices_monthly, method = "log"))
  
  
  returns_df <- returns %>% 
    tk_tbl(preserve_index =  TRUE) %>% 
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
  results_as_xts <- round(results_as_xts, 4)
  
  results_to_tibble <- tk_tbl(t(results_as_xts[,1])) %>% 
    mutate(date = ymd(end_date)) %>% 
    select(date, everything()) 
  
}

# Calculate rolling portfolio component contributions to standard deviation.

my_interval_sd <- function(returns_df, start = 1, weights, window = 20){
  
  # First create start date
  start_date = returns_df$date[start]
  
  # Next an end date
  end_date = returns_df$date[c(start + window)]
  
  # Filter on start and end date
  interval_to_use <- returns_df %>% filter(date >= start_date & date < end_date)
  
  # Convert to xts so can use built in Performance Analytics function.
  returns_xts <- interval_to_use %>% tk_xts(date_var = date) 
  
  # Portfolio weights.
  w <- weights
  
  # Pass xts object to function.
  results_as_xts <- StdDev(returns_xts, weights = w, portfolio_method = "component")
  
  # Convert results to tibble.
  results_to_tibble <- tk_tbl(t(results_as_xts$pct_contrib_StdDev)) %>% 
    mutate(date = ymd(end_date)) %>%
    mutate_if(is.numeric, function(x) x * 100) %>% 
    select(date, everything()) 
}


