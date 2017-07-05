# test a function to load

summy <- function(x, y){
  summy <- x + y
}

# functions to be used downstream

componentReturns_df <- function(stock1, stock2, stock3, start_date = "2017-01-01"){
  
  symbols <- c(stock1, stock2, stock3)
  
  prices <- 
    getSymbols(symbols, src = 'google', from = start_date, 
               auto.assign = TRUE, warnings = FALSE) %>% 
    map(~Cl(get(.))) %>% 
    reduce(merge) %>%
    `colnames<-`(symbols)
  
  # generate daily return series for funds
  returns <-na.omit(ROC(prices, 1, "continuous"))
  
  returns_df <- returns %>% 
    as_tibble(preserve_row_names = TRUE) %>% 
    mutate(date = ymd(row.names)) %>% 
    select(-row.names) %>% 
    select(date, everything())
}

my_interval_sd <- function(returns_df, start = 1, weights, window = 20){
  
  # First create start date
  start_date = returns_df$date[start]
  
  # Next an end date
  end_date = returns_df$date[c(start + window)]
  
  # Filter on start and end date
  interval_to_use <- returns_df %>% filter(date >= start_date & date < end_date)
  
  # Convert to xts so can use built in Performance Analytics function.
  returns_xts <- interval_to_use %>% as_xts(date_col = date) 
  
  # Portfolio weights.
  w <- weights
  
  # Pass xts object to function.
  results_as_xts <- StdDev(returns_xts, weights = w, portfolio_method = "component")
  
  # Convert results to tibble.
  results_to_tibble <- as_tibble(t(results_as_xts$pct_contrib_StdDev)) %>% 
    mutate(date = ymd(end_date)) %>% 
    select(date, everything()) 
}
