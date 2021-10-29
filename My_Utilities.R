Print_The_Phrase <- function() {'hello world your utilities package is loaded'}


# Returns a bigz value of length n with random digits

random_bigz_lngth_n <- function(length = 100) {

  if(length < 3) {
    print('error - length must be greater than 2')
  }

  n <- length

  first_digit <- sample(1:9, 1) # first digit can't be zero so calc seperately

  last_digit <- sample(c(1,3,5,7,9), 1) # last digit can't be zero so calc seperately

  #loop to create subsequent digits

  mid_digits <- NULL

  for (i in 1:(n-2)) {
    mid_digits <- c(mid_digits, sample(0:9, 1))
  }

  mid_digits <- mid_digits %>%
    paste0(collapse='')

  random_ln <- first_digit %>%
    paste0(mid_digits, collapse = '') %>%
    paste0(last_digit, callapse = '') %>%
    as.bigz()

  random_ln}



# Returns a data table of Prime numbers of length n l long
# With how long it took to find them

prime_maker <- function(n, l= 10) {
  Primes_length_n <- data.table(Primes=NULL,
                                find_time_secs = NULL)
  found_primes = 0
  prime_start_find <- Sys.time()

  while(found_primes < l) {
    this_n <- random_bigz_lngth_n(n) %>%
      as.character()

    if(isprime(this_n) > 0) {
      print(paste0('Found ', length(Primes_length_n$Primes)))
      found_primes <- found_primes + 1
      this_ft_secs <- as.duration((Sys.time()-prime_start_find))%>%
        dseconds %>%
        as.numeric()

      Primes_length_n <- rbindlist(list(Primes_length_n,
                                        data.table(Primes = this_n,
                                                   find_time_secs = this_ft_secs)), use.names = TRUE)

      print(Primes_length_n %>%
              select(-Primes))

      prime_start_find <- Sys.time()
    }
  }
  Primes_length_n
}


# performs Excel Style rounding on a numeric vector

Excel_rounder <- function(x,
                          digits = 0,
                          keep_Excel = FALSE,
                          defensive_mode = TRUE){
  library(tidyverse)
  library(openxlsx)
  library(assertive)

  Excel_max_rows <- 1048576
  Excel_max_digits <- 15


  vector_length <- length(x)
  char_lengths <- nchar(x)

  assert_is_numeric(x)
  assert_is_vector(x)
  assert_all_are_in_closed_range(x, lower = -9.99999999999999E+307, upper = 9.99999999999999E+307)
  assert_all_are_less_than_or_equal_to(vector_length, Excel_max_rows)
  assert_all_are_less_than_or_equal_to(char_lengths, Excel_max_digits)

  wb <- createWorkbook()

  addWorksheet(wb, 'ExcelRound')

  writeData(wb, 'ExcelRound', x)

  writeData(wb, 'ExcelRound',
            round(x, digits),
            startCol = 2)

  writeFormula(wb, 'ExcelRound',
               paste0('=round(', x, ', ', digits, ')'),
               startCol = 3)

  saveWorkbook(wb, 'ExcelRounding.xlsx', overwrite = TRUE)

  wb_rounded <- loadWorkbook('ExcelRounding.xlsx')
}


#testing all tests take about 2 minutes on intel macbook pro 14 inch

  start_time <- Sys.time()
  # testing max rows
  Excel_rounder(1:1048576)
  Excel_rounder(-1048576:-1)
  Excel_rounder((1:1048576)+.5)
  Excel_rounder((-1048576:-1)-.5)

  # testing max rows and digits
  Excel_rounder(nchar(123456789123456:(123456789123456+1048575)+.5))
  # testing edge cases
  Excel_rounder(0)
  Excel_rounder(NA)
  Excel_rounder(NULL)
  Excel_rounder(NaN)
  Excel_rounder('a')

  # testing results match test data

  # timing of all tests
  print(start_time - Sys.time())




