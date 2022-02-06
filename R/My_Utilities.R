
Print_The_Phrase <- function() {'hello world'}

# Returns a bigz value of length n with random digits

random_bigz_lngth_n <- function(length = 100) {

  library(gmp, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE)
  library(assertive, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE)
  library(tidyverse, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE)
  library(openxlsx, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE)

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

  library(gmp, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE)
  library(assertive, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE)
  library(tidyverse, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE)
  library(openxlsx, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE)
  library(lubridate, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE)
  library(data.table, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE)

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

# performs Excel rounding on a numeric vector
# limited to 100,000 rows at a time.
# decimal places must be between + and minus 6
# digits must have max charachter length of 12

Excel_rounder <- function(x,
                          digits = 0,
                          keep_files = FALSE,
                          defensive_mode = TRUE){
  library(tidyverse, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE)
  library(openxlsx, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE)
  library(assertive, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE)

  Excel_max_rows <- 100000
  max_digits <- 12

  vector_length <- length(x)
  char_lengths <- nchar(x)

  assert_all_are_in_closed_range(digits, lower = -6, upper = 6)
  assert_is_numeric(x)
  assert_is_vector(x)
  assert_all_are_in_closed_range(x, lower = -9.99999999999999E+307,
                                 upper = 9.99999999999999E+307)
  assert_all_are_less_than_or_equal_to(vector_length, Excel_max_rows)
  assert_all_are_less_than_or_equal_to(char_lengths, max_digits)

  wb <- createWorkbook()

  addWorksheet(wb, 'ExcelRound')

  excel_formulas <-  paste0('round(', x, ', ', digits, ')')

  writeFormula(wb, 'ExcelRound',
               excel_formulas,
               startCol = 1)

  #writeData(wb, 'ExcelRound', 'ExcelRounded', 'A1')

  writeData(wb, 'ExcelRound',
            round(x, digits = digits),
            startCol = 2)

  writeData(wb, 'ExcelRound',
            x,
            startCol = 3)

  writeData(wb, 'ExcelRound', x, startCol = 3)

  openxlsx::saveWorkbook(wb, 'ExcelRounding.xlsx', overwrite = TRUE)

  ##### I hope to remove this dependancy on XLConnect

  ### XLConnect is the only way I can write the result of the formula rather
  ### Than the text of the formula at this stage, but it slows things down
  ### and relies on JAVA
  gc(verbose = FALSE, full = TRUE)
  options(java.parameters = "-Xmx6g" )
  suppressMessages(library(XLConnect, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE))
  options(java.parameters = "-Xmx6g" )
  xlcFreeMemory()
  gc(verbose = FALSE, full = TRUE)
  xlcwb <- XLConnect::loadWorkbook('ExcelRounding.xlsx')
  data <- XLConnect::readWorksheet(xlcwb, sheet = 'ExcelRound', header = FALSE)
  gc(verbose = FALSE, full = TRUE)
  detach('package:XLConnect', unload = TRUE)
  gc(verbose = FALSE, full = TRUE)

  ##### End dependency

  output <- data[, 1]

  if (keep_files == FALSE) {file.remove('ExcelRounding.xlsx')}

  output

}
