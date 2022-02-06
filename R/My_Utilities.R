Print_The_Phrase <- function() {'hello world'}

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


# Testing Excel rounder ----
# This code uses the Excel rounding function by writing the Excel round function
# To an Excel sheet with XLconnett
# this depends on Java is hacky and slow and has memory limits that mean it won't
# reliably work for more than 100,000 numbers at a time
# but it could have its uses,

start_time <- Sys.time()
# testing max rows
t1_result <- Excel_rounder(1:100000)
t2_result <-Excel_rounder(-100000:-1)
t3_result <- Excel_rounder((1:100000)+.5)
t4_result <- Excel_rounder((-100000:-1)-.5)

# testing max rows and digits
t5_result <- Excel_rounder(6789123456:(6789123456+100000-1)+.5)
# testing edge cases
t6_result <- Excel_rounder(0)
t7_result <- Excel_rounder(NA) # Should error
t8_result <- Excel_rounder(NULL) # Should error
t9_result <- Excel_rounder(NaN) # Should error
t10_result <- Excel_rounder('a') # Should Error
t11_result <- Excel_rounder(123.4567891, 7) # Should Error

# testing results match test data

test_data <- data.frame(input = c(6789012345.5,
                                  46.5,
                                  -5,
                                  -4.5,
                                  -4,
                                  3.5,
                                  -2,
                                  -2.5,
                                  0,
                                  2.5,
                                  3.5,
                                  4.5,
                                  5,
                                  2.500000),
                        ex_output = c(6789012346,
                                      47,
                                      -5,
                                      -5,
                                      -4,
                                      4,
                                      -2,
                                      -3,
                                      0,
                                      3,
                                      4,
                                      5,
                                      5,
                                      3))

test_data$rOutput <- round(test_data$input)

test_data$round_issue <- test_data$rOutput != test_data$ex_output

test_data$round_even <- round(test_data$input, digits) %>% `%%` (2) == 0

test_data$trunc_even <- trunc(test_data$input, digits) %>% `%%`(2) == 0

test_data$is_odd <-  test_data$input %>% `%%`(2) != 0

test_data$fix_required <- test_data$round_even == TRUE &
                          test_data$trunc_even == TRUE &
                          test_data$is_odd == TRUE

t12_result <- Excel_rounder(test_data$input)

test_data$ExcelRound <- t12_result

random_test_data <- paste0(sample(0:10, size = 100000, replace = TRUE), '.',
                           sample(0:100000000-1, size = 100000, replace = TRUE)) %>%
  as.numeric() * sample(c(-1, 1), size = 100000, replace = TRUE)

Exceptions <- data.frame(Digit = NULL,
                         Number = NULL,
                         R_rounded = NULL,
                         Excel_rounded = NULL)

for (digit in -6:6) {

  print(paste0('Testing Digit: ', digit))

  R_rounded <- round(random_test_data, digits = digit)

  Excel_rounded <-  Excel_rounder(random_test_data, digit)

for(i in 1:length(R_rounded)) {
    if(R_rounded[i] != Excel_rounded[i]) {

      Exceptions <- rbind(Exceptions,
                          data.frame(Digit = digit,
                                     Number = random_test_data[i],
                                     R_Rounded = R_rounded[i],
                                     Excel_rounded = Excel_rounded[i]))

      print(paste0("Digit ", digit))
      print(paste0("Full Number" , random_test_data[i]))
      print(paste0("R_rounded ", R_rounded[i]))
      print(paste0("Excel_rounded ", Excel_rounded[i]))}
  }
  }

# timing of all tests
print(Sys.time() - start_time)
