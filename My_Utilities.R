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



# performs Excel Style rounding on a numeric vector
# limited to 100,000 rows at a time.

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

  ##### I hope to remove this dependancy on XLConnect -----

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

  ##### End dependency -----

  output <- data[, 1]

  if (keep_files == FALSE) {file.remove('ExcelRounding.xlsx')}

  output

}

#testing all tests take about 1 minute on intel macbook pro 14 inch

start_time <- Sys.time()
# testing max rows
t1_result <- Excel_rounder(1:100000)
t2_result <-Excel_rounder(-100000:-1)
t3_result <- Excel_rounder((1:100000)+.5)
t4_result <- Excel_rounder((-100000:-1)-.5)

# testing max rows and digits
t5_result <- Excel_rounder(6789123456:(6789123456+100000)+.5)
# testing edge cases
t6_result <- Excel_rounder(0)
t7_result <- Excel_rounder(NA)
t8_result <- Excel_rounder(NULL)
t9_result <- Excel_rounder(NaN)
t10_result <- Excel_rounder('a')

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
                        ex_ouptut = c(6789012346,
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

t11_result <- Excel_rounder(test_data$input)

test_data$ExcelRound <- t11_result

test_data$pass <- test_data$ex_ouptut == test_data$ExcelRound

# timing of all tests
print(Sys.time() - start_time)

