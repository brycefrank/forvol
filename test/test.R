## A script intended for testing CVTS against data computed with the FIA compiler
options(java.parameters = "-Xmx8g")
library(devtools)
library(xlsx)
library(dplyr)
load_all('../forvol')


# Adjust some column names for consistency
test_data <- readRDS('../forvol/test/cvts_test.rds')
test_data <- mutate(test_data, CVTS = VOLTSGRS)


test_cvts <- function(spcd, region_string, new_cvts_func) {
  ## Tests the new cvts function on the test data
  ## Returns the average difference

  test_df <- filter(test_data, SPCD == spcd, REGION == region_string)

  dbh <- test_df$DO_BH
  ht <- test_df$HT_TOT
  return(mean(test_df$CVTS - eval(new_cvts_func)))
}

## Example test:
##df_cvts <- BuildEquation('CU202020', GetCoefs('OR_W', 202))

##test_cvts(202, 'Western Oregon', df_cvts)

wh_cvts <- BuildEquation('CU263004', GetCoefs('OR_W', 263))
test_cvts(263, 'Western Oregon', wh_cvts)





