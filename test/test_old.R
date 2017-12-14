## A script intended for testing CVTS against data computed with the FIA compiler
options(java.parameters = "-Xmx8g")
library(devtools)
library(xlsx)
library(dplyr)
load_all('../../forvol')

## Load cvts_test.xlsx
## raw_xlsx <- read.xlsx2('../forvol/test/cvts_test.xlsx', 3,
##                        colClasses= c(ID = 'numeric', AUTHOR = 'character', LOC = 'numeric',
##                                     SPCD = 'numeric',TREENO = 'numeric',DO_BH = 'numeric',
##                                     HT_TOT = 'numeric',OB_VOL = 'character',IB_VOL = 'numeric',
##                                     BK_VOL = 'character',TOP_HT = 'numeric',MEAN_SEG_LEN = 'numeric',
##                                     NPT = 'numeric',ORIGIN = 'numeric',ST_HT = 'numeric',TOP_DIA = 'numeric',
##                                     REGION = 'character', CV4_CRM = 'numeric',
##                                     VOLTSGRS = 'numeric', VOLCFGRS = 'numeric', VOLCSGRS = 'numeric',
##                                     VOLBFGRS = 'numeric', VOLBSGRS = 'numeric', DRYBIOT = 'numeric',
##                                     DRYBIOM = 'numeric'), stringsAsFactors=FALSE)
## saveRDS(raw_xlsx, file='../forvol/test/cvts_test.rds')

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








