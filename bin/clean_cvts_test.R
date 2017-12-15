### For testing, I need to clean the CVTS test locations for compatibility

library(dplyr)

test_data <- readRDS('../test/cvts_test.rds')


test_data$config_code <- test_data %>%
  transmute(config_code = plyr::mapvalues(REGION, c("Eastern Oregon",
                                            "California Mixed Conifer Group",
                                            "Western Oregon",
                                            "Western Washington",
                                            "Idaho and western Montana",
                                            "Southeast Alaska",
                                            "Eastern Washington",
                                            "Northern and Eastern Utah",
                                            "Plains States",
                                            "Southern and western Utah",
                                            "Western Wyoming and Western Colorado",
                                            "Northern New Mexico and Arizona",
                                            "Southern New Mexico and Arizona"),
                                  c("OR_E",
                                    "CA_MC",
                                    "OR_W",
                                    "WA_W",
                                    "ID_MTW",
                                    "AK_SECN",
                                    "WA_E",
                                    "UT_NE",
                                    "NCCS",
                                    "UT_SW",
                                    "CO_W_WY_W",
                                    "AZ_N_NM_N",
                                    "AZ_S_NM_S")))
## Save and check
head(test_data)
names(test_data)
test_data$config_code
test_data$cyl <- NULL
saveRDS(test_data, '../test/csvts_test.rds')

test_2 <- readRDS('../test/csvts_test.rds')
head(test_2)

