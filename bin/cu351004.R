"Western Oregon Occurrences"
CU000025 CU000077 CU000087 CU000102 CU015005 CU020001 CU064012 CU065002 
      15       10        9        2        1        2        1        2 
CU066001 CU081001 CU108003 CU119005 CU122011 CU133001 CU202020 CU263004 
       1        2        1        1        2        1        1        1 
CU264003 CU351004 CU475001 
       2       22        2 


CU351004

cu351004 <- function(dbh, ht) {
  if(ht < 18) {ht <- 18}
  z <- (ht + B25 - dbh / B26) / (ht - 4.5)
  z25 <- z^2.5

  f <- B6 * z25 + B7 * z25 * dbh/1000 + B8 * z25 * ht/1000
  + B9 * z25 * ht * dbh/100000 + B10 * z25 * ht**2/1000000
  + B11 * z25 * ht**0.5/1000 + B12 * z**4 * dbh/1000
  + B13 * z**4 * ht/1000 + B14 * z**33 * ht * dbh/1000000
  + B15 * z**33 * ht**0.5/10000 + B16 * z**41 * ht**2/10000000

  cvt <- B1 * dbh**2 * (ht - 4.5) * f
  cvts <- cvt / (B17 + B18 * B19**(dbh + B20))
  return(cvtS)
}


function(dbh, ht) {if(ht < 18) {ht <- 18}; z <- (ht + B25 - dbh / B26) / (ht - 4.5); z25 <- z^2.5; f <- B6 * z25 + B7 * z25 * dbh/1000 + B8 * z25 * ht/1000 + B9 * z25 * ht * dbh/100000 + B10 * z25 * ht**2/1000000 + B11 * z25 * ht**0.5/1000 + B12 * z**4 * dbh/1000 + B13 * z**4 * ht/1000 + B14 * z**33 * ht * dbh/1000000 + B15 * z**33 * ht**0.5/10000 + B16 * z**41 * ht**2/10000000; cvt <- B1 * dbh**2 * (ht - 4.5) * f; cvts <- cvt / (B17 + B18 * B19**(dbh + B20)) return(cvts)}


