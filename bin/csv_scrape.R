### Scrapes an input xlsx workbook of all subsheets and converts to csv
options(java.parameters = "-Xmx8000m")
library('xlsx')


CSVScrape <- function(workbook.path, dest.path) {
  ## Get number of sheets
  sheets <- getSheets(loadWorkbook(workbook.path))
  num_sheets <- length(sheets)

  ## Read the sheet and output as csv
  for ( i in 1:num_sheets) {
    gc()
    sheet_i <- read.xlsx(workbook.path, i)
    write.csv(sheet_i, file=file.path(dest.path, names(sheets)[i]), row.names = FALSE)
    #print(names(sheets)[i])
    #print(file.path(dest.path))
  }
}

CSVScrape('../excel/volcfgrs_eqn_coefs.xlsx', '../excel/test')




