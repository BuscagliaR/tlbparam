#' Load Patient Data
#'
#' @param file The path and file name of a correctly formatted .xlsx or.csv file
#' @param sheet The Sheet Name in the Excel file. Defaults to Sheet1
#'
#' @return Returns a dataframe
#' @importFrom magrittr %>%
#'
#' @examples
#' load_patientdata(file='../GenSumv21/data-raw/patient_urine_y4.xlsx')
load_patientdata <- function(file, sheet='Sheet1'){
  suffix <- sapply(stringr::str_split(sapply(stringr::str_split(file, pattern='/'), utils::tail, 1), pattern='\\.'), utils::tail, 1)
  if (suffix == 'xlsx'){
    df <-
      as.data.frame(readxl::read_excel(file,
                                       sheet=sheet))
  }else if (suffix == 'csv'){
    df <-
      as.data.frame(readr::read_csv(file,
                                    col_names=TRUE,
                                    col_types = readr::cols(.default = readr::col_guess())))
  }
  return(df)
}
