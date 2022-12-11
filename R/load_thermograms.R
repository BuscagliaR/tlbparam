#' Load Thermograms into a Dataframe
#'
#' @param file The path and file name of a correctly formatted .xlsx or.csv file
#' @param sheet The Sheet Name in the Excel file. Defaults to Sheet1
#' @param blank_row Number of blank rows due to transpose. Defaults to 1.
#' @param temps The range of temperatures for the thermograms. Defaults to 21.0 - 110.0 by 0.1
#'
#' @return Returns a dataframe
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' load_thermograms(file='../GenSumv21/data-raw/plasma_thermo_y3.xlsx')
#' (c(paste0('T', seq(45.0, 90.0, by=0.1)), 'binary'))
load_thermograms <- function(file, sheet='Sheet1', blank_row=1, temps=c(paste0('T', seq(21.0, 110.0, by=0.1)))){

  suffix <- sapply(stringr::str_split(sapply(stringr::str_split(file, pattern='/'), utils::tail, 1), pattern='\\.'), utils::tail, 1)

  if (suffix == 'xlsx'){
    df <-
      as.data.frame(t(readxl::read_excel(file,
                                         sheet=sheet,
                                         col_types='numeric')))
  }else if (suffix == 'csv'){
    df <-
      as.data.frame(t(readr::read_csv(file,
                                      col_names=TRUE,
                                      col_types = readr::cols(.default = readr::col_double()))))
  }
  df <- df %>%
    `colnames<-`(temps) %>%
    dplyr::slice((blank_row+1):dplyr::n()) %>%
    tibble::rownames_to_column('SampleCode')
  return(df)
}
