#' Clean Thermograms
#'
#' @param df A dataframe produced by load_thermograms()
#' @param type The type of thermogram. Defaults to Plasma
#' @param column The column name containing patient/thermogram identifier. Defaults to SampleCode.
#' @param low_temp The low temperature of the desired analysis. Defaults to 45.0
#' @param high_temp The high temperature of the desired analysis. Defaults to 90.0
#' @param temp_range The range of temperature and step
#' @param summary A vector containing desired summary statistics, defaults to NULL
#' @param lag A TRUE or FALSE to include lag information, defaults to FALSE
#' @return Returns a dataframe
#' @export
#' @importFrom magrittr %>%

# removed by Avery:
# @param patient A vector containing desired patient information, defaults to NULL
# patient=NULL; parameter from function(...)
# related code in function is commented out

# @param pc A vector containing desired principal components (example: 1:6), defaults to NULL
# pc=c(1:6); parameter from function(...)
# related code in function is commented out


clean_thermograms <- function(df, type='Plasma', column='SampleCode', low_temp='T45', high_temp='T90', temp_range=seq(45.0, 90.0, by=0.1),
                              summary=c('Width','Area','Max','TMax','TFM','Peak 1','Peak 2','Peak 3','TPeak 1','TPeak 2',
                                        'TPeak 3','Peak 1 / Peak 2'),
                              lag=FALSE){
  df <- tibble::as_tibble(df) %>%
    dplyr::select(!!column, low_temp:high_temp) %>%
    dplyr::mutate(Type = type, .after = !!column) %>%
    dplyr::mutate(Patient = stringr::str_extract(!!rlang::sym(column), '^[^_]+(?=_)'), .before=!!column) %>%
    tibble::rowid_to_column('Index')

  if(!is.null(summary)){
    df.stats0 <- generate_summary(df %>% dplyr::select(low_temp:high_temp),
                                  df['Type'],
                                  df['SampleCode'],
                                  temp_range)
    for(i in 1:length(summary)){
      # print(summary[i])
      df <- add_summary_column(df, df.stats0, summary[i])
    }
  }

  # if(!is.null(patient)){
  #   for(i in 1:length(patient)){
  #     # print(patient[i])
  #     df <- add_patient_column(df, orig_patient, patient[i])
  #   }
  # }

  # if(!is.null(pc)){
  #   for(i in 1:length(pc)){
  #     # print(pc[i])
  #   }
  # }

  if(lag){
    # print(paste('Lag:', lag))
    df <- add_thermogram_lag(df)
  }

  return(df)
}
