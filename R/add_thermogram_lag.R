#' Add Thermogram Lag
#'
#' @param df A dataframe produced by clean_thermograms()
#' @param column The column name containing Lag data. Defaults to Lag.
#' @param low_temp The low temperature of the desired analysis. Defaults to 45.0
#' @param high_temp The high temperature of the desired analysis. Defaults to 90.0
#'
#' @return Returns a dataframe
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#'
#' @examples
#' add_thermogram_lag(df)
add_thermogram_lag <- function(df, column='Lag', low_temp='T45', high_temp='T90'){
  freq <- numeric()
  acf.test <- apply(df %>% dplyr::select(low_temp:high_temp),
                    1, stats::acf, lag=ncol(df), plot=FALSE)
  for(j in 1:nrow(df)){
    freq[j] <- sum(acf.test[[j]]$acf < 0.1 & acf.test[[j]]$acf > -0.1)
  }
  df <- df %>%
    dplyr::mutate(!!column:=freq, .after='Type')
}
