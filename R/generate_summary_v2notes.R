#' Generate Summary
#'
#' @param df A dataframe produced by load_thermograms()
#' @param type The type of thermogram. Defaults to Plasma
#' @param sample_code TBD
#' @param temperatures The range of temperatures for the thermograms. Defaults to 45.0 - 90.0 by 0.1
#'
#' @return Returns a dataframe
#'
generate_summary <- function(df, sample_code, temperatures){
  # smoothing_factor <- 1
  #
  # peakf.low <- 20/smoothing_factor
  # peakf.high <- 150/smoothing_factor
  # peak1.low <- 150/smoothing_factor
  # peak1.high <- 220/smoothing_factor
  # peak2.low <- 220/smoothing_factor
  # peak2.high <- 280/smoothing_factor
  # peak3.low <- 280/smoothing_factor
  # peak3.high <- 330/smoothing_factor

  temp.data.load <- tlbparam::load_thermograms('data-raw/Mela_Publication_Data_RB.xlsx',
                                               sheet='DSC Data', temps = c(paste0('T', seq(45, 90, by=0.1))))
  df <- temp.data.load %>% select(-SampleCode)
  sample_code <- temp.data.load['SampleCode']
  temperatures <- seq(45, 90, 0.1)

  require(dplyr)

  # Construct empty dataframe to build collection of summary statistics
  df.stats <- data.frame(Sample=seq(1, nrow(df), by=1)) %>%
    dplyr::mutate(sample_code)

  # Calculate Mean of each row
  df.stats$mean <- apply(df, 1, mean)

  # Calcuate Median of each row
  df.stats$median <- apply(df, 1, median)

  # Calculate Standard Deviation of each row
  df.stats$sd <- apply(df, 1, sd)

  # Calculate the Max Height of each row
  df.stats$max <- apply(df, 1, max)

  # Calculate the Temperature at Max Height of each row
  df.stats$tmax <- temperatures[apply(df, 1, which.max)]

  # Calculate the Min Height of each row
  df.stats$min <- apply(df, 1, min)

  # Calculate the Temperature at Min Height of each row
  df.stats$tmin <- temperatures[apply(df, 1, which.min)]

  # Calculate Total Area of each row
  df.stats$tarea <- apply(df, 1, function(x) sum(x)*0.1)

  # Calculate Peak F
  # old
  # df.stats$peakf <- apply(df, 1, gen_peak, l.temp=peakf.low, h.temp=peakf.high) # dependency
  peakf.low <- which(temperatures == 47)
  peakf.high <- which(temperatures == 54)
  df.peakf <- df %>% select(peakf.low:peakf.high)
  df.stats$peakf <- apply(df.peakf, 1, max)
  df.stats$tpeakf <- temperatures[peakf.low:peakf.high][apply(df.peakf, 1, which.max)]

  # # Calculate Temperature at Peak F
  # for (i in 1:nrow(df)){df.stats$tpeakf[i] <-
  #   temperatures[match(df.stats$peakf[i], df[i:i,peakf.low:peakf.high])+peakf.low]}

  # Calculate Peak 1
  # old
  # df.stats$peak1 <- apply(df, 1, gen_peak, l.temp=peak1.low, h.temp=peak1.high) # dependency
  peak1.low <- which(temperatures == 60)
  peak1.high <- which(temperatures == 66)
  df.peak1 <- df %>% select(peak1.low:peak1.high)
  df.stats$peak1 <- apply(df.peak1, 1, max)
  df.stats$tpeak1 <- temperatures[apply(df.peak1, 1, which.max)]

  plot(x = seq(45, 90, 0.1), y = df[1,])

  # Calculate Temperature at Peak 1
  for (i in 1:nrow(df)){df.stats$tpeak1[i] <-
    temperatures[match(df.stats$peak1[i], df[i:i,peak1.low:peak1.high])+peak1.low]}

  # Calculate Peak 2
  df.stats$peak2 <- apply(df, 1, gen_peak, l.temp=peak2.low, h.temp=peak2.high)# dependency

  # Calculate Temperature at Peak 2
  for (i in 1:nrow(df)){df.stats$tpeak2[i] <-
    temperatures[match(df.stats$peak2[i], df[i:i,peak2.low:peak2.high])+peak2.low]}

  # Calculate Peak 3
  df.stats$peak3 <- apply(df, 1, gen_peak, l.temp=peak3.low, h.temp=peak3.high)# dependency

  # Calculate Temperature at Peak 3
  for (i in 1:nrow(df)){df.stats$tpeak3[i] <-
    temperatures[match(df.stats$peak3[i], df[i:i,peak3.low:peak3.high])+peak3.low]}

  # Calculate Ratio of Peak 1 and Peak 2
  df.stats$peak12ratio <- as.double(df.stats$peak1)/as.numeric(df.stats$peak2)

  # Calculate Temperature at First Moment
  df.stats$tfm <- apply(df, 1, function(x) sum(x*temperatures)/sum(x))

  # Calculate Width at Half Height
  df.stats$fwhm <- apply(df, 1, gen_fwhm, temperatures)# dependency

  # Calculate minimum between Peak 1 and Peak 2
  for(i in 1:nrow(df)){
    temp <- df %>% slice(i) %>% dplyr::select(paste0('T',df.stats$tpeak1[i]):paste0('T',df.stats$tpeak2[i]))
    df.stats$P1P2.trough[i] <- min(temp)
    df.stats$tP1P2.trough[i] <- seq(df.stats$tpeak1[i], df.stats$tpeak2[i], by = 0.1)[which.min(temp)]
  }

  return(df.stats)
}
