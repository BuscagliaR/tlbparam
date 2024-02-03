#' Generate Summary
#'
#' @param df A dataframe produced by load_thermograms()
#' @param type The type of thermogram. Defaults to Plasma
#' @param sample_code TBD
#' @param temperatures The range of temperatures for the thermograms. Defaults to 45.0 - 90.0 by 0.1
#' @export
#' @return Returns a dataframe
#'
generate_summary <- function(df, type, sample_code, temperatures){
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
  #
  # # Construct empty dataframe to build collection of summary statistics
  # df.stats <- data.frame(Sample=seq(1, nrow(df), by=1)) %>%
  #   dplyr::mutate(sample_code, type)
  #
  # # Calculate Mean of each row
  # df.stats$mean <- apply(df, 1, mean)
  #
  # # Calcuate Median of each row
  # df.stats$median <- apply(df, 1, median)
  #
  # # Calculate Standard Deviation of each row
  # df.stats$sd <- apply(df, 1, sd)
  #
  # # Calculate the Max Height of each row
  # df.stats$max <- apply(df, 1, max)
  #
  # # Calculate the Temperature at Max Height of each row
  # df.stats$tmax <- temperatures[apply(df, 1, which.max)]
  #
  # # Calculate the Min Height of each row
  # df.stats$min <- apply(df, 1, min)
  #
  # # Calculate the Temperature at Min Height of each row
  # df.stats$tmin <- temperatures[apply(df, 1, which.min)]
  #
  # # Calculate Total Area of each row
  # df.stats$tarea <- apply(df, 1, function(x) sum(x)*0.1)
  #
  # # Calculate Peak F
  # df.stats$peakf <- apply(df, 1, gen_peak, l.temp=peakf.low, h.temp=peakf.high) # dependency
  #
  # # Calculate Temperature at Peak F
  # for (i in 1:nrow(df)){df.stats$tpeakf[i] <-
  #   temperatures[match(df.stats$peakf[i], df[i:i,peakf.low:peakf.high])+peakf.low]}
  #
  # # Calculate Peak 1
  # df.stats$peak1 <- apply(df, 1, gen_peak, l.temp=peak1.low, h.temp=peak1.high) # dependency
  #
  # # Calculate Temperature at Peak 1
  # for (i in 1:nrow(df)){df.stats$tpeak1[i] <-
  #   temperatures[match(df.stats$peak1[i], df[i:i,peak1.low:peak1.high])+peak1.low]}
  #
  # # Calculate Peak 2
  # df.stats$peak2 <- apply(df, 1, gen_peak, l.temp=peak2.low, h.temp=peak2.high)# dependency
  #
  # # Calculate Temperature at Peak 2
  # for (i in 1:nrow(df)){df.stats$tpeak2[i] <-
  #   temperatures[match(df.stats$peak2[i], df[i:i,peak2.low:peak2.high])+peak2.low]}
  #
  # # Calculate Peak 3
  # df.stats$peak3 <- apply(df, 1, gen_peak, l.temp=peak3.low, h.temp=peak3.high)# dependency
  #
  # # Calculate Temperature at Peak 3
  # for (i in 1:nrow(df)){df.stats$tpeak3[i] <-
  #   temperatures[match(df.stats$peak3[i], df[i:i,peak3.low:peak3.high])+peak3.low]}
  #
  # # Calculate Ratio of Peak 1 and Peak 2
  # df.stats$peak12ratio <- as.double(df.stats$peak1)/as.numeric(df.stats$peak2)
  #
  # # Calculate Temperature at First Moment
  # df.stats$tfm <- apply(df, 1, function(x) sum(x*temperatures)/sum(x))
  #
  # # Calculate Width at Half Height
  # df.stats$fwhm <- apply(df, 1, gen_fwhm, temperatures)# dependency
  #
  # # Calculate minimum between Peak 1 and Peak 2
  # for(i in 1:nrow(df)){
  #   temp <- df %>% slice(i) %>% dplyr::select(paste0('T',df.stats$tpeak1[i]):paste0('T',df.stats$tpeak2[i]))
  #   df.stats$P1P2.trough[i] <- min(temp)
  #   df.stats$tP1P2.trough[i] <- seq(df.stats$tpeak1[i], df.stats$tpeak2[i], by = 0.1)[which.min(temp)]
  # }

  require(dplyr)

  # Construct empty dataframe to build collection of summary statistics
  df.stats <- data.frame(Sample=seq(1, nrow(df), by=1)) %>%
    dplyr::mutate(sample_code)

  # Calculate Width at Half Height
  df.stats$Width <- apply(df, 1, gen_fwhm, temperatures)# dependency

  # Calculate Total Area of each row
  df.stats$Area <- apply(df, 1, function(x) sum(x)*0.1)

  # Calculate the Max Height of each row
  df.stats$Max <- apply(df, 1, max)

  # Calculate the Temperature at Max Height of each row
  df.stats$TMax <- temperatures[apply(df, 1, which.max)]

  # Calculate Temperature at First Moment
  df.stats$TFM <- apply(df, 1, function(x) sum(x*temperatures)/sum(x))

  # Calculate Peak 1
  # old
  # df.stats$peak1 <- apply(df, 1, gen_peak, l.temp=peak1.low, h.temp=peak1.high) # dependency
  peak1.low <- which(temperatures == 60)
  peak1.high <- which(temperatures == 66)
  df.peak1 <- df %>% dplyr::select(peak1.low:peak1.high)
  df.stats$`Peak 1` <- apply(df.peak1, 1, max)

  peak2.low <- which(temperatures == 67)
  peak2.high <- which(temperatures == 73)
  df.peak2 <- df %>% select(peak2.low:peak2.high)
  df.stats$`Peak 2` <- apply(df.peak2, 1, max)

  peak3.low <- which(temperatures == 73)
  peak3.high <- which(temperatures == 81)
  df.peak3 <- df %>% dplyr::select(peak3.low:peak3.high)
  df.stats$`Peak 3` <- apply(df.peak3, 1, max)


  df.stats$`TPeak 1` <- temperatures[peak1.low:peak1.high][apply(df.peak1, 1, which.max)]
  df.stats$`TPeak 2` <- temperatures[peak2.low:peak2.high][apply(df.peak2, 1, which.max)]
  df.stats$`TPeak 3` <- temperatures[peak3.low:peak3.high][apply(df.peak3, 1, which.max)]

  # Calculate Ratio of Peak 1 and Peak 2
  df.stats$`Peak 1 / Peak 2` <- as.double(df.stats$`Peak 1`)/as.numeric(df.stats$`Peak 2`)

  # Calculate Ratio of Peak 1 and Peak 3
  df.stats$`Peak 1 / Peak 3` <- as.double(df.stats$`Peak 1`)/as.numeric(df.stats$`Peak 3`)

  # Calculate Ratio of Peak 2 and Peak 3
  df.stats$`Peak 2 / Peak 3` <- as.double(df.stats$`Peak 2`)/as.numeric(df.stats$`Peak 3`)

  # Calculate Median of each row
  df.stats$Median <- apply(df, 1, median)

  # Calculate minimum between Peak 1 and Peak 2
  for(i in 1:nrow(df)){


    tpeak_1_temp_index <- which(temperatures == df.stats$`TPeak 1`[i])
    tpeak_2_temp_index <- which(temperatures == df.stats$`TPeak 2`[i])

    tpeak_1_2_indices <- seq(tpeak_1_temp_index, tpeak_2_temp_index, 1)

    temp <- df[i, tpeak_1_2_indices]

    df.stats$V1.2[i] <- min(temp)
    df.stats$TV1.2[i] <-temperatures[tpeak_1_2_indices][which.min(temp)]
  }

  df.stats$`V1.2 / Peak 1` =  df.stats$V1.2 / df.stats$`Peak 1`

  df.stats$`V1.2 / Peak 2` =  df.stats$V1.2/ df.stats$`Peak 2`

  df.stats$`V1.2 / Peak 3` =  df.stats$V1.2/ df.stats$`Peak 3`

  # Calculate the Min Height of each row
  df.stats$Min <- apply(df, 1, min)

  # Calculate the Temperature at Min Height of each row
  df.stats$TMin <- temperatures[apply(df, 1, which.min)]

  peakf.low <- which(temperatures == 50)
  peakf.high <- which(temperatures == 54)
  df.peakf <- df %>% dplyr::select(peakf.low:peakf.high)
  df.stats$`Peak F` <- apply(df.peakf, 1, max)
  df.stats$`TPeak F` <- temperatures[peakf.low:peakf.high][apply(df.peakf, 1, which.max)]

  return(df.stats)
}
