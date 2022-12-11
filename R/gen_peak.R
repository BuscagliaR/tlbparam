#' Generate Peak
#'
#' @param x Value
#' @param l.temp Low temperature range
#' @param h.temp High temperature range
#'
#' @return Returns a peak
#'
gen_peak <- function(x, l.temp, h.temp){
  # seq(45, 90, 0.1)[c(20,151)]
  # seq(45, 90, 0.1)[c(151,221)]
  # seq(45, 90, 0.1)[c(221,281)]
  # seq(45, 90, 0.1)[c(281,331)]
  ups <- c(5,5,4,4,5,3,4,3,5,2,3,4,2,3,2,2,5,1,4,1,3,1,2,1)
  downs <- c(5,4,5,4,3,5,3,4,2,5,3,2,4,2,3,2,1,5,1,4,1,3,1,2)

  peak <- NA

  for (i in (1:length(ups))){
    peaks <- pracma::findpeaks(x, nups=ups[i], ndowns=downs[i], minpeakdistance=5)
    if (!is.null(peaks)){
      for (i in 1:nrow(peaks))
        if ((peaks[i,2] > l.temp) & (peaks[i,2] < h.temp)){
          peak <- peaks[i,1]
          return(peak)
        }
    }
    if (length(l.temp:h.temp) %% 2 == 1){
      peak <- median(x[l.temp:h.temp])
    }else{
      peak <- median(x[l.temp:(h.temp-1)])
    }
  }
  return(peak)
}
