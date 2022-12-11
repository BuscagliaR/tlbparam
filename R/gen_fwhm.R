#' Generate FWHM
#'
#' @param x Value
#' @param temperatures Value
#'
#' @return Returns FWHM
#'
#'
gen_fwhm <- function(x, temperatures){
  lymax <- as.numeric(which.max(x))
  half <- (max(x)-min(x))/2
  x1 <- as.numeric(which(abs(x[1:lymax]-half) == min(abs(x[1:lymax]-half))))
  x2 <- lymax + as.numeric(which(abs(x[lymax:length(x)]-half) == min(abs(x[lymax:length(x)]-half))))
  temp1 <- temperatures[x1[1]]
  temp2 <- temperatures[x2[1]]
  fwhm <- (as.double(temp2-temp1))
  return(fwhm)
}
