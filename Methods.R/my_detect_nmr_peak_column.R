detect_nmr_peaks <- function (spectrum.list, baseline_treshold = 50000) 
{
  res = list()
  x = t(as.matrix(spectrum.list$intensity))
  peaks_detected_idx = speaq::detectSpecPeaks(x, verbose = FALSE, 
                                              baselineThresh = baseline_treshold)
  res$ppm = as.numeric(spectrum.list$ppm[peaks_detected_idx[[1]]])
  res$intensity = as.numeric(spectrum.list$intensity[peaks_detected_idx[[1]]])
  rownames(res) = NULL
  return(res)
}




detect_nmr_peaks_from_column <- function(dataset, column, baseline_tresh = 50000, ap.method = "own", ap.samp.classes = 1, ap.step = 0.03) {

  spec <- list()
  spec[["ppm"]] <- rownames(dataset$data)
  spec[["intensity"]] <- dataset$data[, column]
  
  detected_peaks <- detect_nmr_peaks(spec, baseline_treshold = baseline_tresh)
  message("Spectrum ", column, " has ", length(detected_peaks$ppm), " peaks.")
  message("\n")
  
  detected_peaks_df <- as.data.frame(detected_peaks)
  detected_peaks_df$ppm <- round(detected_peaks_df$ppm, 2)
  
  return(detected_peaks_df)
}
