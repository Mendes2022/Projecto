find_chlorogenic<- function(dataset,column_name, top_n_peaks = 5) {
  # Extract the specified column intensity and ppm values
  sample_column <- unname(dataset$data[, column_name])
  ppm_column <- rownames(dataset$data)

  # Define the chemical shift range for chlorogenic acid
  chlorogenic_acid_shift_range <- c(6.0, 7.0)

  # Filter the NMR data within the specified chemical shift range
  chlorogenic_acid_indices <- which(ppm_column >= chlorogenic_acid_shift_range[1] & ppm_column <= chlorogenic_acid_shift_range[2])
  chlorogenic_acid_data <- data.frame(ppm = ppm_column[chlorogenic_acid_indices], intensity = sample_column[chlorogenic_acid_indices])

  # Sort the data by intensity in descending order
  chlorogenic_acid_data <- chlorogenic_acid_data[order(-chlorogenic_acid_data$intensity), ]

  # Print the top N peaks in the dataset
  cat(paste("Top", top_n_peaks, "peaks indicating chlorogenic acid:\n"))
  head(chlorogenic_acid_data, top_n_peaks)
}