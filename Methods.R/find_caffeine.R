find_caffeine <- function(dataset, column_name, top_n_peaks = 5) {
  # Extract the specified column intensity and ppm values
  sample_column <- unname(dataset$data[, column_name])
  ppm_column <- rownames(dataset$data)

  # Define the chemical shift range for caffeine
  caffeine_shift_range <- c(3.0, 3.5)

  # Filter the NMR data within the specified chemical shift range
  caffeine_indices <- which(ppm_column >= caffeine_shift_range[1] & ppm_column <= caffeine_shift_range[2])
  caffeine_data <- data.frame(ppm = ppm_column[caffeine_indices], intensity = sample_column[caffeine_indices])

  # Sort the data by intensity in descending order
  caffeine_data <- caffeine_data[order(-caffeine_data$intensity), ]

  # Print the top N peaks in the dataset
  cat(paste("Top", top_n_peaks, "peaks indicating", column_name, ":\n"))
  head(caffeine_data, top_n_peaks)
}