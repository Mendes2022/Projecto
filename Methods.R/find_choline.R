# Function to identify the presence of choline in a dataframe
find_choline <- function(dataset, column_name, top_n_samples = 5) {
  # Extract the specified column intensity and ppm values
  sample_column <- unname(dataset$data[, column_name])
  ppm_column <- rownames(dataset$data)

  # Specify the chemical shift range for choline (ppm)
  choline_shift_range <- c(3.51, 4.06)  
  # Filter the NMR data within the specified chemical shift range for choline
  choline_indices <- which(ppm_column >= choline_shift_range[1] & ppm_column <= choline_shift_range[2])
  
  # Check if the intensity values fall within the specified range for choline
  choline_present <- all(sample_column[choline_indices] >= choline_shift_range[1] & sample_column[choline_indices] <= choline_shift_range[2])

  # Determine the presence of choline
  if (choline_present) {
    choline_presence <- "choline is not present"
  } else {
    choline_presence <- "choline is present"
  }

  # Print the choline presence result
  cat("choline presence in", column_name, ":", choline_presence)

  # Get the indices of samples with choline presence
  choline_sample_indices <- which(dataset$data[, column_name] >= choline_shift_range[1] & dataset$data[, column_name] <= choline_shift_range[2])

  # Get the corresponding sample names
  choline_samples <- rownames(dataset$data)[choline_sample_indices]

  # Print the top N samples indicating choline presence
  cat(paste("\nTop", top_n_samples, "samples indicating choline presence:\n"))
  head(choline_samples, top_n_samples)
}
