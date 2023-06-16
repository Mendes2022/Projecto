# Function to identify the presence of alanine in a dataframe
find_alanine <- function(dataset, column_name, top_n_samples = 5) {
  # Extract the specified column intensity and ppm values
  sample_column <- unname(dataset$data[, column_name])
  ppm_column <- rownames(dataset$data)

  # Specify the chemical shift range for alanine (ppm)
  alanine_shift_range <- c(3.777, 3.797)  # δ = 3.787 ppm ± 0.01 ppm
  
  # Filter the NMR data within the specified chemical shift range for alanine
  alanine_indices <- which(ppm_column >= alanine_shift_range[1] & ppm_column <= alanine_shift_range[2])
  
  # Check if the intensity values fall within the specified range for alanine
  alanine_present <- all(sample_column[alanine_indices] >= alanine_shift_range[1] & sample_column[alanine_indices] <= alanine_shift_range[2])

  # Determine the presence of alanine
  if (alanine_present) {
    alanine_presence <- "Alanine is present"
  } else {
    alanine_presence <- "Alanine is not present"
  }

  # Print the alanine presence result
  cat("Alanine presence in", column_name, ":", alanine_presence)

  # Get the indices of samples with alanine presence
  alanine_sample_indices <- which(dataset$data[, column_name] >= alanine_shift_range[1] & dataset$data[, column_name] <= alanine_shift_range[2])

  # Get the corresponding sample names
  alanine_samples <- rownames(dataset$data)[alanine_sample_indices]

  # Print the top N samples indicating alanine presence
  cat(paste("\nTop", top_n_samples, "samples indicating alanine presence:\n"))
  head(alanine_samples, top_n_samples)
}
