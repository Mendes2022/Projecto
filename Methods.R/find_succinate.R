# Function to identify the presence of succinate in a dataframe
find_succinate <- function(dataset, column_name, top_n_samples = 5) {
  # Extract the specified column intensity and ppm values
  sample_column <- unname(dataset$data[, column_name])
  ppm_column <- rownames(dataset$data)

  # Specify the chemical shift range for succinate (ppm)
  succinate_shift_range <- c(2.39,2.41) 

  # Filter the NMR data within the specified chemical shift range for succinate
  succinate_indices <- which(ppm_column >= succinate_shift_range[1] & ppm_column <= succinate_shift_range[2])
  
  # Check if the intensity values fall within the specified range for succinate
  succinate_present <- all(sample_column[succinate_indices] >= succinate_shift_range[1] & sample_column[succinate_indices] <= succinate_shift_range[2])

  # Determine the presence of succinate
  if (succinate_present) {
    succinate_presence <- "succinate is present"
  } else {
    succinate_presence <- "succinate is not present"
  }

  # Print the succinate presence result
  cat("succinate presence in", column_name, ":", succinate_presence)

  # Get the indices of samples with succinate presence
  succinate_sample_indices <- which(dataset$data[, column_name] >= succinate_shift_range[1] & dataset$data[, column_name] <= succinate_shift_range[2])

  # Get the corresponding sample names
  succinate_samples <- rownames(dataset$data)[succinate_sample_indices]

  # Print the top N samples indicating succinate presence
  cat(paste("\nTop", top_n_samples, "samples indicating succinate presence:\n"))
  head(succinate_samples, top_n_samples)
}
