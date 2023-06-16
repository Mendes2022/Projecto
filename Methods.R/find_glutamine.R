find_glutamine <- function(dataset, column_name, top_n_samples = 5) {
  # Extract the specified column intensity and ppm values
  sample_column <- unname(dataset$data[, column_name])
  ppm_column <- rownames(dataset$data)

  # Specify the chemical shift range for glutamine (ppm)
  glutamine_shift_range <- c(2.125, 3.45)
  
  # Filter the NMR data within the specified chemical shift range for glutamine
  glutamine_indices <- which(ppm_column >= glutamine_shift_range[1] & ppm_column <= glutamine_shift_range[2])
  
  # Check if the intensity values fall within the specified range for glutamine
  glutamine_present <- all(sample_column[glutamine_indices] >= glutamine_shift_range[1] & sample_column[glutamine_indices] <= glutamine_shift_range[2])

  # Determine the presence of glutamine
  if (glutamine_present) {
    glutamine_presence <- "glutamine is present"
  } else {
    glutamine_presence <- "glutamine is not present"
  }

  # Print the glutamine presence result
  cat("glutamine presence in", column_name, ":", glutamine_presence)

  # Get the indices of samples with glutamine presence
  glutamine_sample_indices <- which(dataset$data[, column_name] >= glutamine_shift_range[1] & dataset$data[, column_name] <= glutamine_shift_range[2])

  # Get the corresponding sample names
  glutamine_samples <- rownames(dataset$data)[glutamine_sample_indices]

  # Print the top N samples indicating glutamine presence
  cat(paste("\nTop", top_n_samples, "samples indicating glutamine presence:\n"))
  head(glutamine_samples, top_n_samples)
}
