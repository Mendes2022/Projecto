# Function to identify the presence of acetate in a dataframe
find_acetate <- function(dataset, column_name, top_n_samples = 5) {
  # Extract the specified column intensity and ppm values
  sample_column <- unname(dataset$data[, column_name])
  ppm_column <- rownames(dataset$data)

  # Specify the chemical shift range for acetate (ppm)
  acetate_shift_range <- c(1.9, 1.92) 

  # Filter the NMR data within the specified chemical shift range for acetate
  acetate_indices <- which(ppm_column >= acetate_shift_range[1] & ppm_column <= acetate_shift_range[2])
  
  # Check if the intensity values fall within the specified range for acetate
  acetate_present <- all(sample_column[acetate_indices] >= acetate_shift_range[1] & sample_column[acetate_indices] <= acetate_shift_range[2])

  # Determine the presence of acetate
  if (acetate_present) {
    acetate_presence <- "acetate is present"
  } else {
    acetate_presence <- "acetate is not present"
  }

  # Print the acetate presence result
  cat("acetate presence in", column_name, ":", acetate_presence)

  # Get the indices of samples with acetate presence
  acetate_sample_indices <- which(dataset$data[, column_name] >= acetate_shift_range[1] & dataset$data[, column_name] <= acetate_shift_range[2])

  # Get the corresponding sample names
  acetate_samples <- rownames(dataset$data)[acetate_sample_indices]

  # Print the top N samples indicating acetate presence
  cat(paste("\nTop", top_n_samples, "samples indicating acetate presence:\n"))
  head(acetate_samples, top_n_samples)
}