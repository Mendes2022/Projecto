# Function to identify the presence of glycine in a dataframe
find_glycine <- function(dataset, column_name, top_n_samples = 5) {
  # Extract the specified column intensity and ppm values
  sample_column <- unname(dataset$data[, column_name])
  ppm_column <- rownames(dataset$data)

  # Specify the chemical shift range for glycine (ppm)
  glycine_shift_range <- c(3.54, 3.56)  
  # Filter the NMR data within the specified chemical shift range for glycine
  glycine_indices <- which(ppm_column >= glycine_shift_range[1] & ppm_column <= glycine_shift_range[2])
  
  # Check if the intensity values fall within the specified range for glycine
  glycine_present <- all(sample_column[glycine_indices] >= glycine_shift_range[1] & sample_column[glycine_indices] <= glycine_shift_range[2])

  # Determine the presence of glycine
  if (glycine_present) {
    glycine_presence <- "glycine is not present"
  } else {
    glycine_presence <- "glycine is present"
  }

  # Print the glycine presence result
  cat("glycine presence in", column_name, ":", glycine_presence)

  # Get the indices of samples with glycine presence
  glycine_sample_indices <- which(dataset$data[, column_name] >= glycine_shift_range[1] & dataset$data[, column_name] <= glycine_shift_range[2])

  # Get the corresponding sample names
  glycine_samples <- rownames(dataset$data)[glycine_sample_indices]

  # Print the top N samples indicating glycine presence
  cat(paste("\nTop", top_n_samples, "samples indicating glycine presence:\n"))
  head(glycine_samples, top_n_samples)
}