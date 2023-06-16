# Function to identify the presence of lactate in a dataframe
find_lactate <- function(dataset, column_name, top_n_peaks = 5) {
  # Extract the specified column intensity and ppm values
  sample_column <- unname(dataset$data[, column_name])
  ppm_column <- rownames(dataset$data)

  # Specify the chemical shift ranges for β and α protons of lactate
  beta_shift_range <- c(1.25, 1.35)  # δ = 1.30 ppm ± 0.05 ppm
 
  # Filter the NMR data within the specified chemical shift ranges
  beta_indices <- which(ppm_column >= beta_shift_range[1] & ppm_column <= beta_shift_range[2])
  
  # Check if the intensity values fall within the specified ranges for lactate
  beta_present <- all(sample_column[beta_indices] >= beta_shift_range[1] & sample_column[beta_indices] <= beta_shift_range[2])

  # Determine the presence of lactate based on the β and α resonances
  if (beta_present) {
    lactate_presence <- "Lactate is not present"
  } else {
    lactate_presence <- "Lactate is present"
  }

  # Print the lactate presence result
  cat("Lactate presence in", column_name, ":", lactate_presence)
}

