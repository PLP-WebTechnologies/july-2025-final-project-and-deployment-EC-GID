# Load the packages
library(lidR)
library(raster)

# Set the directory path
immah_data <- "D:/Research/AllCHM/ALLCHM"

# List the .tif files in the directory
chm_files <- list.files(immah_data, pattern = "\\.tif$", full.names = TRUE)

# Print the list of files to verify
print(chm_files)



# Function to visualize a CHM file
visualize_chm <- function(chm_file) {
  # Load the CHM data
  chm <- raster(chm_file)
  
  # Visualize the CHM
  plot(chm, main = chm_file)
  
  # Wait for user input to proceed to the next file
  cat("Press [Enter] to continue to the next file...")
  readline()
}

# Iterate through each CHM file and visualize it
for (chm_file in chm_files) {
  visualize_chm(chm_file)
}
