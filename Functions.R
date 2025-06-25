# Script for function associated with CBF analysis
# Sam Siljee
# 17/06/2025

# Function to apply custom LUT to an image
apply_custom_LUT <- function(input_matrix, scale = TRUE, LUT = Custom_LUT) {
  # Get dimensions from input
  height <- nrow(input_matrix)
  width <- ncol(input_matrix)
  
  # Scale input if required
  if(scale){
    image_matrix <- (input_matrix - min(input_matrix)) / (max(input_matrix) - min(input_matrix)) * 256
  } else {
    image_matrix <- input_matrix
  }
  
  # Deal with overflow pixels
  image_matrix[image_matrix > 256] <- 256
  
  # Initialise blank 3D matrix
  RGB_array <- array(NA, dim = c(height, width, 3))
  
  # Add lookup table
  for (y in 1:width) { # loop through columns
    for (x in 1:height) { # Loop through rows
      # Get scaled index for LUT
      idx <- round(image_matrix[x, y] * (nrow(Custom_LUT) - 1) + 1)
      
      # Add values for each channel
      RGB_array[x, y, 1] <- LUT[idx, 2]
      RGB_array[x, y, 2] <- LUT[idx, 3]
      RGB_array[x, y, 3] <- LUT[idx, 4]
    }
  }
  
  # Scale form 0 to 1 as required for writePNG
  RGB_array <- RGB_array / 255
  
  return(RGB_array)
}

# Function to normalise the image for .png export
normalise_image <- function(input_matrix) {
  normalised_matrix <- (input_matrix - min(input_matrix)) / (max(input_matrix) - min(input_matrix))
  return(normalised_matrix)
}

# Function to identify clusters - translation from Lambert's CiliaClusters code
identify_clusters <- function(input_matrix) {
  # Get unique pixel values
  pixel_values <- unique(as.numeric(input_matrix))
  
  # Initialise output
  output_matrix <- matrix(0, nrow = nrow(input_matrix), ncol = ncol(input_matrix))
  
  # Loop through different pixel values
  for(i in 1:length(pixel_values)) {
    # Set pixel value to cluster by
    pixel_value <- pixel_values[i]
    
    # Create binary mask for this pixel value
    mask <- input_matrix == pixel_value
    
    # Label connected components in this mask - convert to linear numeric
    connected_components <- bwlabel(mask) %>% as.numeric()
    
    # Get number of components (excluding background)
    num_clusters <- max(connected_components)
    
    # Get cluster sizes and update output matrix
    for(j in 1:num_clusters) {
      cluster_pixels <- connected_components == j
      cluster_size <- sum(cluster_pixels)
      output_matrix[cluster_pixels] <- cluster_size
    }
  }
  
  # Output as a matrix
  return(output_matrix)
}
