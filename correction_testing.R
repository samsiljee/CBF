# Create uncorrected video_array array
video_array_uncorrected <- video_array

# Initialise blank matrix for signal power image
signal_power_image_uncorrected <- matrix(data = 0, ncol = width, nrow = height)

# Loop through frames to get differences
for (z in 2:num_frames) {
  # Get absolute difference with previous frame
  difference <- abs(video_array_uncorrected[,,z] - video_array_uncorrected[,,(z-1)])
  
  # Add to signal power image
  signal_power_image_uncorrected <- signal_power_image_uncorrected + difference
}

# Save as a PNG image - grayscale
writePNG(normalise_image(signal_power_image - signal_power_image_uncorrected), paste0(output_dir, "signal_power_images/signal_power_image_", d, "_subtraction.png"))

# Save as a PNG image - grayscale
writePNG(normalise_image(signal_power_image_uncorrected), paste0(output_dir, "signal_power_images/signal_power_image_", d, "_uncorrected.png"))

# Initialise blank matrix
variance_matrix_uncorrected <- matrix(ncol = width, nrow = height)

# Get variance by pixel
for (y in 1:width) { # loop through columns
  for (x in 1:height) { # Loop through rows
    # Calculate variances and add to list in the required position
    variance_matrix_uncorrected[x, y] <- var(video_array_uncorrected[x, y, ])
  }
}

# Save as a PNG image - grayscale
writePNG(normalise_image(variance_matrix_uncorrected), paste0(output_dir, "variance_images/variance_image_", d, "_uncorrected.png"))

# Save as a PNG image - grayscale
writePNG(normalise_image(variance_matrix - variance_matrix_uncorrected), paste0(output_dir, "variance_images/variance_image_", d, "_subtraction.png"))

# Test FFT uncorrected
# Initialise matrices to contain dominant frequencies
FFT_matrix_uncorrected <- matrix(ncol = width, nrow = height)
FFT_matrix_unconverted_uncorrected <- FFT_matrix_uncorrected

# Calculate FFT and extract dominant frequencies
for (y in 1:width) { # loop through columns
  for (x in 1:height) { # Loop through rows
    # Get vector of pixel values
    pixel_values <- video_array_uncorrected[x, y, ]
    
    # Run FFT
    fft_result <- fft(pixel_values)
    
    # Get magnitudes - using only first half of FFT results
    magnitude <- abs(fft_result)[1:(num_frames/2 + 1)]
    
    # Set all magnitudes below frequency threshold to -1 so they won't be selected
    magnitude[1:threshold_offset] <- -1
    
    # Find the frequency with maximum amplitude
    max_index <- which.max(magnitude)
    
    # Add to list in the required position - converted to real frequency and unconverted
    FFT_matrix_uncorrected[x, y] <- freq[max_index]
    FFT_matrix_unconverted_uncorrected[x, y] <- max_index
  }
}

# Save as a PNG image - grayscale
writePNG(normalise_image(FFT_matrix_uncorrected), paste0(output_dir, "fft_images/FFT_freq_image_", d, "_uncorrected.png"))

# Save as a PNG image - grayscale
writePNG(normalise_image(FFT_matrix_unconverted), paste0(output_dir, "fft_images/FFT_unconverted_freq_image_", d, "_uncorrected.png"))

# Save as a PNG image - grayscale
writePNG(normalise_image(FFT_matrix_uncorrected - FFT_matrix), paste0(output_dir, "fft_images/FFT_freq_image_", d, "_subtraction.png"))

# Save as a PNG image - grayscale
writePNG(normalise_image(FFT_matrix_unconverted_uncorrected - FFT_matrix_unconverted), paste0(output_dir, "fft_images/FFT_unconverted_freq_image_", d, "_subtraction.png"))
