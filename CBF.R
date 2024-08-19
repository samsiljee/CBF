# Script to try out cilia beat frequency analysis in R
# Sam Siljee
# 14 August 2024

# Libraries
library(dplyr) # Pipe and data manipulation
library(StereoMorph) # extract video frames
library(imager) # load images
library(signal)

# Vector for donor IDs
donor_IDs <- c(
  "21_073",
  "21_161",
  "21_166",
  "21_189"
)

# Extract frame from the videos
# Loop through donors
for (donor in donor_IDs) {
  # Loop through wells
  for (i in 1:6) {
    # Extract the last 500 frames
    extractFrames(
      file = paste0("HS_video_copies/", donor, "_", i, ".MOV"),
      save.to = paste0("Frames/", donor, "_", i),
      frames = 9085:9584,
      names = paste0(donor, "_", i, "_", sprintf("%03.0f", 1:500)),
      ext = "jpg",
      warn.min = 501
    )
  }
}

# List of images in a folder
image_files <- list.files("Frames/21_073_1/", pattern = '*.jpg', full.names = TRUE)

# Load the images
image_list <- lapply(image_files, load.image)

# Set video details
height <- 640
width <- 360
frame_rate <- 480
num_frames <- 500

# Initialise an empty list to store the pixel intensity series
pixel_series <- vector("list", height * width)

# Loop through each pixel position
for (i in 1:height) {
  for (j in 1:width) {
    # Extract the intensity values for pixel (i, j) in the red channel across all frames
    series <- sapply(image_list, function(img) {
      img[i, j, 1, 1]  # Accessing the first channel (red channel)
    })
    
    # Store the series in the list
    pixel_series[[(i - 1) * width + j]] <- series
  }
  # Print progress
  print(paste(round(i/height*100), "%"))
}

# Calculate variance for each pixel series
variances <- sapply(pixel_series, var)

# Perform Fourier transformation and find the dominant frequency in Hz
dominant_frequencies_hz <- sapply(pixel_series, function(series) {
  fft_res <- fft(series)
  # Calculate the magnitude of the FFT
  mag <- Mod(fft_res)
  # Find the index of the dominant frequency (ignoring the zero-frequency component)
  dominant_index <- which.max(mag[-1]) + 1
  # Calculate the dominant frequency in Hz
  freq_hz <- (dominant_index - 1) * frame_rate / num_frames
  return(freq_hz)
})

# Reshape the variances and frequencies into matrices
variance_matrix <- matrix(variances, nrow = height, ncol = width)
frequency_matrix <- matrix(dominant_frequencies, nrow = height, ncol = width)

# Visualize using imager's plot function
plot(as.cimg(variance_matrix), main="Variance Map")
plot(as.cimg(frequency_matrix), main="Dominant Frequency Map")
