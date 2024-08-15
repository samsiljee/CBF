# Script to try out cilia beat frequency analysis in R
# Sam Siljee
# 14 August 2024

# Libraries
library(dplyr) # Pipe and data manipulation
library(imager) # Load video
library(StereoMorph) # extract video frames

# Vector for donor IDs
donor_IDs <- c(
  "21_073",
  "21_161",
  "21_166",
  "21_189"
)

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
