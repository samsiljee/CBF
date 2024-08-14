# Script to try out cilia beat frequency analysis in R
# Sam Siljee
# 14 August 2024

# Libraries
library(dplyr) # Pipe and data manipulation
library(imager) # Load video
library(StereoMorph) # extract video frames

# Load data
video <- load.video("21_073_1.MOV", maxSize = 1)

# Extract the last 500 frames
extractFrames(file = '21_073_1.MOV', save.to = 'Frames', frames = 9582:9584)
