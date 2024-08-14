# Script to try out cilia beat frequency analysis in R
# Sam Siljee
# 14 August 2024

# Libraries
library(dpylr) # Pipe and data manipulation
library(imager) # Load video

# Load data
video <- load.video("21_073_1.MOV")
