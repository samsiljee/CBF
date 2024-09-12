# Script to change from Olympus .MOV files to raw byte volume files - trimming to the last 1000 frames
# Sam Siljee
# 12 September 2024

# Libraries
library(magick) # Read in video files, with `ffmpeg`

# Load video
video_frames <- image_read_video("21_073_1.MOV", fps = NULL)

# Get the number of frames
num_frames <- length(video_frames)

# keep the last 1000 frames
video_frames <- video_frames[(num_frames-999):num_frames]

# update the number of frames
num_frames <- length(video_frames)

# Initialize an empty list to store grayscale frames
frames_list <- vector("list", num_frames)

for (i in 1:num_frames) {
  # Extract the current frame
  frame <- video_frames[i]
  
  # Convert the frame to grayscale
  frame_gray <- image_convert(frame, colorspace = "gray")
  
  # Get pixel values and scale to 0-255 range
  frame_matrix <- as.integer(image_data(frame_gray))
  
  # Store the frame matrix in the list
  frames_list[[i]] <- frame_matrix
}

# Get the dimensions of the frames
frame_height <- dim(frames_list[[1]])[1]
frame_width  <- dim(frames_list[[1]])[2]

# Stack the frames into a 3D array (height x width x num_frames)
byte_volume <- array(unlist(frames_list), dim = c(frame_height, frame_width, num_frames))

# Convert to unsigned 8-bit integer
byte_volume <- as.raw(byte_volume)

# Write byte volume
writeBin(byte_volume, "output_volume.raw")
