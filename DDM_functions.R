## Script containing functions required for DDM analysis

# Function to preprocess the video
preprocess_video <- function(video_array) {
  # reassign input data
  video_processed <- video_array
  
  # Remove temporal mean (background subtraction)
  temporal_mean <- apply(video_processed, c(1, 2), mean, na.rm = TRUE)
  for (t in 1:num_frames) {
    video_processed[,,t] <- video_processed[,,t] - temporal_mean
  }
  
  # Apply spatial smoothing to reduce noise
  for (t in 1:num_frames) {
    video_processed[,,t] <- apply_gaussian_filter(video_processed[,,t], sigma = 1)
  }
  
  return(video_processed)
}

# Gaussian filter helper function
apply_gaussian_filter <- function(img, sigma = 1) {
  # Simple Gaussian filter implementation
  kernel_size <- ceiling(6 * sigma)
  if (kernel_size %% 2 == 0) kernel_size <- kernel_size + 1
  
  # Create Gaussian kernel
  x <- seq(-(kernel_size-1)/2, (kernel_size-1)/2, 1)
  kernel_1d <- exp(-x^2 / (2 * sigma^2))
  kernel_1d <- kernel_1d / sum(kernel_1d)
  
  # Apply separable filter with proper padding
  img_filtered <- img
  pad_size <- (kernel_size - 1) %/% 2
  
  # Horizontal pass
  for (i in 1:nrow(img)) {
    row_data <- img[i, ]
    # Pad the row data
    padded_row <- c(rep(row_data[1], pad_size), row_data, rep(row_data[length(row_data)], pad_size))
    # Convolve and extract central part
    filtered_row <- convolve(padded_row, rev(kernel_1d), type = "open")
    # Extract the part corresponding to original image
    start_idx <- pad_size + 1
    end_idx <- start_idx + length(row_data) - 1
    img_filtered[i, ] <- filtered_row[start_idx:end_idx]
  }
  
  # Vertical pass
  for (j in 1:ncol(img_filtered)) {
    col_data <- img_filtered[, j]
    # Pad the column data
    padded_col <- c(rep(col_data[1], pad_size), col_data, rep(col_data[length(col_data)], pad_size))
    # Convolve and extract central part
    filtered_col <- convolve(padded_col, rev(kernel_1d), type = "open")
    # Extract the part corresponding to original image
    start_idx <- pad_size + 1
    end_idx <- start_idx + length(col_data) - 1
    img_filtered[, j] <- filtered_col[start_idx:end_idx]
  }
  
  return(img_filtered)
}

# Core DDM structure function calculation
calculate_ddm_structure_function <- function(video_array, window_size = 16, overlap = 0.25, time_lags = c(1,2,4,8)) {
  # Calculate step size for overlapping windows
  step_size <- floor(window_size * (1 - overlap))
  
  # Initialize result arrays
  n_windows_x <- floor((width - window_size) / step_size) + 1
  n_windows_y <- floor((height - window_size) / step_size) + 1
  
  # Structure function array: [y_window, x_window, q_magnitude, time_lag]
  q_values <- seq(0.1, 2, by = 0.2)  # Wave vector magnitudes
  structure_function <- array(0, dim = c(n_windows_y, n_windows_x, length(q_values), length(time_lags)))
  
  # Process each spatial window
  for (wy in 1:n_windows_y) {
    for (wx in 1:n_windows_x) {
      
      y_start <- (wy - 1) * step_size + 1
      y_end <- y_start + window_size - 1
      x_start <- (wx - 1) * step_size + 1
      x_end <- x_start + window_size - 1
      
      # Extract window time series
      window_data <- video_array[y_start:y_end, x_start:x_end, ]
      
      # Calculate structure function for this window
      window_sf <- calculate_window_structure_function(window_data, time_lags, q_values)
      structure_function[wy, wx, , ] <- window_sf
    }
    
    if (wy %% 10 == 0) {
      cat(sprintf("Processed %d/%d window rows\n", wy, n_windows_y))
    }
  }
  
  return(list(
    structure_function = structure_function,
    time_lags = time_lags,
    q_values = q_values,
    window_positions = list(
      x = seq(1, width - window_size + 1, by = step_size),
      y = seq(1, height - window_size + 1, by = step_size)
    )
  ))
}

# Calculate structure function for a single window
calculate_window_structure_function <- function(window_data, time_lags, q_values) {
  
  window_size <- dim(window_data)[1]  # Assuming square windows
  num_frames <- dim(window_data)[3]
  
  # Initialize structure function matrix
  sf_matrix <- matrix(0, nrow = length(q_values), ncol = length(time_lags))
  
  # Create wave vector grid
  qx <- seq(-pi, pi, length.out = window_size)
  qy <- seq(-pi, pi, length.out = window_size)
  q_grid <- expand.grid(qx, qy)
  q_magnitudes <- sqrt(q_grid[,1]^2 + q_grid[,2]^2)
  
  for (lag_idx in seq_along(time_lags)) {
    lag <- time_lags[lag_idx]
    
    if (lag >= num_frames) next
    
    # Calculate intensity differences for this lag
    diff_sum <- array(0, dim = c(window_size, window_size))
    n_pairs <- 0
    
    for (t in 1:(num_frames - lag)) {
      diff_frame <- window_data[,,t + lag] - window_data[,,t]
      diff_sum <- diff_sum + diff_frame^2
      n_pairs <- n_pairs + 1
    }
    
    # Average over time pairs
    if (n_pairs > 0) {
      avg_diff <- diff_sum / n_pairs
      
      # Fourier transform
      fft_diff <- fft(avg_diff)
      power_spectrum <- abs(fft_diff)^2
      
      # Average over q-shells
      for (q_idx in seq_along(q_values)) {
        q_target <- q_values[q_idx]
        
        # Find pixels within q-shell
        q_mask <- (q_magnitudes >= q_target - 0.05) & (q_magnitudes < q_target + 0.05)
        
        if (sum(q_mask) > 0) {
          sf_matrix[q_idx, lag_idx] <- mean(power_spectrum[q_mask])
        }
      }
    }
  }
  
  return(sf_matrix)
}

# Extract frequency maps from DDM results
extract_frequency_maps <- function(ddm_result, frame_rate, freq_range) {
  cat("Extracting frequency maps...\n")
  
  structure_function <- ddm_result$structure_function
  time_lags <- ddm_result$time_lags
  
  n_windows_y <- dim(structure_function)[1]
  n_windows_x <- dim(structure_function)[2]
  
  # Initialize frequency maps
  dominant_freq_map <- matrix(0, nrow = n_windows_y, ncol = n_windows_x)
  amplitude_map <- matrix(0, nrow = n_windows_y, ncol = n_windows_x)
  
  # Process each spatial window
  for (wy in 1:n_windows_y) {
    for (wx in 1:n_windows_x) {
      
      # Average over q-values for this spatial location
      avg_structure_func <- apply(structure_function[wy, wx, , ], 2, mean)
      
      # Fit exponential decay model to extract frequency
      freq_result <- fit_ddm_model(avg_structure_func, time_lags, frame_rate, freq_range)
      
      dominant_freq_map[wy, wx] <- freq_result$frequency
      amplitude_map[wy, wx] <- freq_result$amplitude
    }
  }
  
  return(list(
    dominant_frequency = dominant_freq_map,
    amplitude = amplitude_map
  ))
}

# Fit DDM model to extract frequency information
fit_ddm_model <- function(structure_func, time_lags, frame_rate, freq_range) {
  
  # Convert time lags to actual time
  time_points <- time_lags / frame_rate
  
  # Simple approach: look for oscillatory component in structure function
  # Take FFT of structure function to find dominant frequency
  
  if (length(structure_func) < 4) {
    return(list(frequency = 0, amplitude = 0))
  }
  
  # Detrend the structure function
  trend <- lm(structure_func ~ time_points)
  detrended <- residuals(trend)
  
  # Apply window to reduce edge effects
  if (length(detrended) > 4) {
    window <- signal::hanning(length(detrended))
    windowed_signal <- detrended * window
    
    # FFT to find frequency content
    fft_result <- fft(windowed_signal)
    power_spectrum <- abs(fft_result[1:(length(fft_result) %/% 2)])
    
    # Frequency axis
    freq_axis <- (0:(length(power_spectrum) - 1)) * frame_rate / length(windowed_signal)
    
    # Find peak within expected cilia frequency range
    freq_mask <- (freq_axis >= freq_range[1]) & (freq_axis <= freq_range[2])
    
    if (sum(freq_mask) > 0) {
      masked_power <- power_spectrum
      masked_power[!freq_mask] <- 0
      
      peak_idx <- which.max(masked_power)
      dominant_freq <- freq_axis[peak_idx]
      amplitude <- power_spectrum[peak_idx]
      
      return(list(frequency = dominant_freq, amplitude = amplitude))
    }
  }
  
  return(list(frequency = 0, amplitude = 0))
}

# Identify ciliated regions based on frequency maps
identify_ciliated_regions <- function(freq_maps, 
                                      freq_threshold = c(10, 25),
                                      amplitude_threshold_percentile = 0.6) {
  
  cat("Identifying ciliated regions...\n")
  
  freq_map <- freq_maps$dominant_frequency
  amp_map <- freq_maps$amplitude
  
  # Frequency-based mask
  freq_mask <- (freq_map >= freq_threshold[1]) & (freq_map <= freq_threshold[2])
  
  # Amplitude-based mask (regions with significant signal)
  amp_threshold <- quantile(amp_map[amp_map > 0], amplitude_threshold_percentile, na.rm = TRUE)
  amp_mask <- amp_map >= amp_threshold
  
  # Combined mask
  cilia_mask <- freq_mask & amp_mask
  
  return(cilia_mask)
}

# Calculate CBF statistics
calculate_cbf_statistics <- function(freq_maps, cilia_mask) {
  
  freq_map <- freq_maps$dominant_frequency
  amp_map <- freq_maps$amplitude
  
  # Extract frequencies from ciliated regions only
  cilia_frequencies <- freq_map[cilia_mask]
  cilia_amplitudes <- amp_map[cilia_mask]
  
  # Remove zeros and invalid values
  valid_freq <- cilia_frequencies[cilia_frequencies > 0 & !is.na(cilia_frequencies)]
  
  if (length(valid_freq) == 0) {
    warning("No valid ciliated regions detected!")
    return(list(
      mean_cbf = NA,
      median_cbf = NA,
      std_cbf = NA,
      ciliated_fraction = 0,
      n_ciliated_regions = 0
    ))
  }
  
  stats <- list(
    mean_cbf = mean(valid_freq),
    median_cbf = median(valid_freq),
    std_cbf = sd(valid_freq),
    ciliated_fraction = sum(cilia_mask) / length(cilia_mask),
    n_ciliated_regions = sum(cilia_mask),
    frequency_distribution = valid_freq
  )
  
  return(stats)
}