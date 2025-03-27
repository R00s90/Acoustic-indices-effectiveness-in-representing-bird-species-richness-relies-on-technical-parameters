library(tuneR)
library(tidyverse)
library(seewave)
library(foreach)
library(doParallel)

#'# ____Function for downsampling and saving files____
downsample_and_save <- function(input_file, output_folder, target_rate) {
  # Load the .wav file
  wav <- readWave(input_file)
  
  # Downsample to the target rate (96 kHz)
  resampled_wav <- resamp(wav, f = wav@samp.rate, g = target_rate, output = "Wave")
  
  # Create output file path
  output_file <- file.path(output_folder, basename(input_file))
  
  # Save the downsampled .wav file
  writeWave(resampled_wav, output_file, extensible = FALSE)
}

#'# _____parallel loop to downsample all the recordings___
# register the number of cores to use
doParallel::registerDoParallel(detectCores()-2 )

# loop
foreach(i = c(1:nbr_of_my_recordings)) %dopar% {
  foreach(target_rate = c(96000, 48000, 24000)) %do% {
    
    # prepare input_file and output_folder
    output_folder <- paste("my_directory", target_rate, sep = "/")
    input_file <- "my_recording.wav"
    
    # downsample and save all the WAV files
    downsample_and_save(input_file = input_file, 
                        output_folder = output_folder,
                        target_rate = target_rate)
  }
  
}