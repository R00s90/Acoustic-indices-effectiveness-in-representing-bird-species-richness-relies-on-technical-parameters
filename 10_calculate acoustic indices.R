library(soundecology)
library(tidyverse)
library(seewave)
library(doParallel)

#'# ___Calculate indices - adjusted frequency range__
# Calculate the number of cores to use
no_cores_available <- detectCores()-2

# calculate the acoustic complexity index and the bioacoustic index
for(window_size in c(512, 1024, 2048, 4096)) {
  # ACI
  multiple_sounds(directory = input_directory,
                  resultfile = output_file_aci,
                  soundindex = "acoustic_complexity",
                  no_cores = no_cores_available,
                  min_freq = 500, 
                  max_freq = 10000, 
                  j = 5, 
                  fft_w = window_size
  )
  
  # BI
  multiple_sounds(directory = input_directory,
                  resultfile = output_file_bi,
                  soundindex = "bioacoustic_index",
                  no_cores = no_cores_available,
                  min_freq = 500, 
                  max_freq = 10000, 
                  fft_w = window_size
  )
}


#'# ___Calculate indices - default frequency range__
# Calculate the number of cores to use
no_cores_available <- detectCores()-2

# calculate the acoustic complexity index and the bioacoustic index
for(window_size in c(512, 1024, 2048, 4096)) {
  # ACI
  multiple_sounds(directory = input_directory,
                  resultfile = output_file_aci,
                  soundindex = "acoustic_complexity",
                  no_cores = no_cores_available,
                  j = 5, 
                  fft_w = window_size
  )
  
  # BI
  multiple_sounds(directory = input_directory,
                  resultfile = output_file_bi,
                  soundindex = "bioacoustic_index",
                  no_cores = no_cores_available,
                  fft_w = window_size
  )
}