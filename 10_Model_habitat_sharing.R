library(lme4)
library(tidyverse)
library(lmerTest)
library(ggstance)
library(interactions)


#' _____progress bar___
#' Because some of the LOOPs take quite long, a progress bar is informative
#' After the example from: https://stackoverflow.com/questions/26919787/r-text-progress-bar-in-for-loop

progress_bar <- function(n, iteration, width){
  ii <- iteration
  extra <- nchar('||100%')
  width <- options()$width
  step <- round(ii / n * (width - extra))
  text <- sprintf('|%s%s|% 3s%%', strrep('=', step),
                  strrep(' ', width - step - extra), round(ii / n * 100))
  cat(text)
  Sys.sleep(0.05)
  cat(if (ii == n) '\n' else '\014')
}

#' ### ___read data from csv ___
all <- read.csv("my_data.csv")

#'### ____group habitat variables___###

levels(all$habitat) <- c("Farmland and \n semi-natural \nmeadows", "Logged \nspruce forest", "Natural \nregeneration \n forest", "Unaffected \nspruce forest", "Background \nforest" )

#'### ___LOOP: scale values by index, sampling rate and window length____####

## create a list of each dataset to be used in a model (splitted by index, sampling.rate, fft_w, settings)
all_list <- all %>% split(f = list(all$index, all$sampling.rate, all$fft_w, all$settings), drop = T) 

# an empty df to store the output of the loop
all_scaled <- data.frame()

# set seed to ensure reproducibility
set.seed(42)

# loop through the list, to scale the parameters
for(i in 1:length(all_list)){
  
  # The new scaled variable has a mean value of 0, and a standard deviation of 1
  to_scale <- all_list[[i]] %>% 
    mutate(value_scaled = scale(value) %>% as.vector())
  
  all_scaled <- rbind(to_scale, all_scaled)
  
  # Progress bar
  progress_bar(n = length(all_list),
               iteration = i, 
               width = 100)
  
}

# sd = 1, mean = 0
sd(all_scaled$value_scaled) 
mean(all_scaled$value_scaled)

#'## ____ LOOP: model each combination of technical parameters with habitat as an interaction_____#####

# create a list of each dataset to be used in a model (splitted by index, sampling.rate, fft_w)
all_list <- all_scaled %>% split(f = list(all_scaled$index, all_scaled$sampling.rate, all_scaled$fft_w, all_scaled$settings), drop = T)

# an empty df to store the output of the loop
model_out_hab <- data.frame()

for(i in 1:length(all_list)){
  # get data to use in the model
  df_model <- all_list[[i]]
  
  # in case of value == NA: move to the next iteration
  if(is.na(df_model$value[1])) next
  
  # create model
  m2 <- lmer(value_scaled ~ richness * habitat + (1|plotID) + (1|date_time), data = df_model)
  
  
  ## model output to save 
  
  # slopes in different habitats in dataframe
  slope <- interactions::sim_slopes(model = m2, pred = "richness", modx = "habitat")$slopes %>% 
    
    # get other values from modeled data:
    mutate(index = df_model$index[1],
           sampling.rate = df_model$sampling.rate[1],
           fft_w = df_model$fft_w[1],
           settings = df_model$settings[1],
           n = nrow(df_model))
  
  # rbind output with the output from previous loop rounds
  model_out_hab <- rbind(model_out_hab, slope)
  
  # progress bar
  progress_bar(n = length(all_list),
                  iteration = i, 
                  width = 100)
}

#'## ____ Modify output from the loop__
model_out_hab_2 <- model_out_hab%>% 
  # improve naming to make things easier downstream
  rename(low_ci = "2.5%",
         up_ci = "97.5%",
         estimate = "Est.",
         habitat = "Value of habitat") %>% 
  # identify significant and unsignificant and positive and negative effects
  mutate(effect = ifelse(sign(low_ci) == sign(up_ci), "significant", "unsignificant"),
         pos_neg = ifelse(estimate > 0, "positive", "negative") %>% as.factor()) %>% 
  
  # change the order of the levels; i.e. unsignificant comes before significant
  mutate(effect = fct_relevel(effect, c("unsignificant", "significant")),
         
         # improve index names
         index = recode(index, 
                        acoustic_complexity = "Acoustic complexity \nindex", 
                        bioacoustic_index = "Bioacoustic index"),
         
         # improve habitat names
         habitat = factor(habitat, levels = c("Farmland", "Logged \nspruce stands", "Natural \nregeneration \nstands", "Unaffected \nspruce stands", "Background \nforest stands"))) %>% 
  
  # remove some unnecessary parameters
  select(!c('t val.', 'p', 'S.E.'))

#'## __________plot_______________####
# set the index_filter to either "Bioacoustic index" or to "Acoustic complexity \nindex"
index_filter <- "Bioacoustic index"

# filter data for only the index of interest
df_plot <- model_out_hab_2 %>% 
  filter(index == index_filter,
  )%>% 
  
  # improve the name of the sampling rate classes
  mutate(sampling.rate = paste(sampling.rate/1000, "kHz") %>% as.factor()) %>% 
  
  # improve the order of the sampling rate classes
  mutate(sampling.rate = fct_relevel(sampling.rate, c("24 kHz",  "48 kHz",  "96 kHz", "192 kHz")))


p <- ggplot(data = df_plot,
            
            # general aesthetics
            aes(x = estimate, 
                y = fft_w %>% as.factor(),
                shape = settings,
                colour = pos_neg,
                alpha = effect)) +
  
  # create facets based on the sampling rate and the habitats
  facet_grid(cols = vars(sampling.rate), rows = vars(habitat)) +

  # add lines for error bars based on the 95% confidence intervals, dodged to avoid overlap
  geom_errorbar(aes(xmin = low_ci, 
                    xmax = up_ci), 
                width = 0,
                position = position_dodgev(height=0.5))+
  
  # add points of each model effect, dodged to avoid overlap
  geom_point(
    size = 2,
    position = position_dodgev(height=0.5)) +
  
  # add a vertical line indicating no effect (i.e. effect = 0)
  geom_vline(xintercept = 0, linetype = "solid", colour = "grey30", linewidth = 1) +
  
  # set the transparancy values to distinguish between significant and unsignificant effects
  scale_alpha_manual(values = c(0.3, 1), name = NULL) +
  
  # remove name from the shape legend
  scale_shape_discrete(name = NULL) +
  
  # remove name from the colour legend and set the colour values
  scale_color_manual(name = NULL, values = c("hotpink2", "royalblue4")) +
  
  # edit the x-axis
  scale_x_continuous(n.breaks = 3, breaks = c(-0.1, 0, 0.1), name = "Model estimate") +
  
  # title of the y-axis
  ylab("FFT window size") +
  
  # add the index of the figure above the plot
  labs(title = index_filter) +
  
  # black and white theme
  theme_bw()
p
