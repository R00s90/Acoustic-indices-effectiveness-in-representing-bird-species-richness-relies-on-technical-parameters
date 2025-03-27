library(lme4)
library(tidyverse)
library(interactions)
library(lmerTest)

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
all <- read.csv("R_Output/ai_richness.csv")

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

#'## ____ model general LOOP_____#####
#  create a list of each dataset to be used in a model (splitted by index, sampling.rate, fft_w)
all_list <- all_scaled %>% split(f = list(all_scaled$index, all_scaled$sampling.rate, all_scaled$fft_w, all_scaled$settings), drop = T)

# an empty df to store the output of the loop
model_out_general <- data.frame()

for(i in 1:length(all_list)){
  # get data to use in the model
  df_model <- all_list[[i]]
  
  # in case of value == NA: move to the next iteration
  if(is.na(df_model$value[1])) next
  
  # create model
  m2 <- lmer(value_scaled ~ richness + (1|plotID) + (1|date_time), data = df_model)
  
  
  # model output to save
  low_ci <- confint(m2,  parm = "richness")[,1] # 2.5% CI method="Wald" to make it quicker
  estimate <- coef(summary(as(m2,"lmerModLmerTest")))[2,1] # estimate
  up_ci <- confint(m2,  parm = "richness")[,2] # 97.5% CI
  
  # get other values from modeled data
  index <- df_model$index[1]
  sampling.rate <- df_model$sampling.rate[1]
  fft_w <- df_model$fft_w[1]
  settings <- df_model$settings[1]
  n <- nrow(df_model)
  habitat <- "All habitats \ncombined"
  
  # save into df
  df_temp <- data.frame(low_ci, 
                        estimate, 
                        up_ci, 
                        index, 
                        sampling.rate, 
                        fft_w,
                        settings,
                        habitat,
                        n)
  
  # rbind to the empty df created outside of the model
  model_out_general <- rbind(model_out_general, df_temp)
  
  # Progress bar
  progress_bar(n = length(all_list),
                  iteration = i, 
                  width = 100)
  
}

## Identify model effects that are significant
model_out_general_2 <- model_out_general%>% 
  mutate(effect = ifelse(sign(low_ci) == sign(up_ci), "significant", "unsignificant"),
         pos_neg = ifelse(estimate > 0, "positive", "negative") %>% as.factor()) %>% 
  mutate(effect = fct_relevel(effect, c("unsignificant", "significant")),
         index = recode(index, acoustic_complexity = "Acoustic complexity", bioacoustic_index = "Bioacoustic index"))

#'## __________plot_______________####

# modify df to prepare data for plotting
df_plot <- model_out_general_2 %>% 
  mutate(sampling.rate = paste(sampling.rate/1000, "kHz") %>% as.factor()) %>% 
  mutate(sampling.rate = fct_relevel(sampling.rate, c("24 kHz",  "48 kHz",  "96 kHz", "192 kHz")))

p <- ggplot(data = df_plot, 
            
            # general aesthetics
            aes(x = estimate, 
                y = fft_w %>% as.factor(),
                shape = settings,
                colour = pos_neg,
                alpha = effect)) +
  
  # Create facets for each sampling rate and each index
  facet_grid(cols = vars(sampling.rate), rows = vars(index)) +
  
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
  geom_vline(xintercept = 0,  colour = "grey30", linewidth = 1) +
  
  # set the transparancy values to distinguish between significant and unsignificant effects
  scale_alpha_manual(values = c(0.3, 1), name = NULL) +
  
  # remove name from the shape legend
  scale_shape_discrete(name = NULL) +
  
  # remove name from the colour legend and set the colour values
  scale_color_manual(name = NULL, values = c("hotpink2", "royalblue4")) +
  
  # edit the x-axis
  scale_x_continuous(n.breaks = 3, 
                     breaks = c(-0.1, 0, 0.1), 
                     name = "Model estimate", 
                     limits = c(-0.1,0.12)) +
  
  # title of the y-axis
  ylab("FFT window size") +
  
  # black and white theme
  theme_bw()
p