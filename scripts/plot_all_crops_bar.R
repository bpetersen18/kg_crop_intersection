# Load libraries
library(tidyverse)

`%!in%` <- Negate(`%in%`)
source("scripts/plot_crop_prod_bar.R")

# List available crops
avail_crops <- list.dirs("data/crop_prod_per_kg_climate", recursive = FALSE, full.names = FALSE)

# List already plotted crops
plotted_crops  <- list.dirs("figures/per_crop", recursive = F, full.names = F)

tibble(crop = avail_crops) %>% 
    filter(crop %!in% plotted_crops)  %>% 
    pull(crop) %>% 
    map(~ plot_crop_prod_bar(.))
