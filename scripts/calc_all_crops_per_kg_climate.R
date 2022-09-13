# Load libraries
library(tidyverse)

# Set working directory
setwd("/work/bryan20/projects/kg_crop_intersection")

# Load functions
source("scripts/calc_crop_per_kg_climate.R")

`%!in%` <- Negate(`%in%`)

processed_crops <- sapply(str_split(basename(list.files(path = "data/derived/crop_per_kg_climate/", pattern = "_per_kg_stats.csv", recursive = TRUE, full.names = FALSE)), pattern = "_"), "[[", 1)

# Calculate 
tibble(crops = list.dirs("data/raw/HarvestedAreaYield175Crops_Geotiff/GeoTiff", recursive = F, full.names = F)) %>% 
   filter(crops %!in% processed_crops)  %>% 
   pull(crops) %>% 
   map(~ calc_crop_per_kg_climate(.))
