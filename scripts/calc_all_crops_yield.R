# Load libraries
library(tidyverse)

# Set working directory
setwd("/home/bryan20/projects/kg_crop_intersection")

# Load functions
source("scripts/calc_crop_yield_per_kg_climate.R")

`%!in%` <- Negate(`%in%`)

processed_crops <- sapply(str_split(basename(list.files(path = "data/crop_per_kg_climate/", pattern = "_yield.csv", recursive = TRUE, full.names = FALSE)), pattern = "_"), "[[", 1)

# Calculate crop production percentages in all K-G climates
tibble(crops = list.dirs("data/HarvestedAreaYield175Crops_Geotiff/GeoTiff", recursive = F, full.names = F)) %>% 
   filter(crops %!in% processed_crops)  %>% 
   pull(crops) %>% 
   map(~ calc_crop_prod_per_kg_climate(.))
