# Load libraries
library(tidyverse)

# Set working directory
setwd("/home/bryan20/projects/kg_crop_intersection")

# Load functions
source("scripts/calc_crop_prod_per_kg_climate.R")

`%!in%` <- Negate(`%in%`)

# Calculate crop production percentages in all K-G climates
tibble(crops = list.dirs("data/HarvestedAreaYield175Crops_Geotiff/GeoTiff", recursive = F, full.names = F)) %>% 
   filter(crops %!in% list.dirs("data/crop_prod_per_kg_climate", recursive = FALSE, full.names = FALSE))  %>% 
   pull(crops) %>% 
   map(~ calc_crop_prod_per_kg_climate(.))

# crops <- tibble(crops = list.dirs("data/HarvestedAreaYield175Crops_Geotiff/GeoTiff", recursive = F, full.names = F)) %>% 
#     filter(crops %!in% c("wheat", "sugarcane", "maize", "cotton", "oats", "rice", "soybean"))  %>% 
#     pull(crops)

# cl  <- makeCluster(100, type = "FORK")
# registerDoParallel(cl)

#  foreach(i = crops) %dopar%
#      calc_crop_prod_per_kg_climate(i)

# stopCluster(cl)
