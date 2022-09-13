# Load libraries
library(tidyverse)
library(raster)

rasterOptions(tmpdir = "tmp/raster/", tmptime = 24)

# Read historical kg climate
hist_kg_raster <- raster("data/raw/Beck_KG_V1/Beck_KG_V1_present_0p083.tif")

# Read future kg climate
future_kg_raster  <- raster("data/raw/Beck_KG_V1/Beck_KG_V1_future_0p083.tif")

hist_kg_tbl <- as_tibble(as.data.frame(hist_kg_raster, xy = T)) %>% 
    arrange(x, y)  %>% 
    mutate(Beck_KG_V1_present_0p083 = na_if(Beck_KG_V1_present_0p083, 0))

future_kg_tbl <- as_tibble(as.data.frame(future_kg_raster, xy = T)) %>% 
    arrange(x, y)  %>% 
    mutate(Beck_KG_V1_future_0p083 = na_if(Beck_KG_V1_future_0p083, 0))

# Write out
write_csv(hist_kg_tbl, file = "data/derived/kg_climate_tibbles/hist_kg_tbl.csv")
write_csv(future_kg_tbl, file = "data/derived/kg_climate_tibbles/future_kg_tbl.csv")
