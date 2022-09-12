calc_crop_prod_per_kg_climate  <-  function(crop_type){
    
    # Load libraries
    library(tidyverse)
    library(raster)

    rasterOptions(tmpdir = "tmp/raster/", tmptime = 24)

    # Read historical kg climate
    hist_kg_raster <- raster("data/Beck_KG_V1/Beck_KG_V1_present_0p0083.tif")

    # Read crop yield
    yield_raster  <- raster(paste0("data/HarvestedAreaYield175Crops_Geotiff/GeoTiff/", crop_type, "/", crop_type, "_YieldPerHectare.tif"))

    # Read KG climate legend
    kg_legend <- read_delim("data/Beck_KG_V1/legend.txt", skip = 3, delim = ":", n_max = 30, col_names = c("id", "longname"))  %>%  # nolint
        mutate(id = as.integer(id), 
            name = str_extract(longname, pattern = "(?<= )\\w{2,3}(?= )"),
            name = as.factor(name))  %>% 
        dplyr::select(id, name)

    beginCluster(n = 120)

    yield_resamp  <- resample(yield_raster, hist_kg_raster, method = "bilinear")

    endCluster()

    yield_tbl <- as_tibble(as.data.frame(yield_resamp, xy = T))  %>% 
        arrange(x, y)  %>% 
        rename("yield" = paste0(crop_type, "_YieldPerHectare"))

    hist_kg_tbl <- as_tibble(as.data.frame(hist_kg_raster, xy = T))  %>% 
        arrange(x, y)  %>% 
        mutate(Beck_KG_V1_present_0p0083 = na_if(Beck_KG_V1_present_0p0083, 0))

    final_tbl <- yield_tbl %>% 
        cbind(., hist_kg_tbl) %>%
        as_tibble(.name_repair = "unique") %>% 
        dplyr::select(x...1, y...2, yield, Beck_KG_V1_present_0p0083)  %>% 
        rename("lon" = "x...1", "lat" = "y...2") %>% 
        drop_na(Beck_KG_V1_present_0p0083)  %>% 
        mutate(Beck_KG_V1_present_0p0083 = as.factor(Beck_KG_V1_present_0p0083)) %>% 
        pivot_longer(Beck_KG_V1_present_0p0083) %>% 
        mutate(name = as.factor(name)) %>% 
        group_by(name, value, .drop = FALSE) %>%
        summarise(yield_mean = mean(yield, na.rm = T),
                  yield_upper_quant = quantile(yield, 0.75, na.rm = T),
                  yield_lower_quant = quantile(yield, 0.25, na.rm = T),
                  yield_sd = sd(yield, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(scenario = str_extract(name, pattern = "future|present"), id = as.integer(value)) %>%
        dplyr::select(!c(name, value)) %>%
        left_join(kg_legend, by = "id") %>% 
        rename("kg_id" = "id")  %>% 
        mutate(crop = crop_type)

    system(paste0("mkdir data/crop_per_kg_climate/", crop_type))

    write_csv(final_tbl, file = paste0("data/crop_per_kg_climate/", crop_type, "/", crop_type, "_yield.csv"), col_names = T)
}


