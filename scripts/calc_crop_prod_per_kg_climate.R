calc_crop_prod_per_kg_climate  <-  function(crop_type){
    
    # Load libraries
    library(tidyverse)
    library(raster)

    rasterOptions(tmpdir = "tmp/raster/", tmptime = 24)

    # Read historical kg climate
    hist_kg_raster <- raster("data/Beck_KG_V1/Beck_KG_V1_present_0p0083.tif")

    # Read future kg climate
    future_kg_raster  <- raster("data/Beck_KG_V1/Beck_KG_V1_future_0p0083.tif")

    # Read crop production
    prod_raster  <- raster(paste0("data/HarvestedAreaYield175Crops_Geotiff/GeoTiff/", crop_type, "/", crop_type, "_Production.tif"))

    # Read KG climate legend
    kg_legend <- read_delim("data/Beck_KG_V1/legend.txt", skip = 3, delim = ":", n_max = 30, col_names = c("id", "longname"))  %>%  # nolint
        mutate(id = as.integer(id), 
            name = str_extract(longname, pattern = "(?<= )\\w{2,3}(?= )"),
            name = as.factor(name))  %>% 
        dplyr::select(id, name)

    beginCluster(n = 120)

    prod_resamp  <- resample(prod_raster, hist_kg_raster, method = "bilinear")

    endCluster()

    prod_tbl <- as_tibble(as.data.frame(prod_resamp, xy = T))  %>% 
        arrange(x, y)  %>% 
        rename("production" = paste0(crop_type, "_Production"))

    hist_kg_tbl <- as_tibble(as.data.frame(hist_kg_raster, xy = T))  %>% 
        arrange(x, y)  %>% 
        mutate(Beck_KG_V1_present_0p0083 = na_if(Beck_KG_V1_present_0p0083, 0))

    future_kg_tbl <- as_tibble(as.data.frame(future_kg_raster, xy = T))  %>% 
        arrange(x, y)  %>% 
        mutate(Beck_KG_V1_future_0p0083 = na_if(Beck_KG_V1_future_0p0083, 0))

    percent_tbl <- prod_tbl %>% 
        cbind(., hist_kg_tbl) %>%
        cbind(., future_kg_tbl) %>%
        as_tibble(.name_repair = "unique") %>% 
        dplyr::select(x...1, y...2, production, Beck_KG_V1_present_0p0083, Beck_KG_V1_future_0p0083)  %>% 
        rename("lon" = "x...1", "lat" = "y...2") %>% 
        drop_na(Beck_KG_V1_present_0p0083)  %>% 
        mutate(Beck_KG_V1_future_0p0083 = as.factor(Beck_KG_V1_future_0p0083),
            Beck_KG_V1_present_0p0083 = as.factor(Beck_KG_V1_present_0p0083)) %>% 
        pivot_longer(c(Beck_KG_V1_future_0p0083, Beck_KG_V1_present_0p0083)) %>% 
        mutate(name = as.factor(name)) %>% 
        group_by(name, value, .drop = FALSE) %>%
        summarise(production = sum(production, na.rm = T)) %>% 
        ungroup() %>% 
        group_by(name) %>% 
        mutate(percent_production = production/sum(production)*100) %>% 
        ungroup() %>% 
        mutate(scenario = str_extract(name, pattern = "future|present"), id = as.integer(value)) %>%
        dplyr::select(!c(name, value)) %>%
        left_join(kg_legend, by = "id") %>% 
        rename("kg_id" = "id")  %>% 
        mutate(crop = crop_type)

    system(paste0("mkdir data/crop_prod_per_kg_climate/", crop_type))

    write_csv(percent_tbl, file = paste0("data/crop_prod_per_kg_climate/", crop_type, "/", crop_type, "_production.csv"), col_names = T)
}


