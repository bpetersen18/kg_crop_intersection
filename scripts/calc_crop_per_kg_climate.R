calc_crop_per_kg_climate  <-  function(crop_type){
    
    # Load libraries
    library(tidyverse)
    library(raster)

    rasterOptions(tmpdir = "tmp/raster/", tmptime = 24)

    # Read crop production
    prod_raster <- raster(paste0("data/raw/HarvestedAreaYield175Crops_Geotiff/GeoTiff/", crop_type, "/", crop_type, "_Production.tif"))
    yield_raster <- raster(paste0("data/raw/HarvestedAreaYield175Crops_Geotiff/GeoTiff/", crop_type, "/", crop_type, "_YieldPerHectare.tif"))
    area_raster <- raster(paste0("data/raw/HarvestedAreaYield175Crops_Geotiff/GeoTiff/", crop_type, "/", crop_type, "_HarvestedAreaHectares.tif"))

    # Read KG climate legend
    kg_legend <- read_delim("data/raw/Beck_KG_V1/legend.txt", skip = 3, delim = ":", n_max = 30, col_names = c("id", "longname"))  %>%  # nolint
        mutate(id = as.integer(id), 
            name = str_extract(longname, pattern = "(?<= )\\w{2,3}(?= )"),
            name = as.factor(name))  %>% 
        dplyr::select(id, name)

    prod_tbl <- as_tibble(as.data.frame(prod_raster, xy = T))  %>% 
        arrange(x, y)  %>% 
        rename("production" = paste0(crop_type, "_Production"))

    yield_tbl <- as_tibble(as.data.frame(yield_raster, xy = T))  %>% 
        arrange(x, y)  %>% 
        rename("yield" = paste0(crop_type, "_YieldPerHectare"))

    area_tbl <- as_tibble(as.data.frame(area_raster, xy = T)) %>% 
        arrange(x, y)  %>% 
        rename("area" = paste0(crop_type, "_HarvestedAreaHectares"))

    hist_kg_tbl <- read_csv(file = "data/derived/kg_climate_tibbles/hist_kg_tbl.csv", col_names = T)

    future_kg_tbl <- read_csv(file = "data/derived/kg_climate_tibbles/future_kg_tbl.csv", col_names = T)

    percent_tbl <- prod_tbl %>%
        cbind(., yield_tbl) %>% 
        cbind(., area_tbl) %>%  
        cbind(., hist_kg_tbl) %>%
        cbind(., future_kg_tbl) %>%
        as_tibble(.name_repair = "unique") %>% 
        dplyr::select(x...1, y...2, production, yield, area, Beck_KG_V1_present_0p083, Beck_KG_V1_future_0p083)  %>% 
        rename("lon" = "x...1", "lat" = "y...2") %>% 
        drop_na(Beck_KG_V1_present_0p083)  %>% 
        mutate(Beck_KG_V1_future_0p083 = as.factor(Beck_KG_V1_future_0p083),
            Beck_KG_V1_present_0p083 = as.factor(Beck_KG_V1_present_0p083)) %>% 
        pivot_longer(c(Beck_KG_V1_future_0p083, Beck_KG_V1_present_0p083)) %>% 
        mutate(name = as.factor(name)) %>% 
        group_by(name, value, .drop = FALSE) %>%
        summarise(production = sum(production, na.rm = T),
                  area = sum(area, na.rm = T),
                  yield_mean = mean(yield, na.rm = T),
                  yield_upper_quant = quantile(yield, 0.75, na.rm = T),
                  yield_lower_quant = quantile(yield, 0.25, na.rm = T),
                  yield_sd = sd(yield, na.rm = T)) %>% 
        ungroup() %>% 
        group_by(name) %>% 
        mutate(percent_production = production/sum(production)*100) %>% 
        ungroup() %>% 
        mutate(scenario = str_extract(name, pattern = "future|present"), id = as.integer(value)) %>%
        dplyr::select(!c(name, value)) %>%
        left_join(kg_legend, by = "id") %>% 
        rename("kg_id" = "id")  %>% 
        mutate(crop = crop_type)

    system(paste0("mkdir data/derived/crop_per_kg_climate/", crop_type))

    write_csv(percent_tbl, file = paste0("data/derived/crop_per_kg_climate/", crop_type, "/", crop_type, "_per_kg_stats.csv"), col_names = T)
}


