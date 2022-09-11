plot_crop_prod_bar <- function(crop){
    # Load libraries
    library(tidyverse)

    p1 <- read_csv(paste0("data/crop_prod_per_kg_climate/", crop, "/", crop, "_production.csv"), col_names = T) %>% 
        group_by(name) %>% 
        mutate(percent_production_tot = sum(percent_production)) %>% 
        ungroup() %>% 
        filter(percent_production_tot >= 0.1) %>% 
        ggplot(aes(x = name, y = percent_production, fill = scenario)) + 
        geom_col(position = "dodge") +
        labs(x = "Koppen-Gieger Climate Classification", y = "Percent of Production", fill = NULL,
            title = paste0(str_to_upper(crop))) +
        scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
        theme_bw() +
        theme(axis.text = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 14, face = "bold"),
            legend.text = element_text(size = 14, face = "bold"),
            legend.position = c(0.9, 0.9),
            plot.title = element_text(size = 14, face = "bold"))

    system(paste0("mkdir figures/per_crop/", crop))

    ggsave(filename = paste0("figures/per_crop/", crop, "/", crop, "_production.png"),
        plot = p1, height = 6, width = 10, units = "in", dpi = 400)
}