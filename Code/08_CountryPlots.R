#Maps zoom

#Thailand Vietnam
limits <- st_bbox(c(xmin = 96.7, xmax = 108.2, ymin = 4, ymax = 22.5), crs = st_crs(4326)) %>% 
  st_as_sfc() %>%
  st_transform(cCRS) %>%
  st_bbox()

plot <- fPlot_Rank(result_BioServ, limits = limits, palet = "viridis")

ggsave("Figures/Mollweide_splitted/Minimum_Shortfall/Zoom/ThaVnm.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(28, 50, 75, 100), lm = c(28, 100))

ggsave("Figures/Mollweide_splitted/Minimum_Shortfall/Zoom/ThaVnm_WDPA.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

#PNG
limits <- st_bbox(c(xmin = 140, xmax = 152.5, ymin = -18.5, ymax = 1), crs = st_crs(4326)) %>% 
  st_as_sfc() %>%
  st_transform(cCRS) %>%
  st_bbox()

plot <- fPlot_Rank(result_BioServ, limits = limits, palet = "viridis")

ggsave("Figures/Mollweide_splitted/Minimum_Shortfall/Zoom/PNG.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(28, 50, 75, 100), lm = c(28, 100))

ggsave("Figures/Mollweide_splitted/Minimum_Shortfall/Zoom/PNG_WDPA.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

#SE Asia
limits <- st_bbox(c(xmin = 108, xmax = 125.5, ymin = -0.3, ymax = -11.5), crs = st_crs(4326)) %>% 
  st_as_sfc() %>%
  st_transform(cCRS) %>%
  st_bbox()

plot <- fPlot_Rank(result_BioServ, limits = limits, palet = "viridis")

ggsave("Figures/Mollweide_splitted/Minimum_Shortfall/Zoom/SEAsia.pdf",  dpi = 1000, 
       width = 8, height = 6, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(28, 50, 75, 100), lm = c(28, 100))

ggsave("Figures/Mollweide_splitted/Minimum_Shortfall/Zoom/SEAsia_WDPA.pdf",  dpi = 1000, 
       width = 8, height = 6, units = "cm", limitsize = FALSE)


#Red Sea 
limits <- st_bbox(c(xmin = 44, xmax = 32.5, ymin = 12, ymax = 29.5), crs = st_crs(4326)) %>% 
  st_as_sfc() %>%
  st_transform(cCRS) %>%
  st_bbox()

plot <- fPlot_Rank(result_BioServ, limits = limits, palet = "viridis")

ggsave("Figures/Mollweide_splitted/Minimum_Shortfall/Zoom/RedSea.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(28, 50, 75, 100), lm = c(28, 100))

ggsave("Figures/Mollweide_splitted/Minimum_Shortfall/Zoom/RedSea_WDPA.pdf",  dpi = 1000, 
       width = 6, height = 7, units = "cm", limitsize = FALSE)

#Latin America
limits <- st_bbox(c(xmin = -93.5, xmax = -76, ymin = 11.5, ymax = 24), crs = st_crs(4326)) %>% 
  st_as_sfc() %>%
  st_transform(cCRS) %>%
  st_bbox()

plot <- fPlot_Rank(result_BioServ, limits = limits, palet = "viridis")

ggsave("Figures/Mollweide_splitted/Minimum_Shortfall/Zoom/LatAm.pdf",  dpi = 1000, 
       width = 8, height = 6, units = "cm", limitsize = FALSE)

plot <- fPlot_Rank(result_BioServ_WDPA_rmPA, limits = limits, palet = "viridis", 
                   brk = c(28, 50, 75, 100), lm = c(28, 100))

ggsave("Figures/Mollweide_splitted/Minimum_Shortfall/Zoom/LatAm_WDPA.pdf",  dpi = 1000, 
       width = 8, height = 6, units = "cm", limitsize = FALSE)

# Using the cowplot package
plot <- ggplot() +
  #geom_sf(data = world_map, colour ="grey30", fill = "grey50", size = 0.01) +
  geom_sf(data = PUs, colour = "black", size = 0.01) +
  geom_sf(data = result_BioServ, aes(fill = rank), colour = "black", size = 0.01) +
  #geom_sf(data = ProtectedPUs, fill = "black", colour = "black", size = 0.01) +
  #scale_fill_viridis_c(limits = c(1, 100)) +
  theme_bw(base_size = 8) +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_sf(xlim = c(limits$xmin, limits$xmax), 
           ylim = c(limits$ymin, limits$ymax), 
           expand = FALSE)

legend <- cowplot::get_legend(plot)

grid::grid.newpage()
grid::grid.draw(legend)

ggsave("Figures/Mollweide_splitted/Minimum_Shortfall/Zoom/Legend.svg",  dpi = 1000, 
       width = 1, height = 3, units = "cm", limitsize = FALSE)
