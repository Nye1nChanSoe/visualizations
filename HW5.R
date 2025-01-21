library(tidyverse)
library(sf)

setwd('/Users/nyeinchan/Desktop/Harbour.Space/data_to_knowledge/SESSION_5/data')



esp_adm2 <- read_sf(dsn = ".", "ESP_adm2")

name_1_list <- esp_adm2 %>%
  dplyr::distinct(NAME_1) %>%
  dplyr::pull(NAME_1) # extract as a vector

name_1_list

# Hardcoded colors for 18 regions
region_colors <- c(
  "Andalucía" = "#8B0000",             # Dark Red
  "Aragón" = "#00008B",                # Dark Blue
  "Cantabria" = "#006400",             # Dark Green
  "Castilla-La Mancha" = "#4B0082",    # Indigo
  "Castilla y León" = "#8B4513",       # Saddle Brown
  "Cataluña" = "#2F4F4F",              # Dark Slate Gray
  "Ceuta y Melilla" = "#556B2F",       # Dark Olive Green
  "Comunidad de Madrid" = "#B22222",   # Firebrick
  "Comunidad Foral de Navarra" = "#483D8B", # Dark Slate Blue
  "Comunidad Valenciana" = "#228B22",  # Forest Green
  "Extremadura" = "#8B8000",           # Dark Yellow
  "Galicia" = "#191970",               # Midnight Blue
  "Islas Baleares" = "#5F9EA0",        # Cadet Blue
  "Islas Canarias" = "#6B8E23",        # Olive Drab
  "La Rioja" = "#8B008B",              # Dark Magenta
  "País Vasco" = "#2E8B57",            # Sea Green
  "Principado de Asturias" = "#4682B4",# Steel Blue
  "Región de Murcia" = "#A0522D"       # Sienna
)
region_colors <- sapply(region_colors, function(color) adjustcolor(color, alpha.f = 0.85))
print(region_colors)


# Simplify geometries to reduce complexity for plotting
esp_adm2_simply <- st_simplify(esp_adm2, preserveTopology = TRUE, dTolerance = 1000)
ggplot() +
  geom_sf(data = esp_adm2_simply) +
  ggtitle("Simplified Geometries of Spain")



canary_island <- esp_adm2_simply |> filter(NAME_1 == "Islas Canarias")
st_bbox(canary_island)
#      xmin      ymin      xmax      ymax 
# -18.15792  27.63875 -13.41875  29.41542 
ggplot() +
  geom_sf(data = canary_island) +
  ggtitle("Canary Islands")



esp_adm2_simply_without_canary <- esp_adm2_simply |> filter(NAME_1 != "Islas Canarias")
st_bbox(esp_adm2_simply_without_canary)
#       xmin      ymin      xmax      ymax 
#    -9.299860 35.283909  4.314306 43.790417 
ggplot() +
  geom_sf(data = esp_adm2_simply_without_canary) +
  ggtitle("Mainland Spain (Without Canary Islands)")


# calculate offsets to reposition Canary Islands
# extract geometry of the Canary Island
canary_island_g <- st_geometry(canary_island)
offset_x <- -12.5 - st_bbox(canary_island)["xmin"]
offset_y <- 35.0 - st_bbox(canary_island)["ymin"]

# apply offsets to reposition the island
canary_island_g <- canary_island_g + c(offset_x, offset_y)

st_crs(canary_island_g)
canary_island_g <- st_as_sf(canary_island_g, crs = st_crs(esp_adm2_simply_without_canary))

# Add region names
canary_island_g$NAME_1 <- canary_island$NAME_1
canary_island_g$NAME_2 <- canary_island$NAME_2

ggplot() +
  geom_sf(data = esp_adm2_simply_without_canary, aes(fill = NAME_1), color = "black") +
  geom_sf_text(
    data = esp_adm2_simply_without_canary,
    mapping = aes(
      label = NAME_2, 
      color = ifelse(NAME_2 %in% c("Ceuta", "Melilla"), "black", "white")
    ),
    size = 2.5,
    fontface = "bold"
  ) +
  
  # Plot Canary Islands with unique colors and labels
  geom_sf(data = canary_island_g, aes(fill = NAME_1), color = "black") +
  geom_sf_text(
    data = canary_island_g,
    mapping = aes(label = NAME_2),
    size = 2.5,
    fontface = "bold",
    color = "black") +
  
  # Title and theme
  labs(
    title = "Map of provinces in Spain",
    x = "",
    y = "",
  ) +
  theme_minimal() +
  theme(
    legend.position = "hide", 
    plot.title = element_text(
      size = 20, 
      face="bold", 
      hjust = 0.5
    ),
    panel.grid = element_blank(),
    axis.text = element_blank()
  ) +
  guides(fill = guide_legend(title = "Regions")) +
  scale_fill_manual(values = region_colors) +
  scale_color_identity()

ggsave("./HW_IMAGE/1_exe_esp_adm2_name_2_provinces_map.png",
       scale = 1,
       height = 8,
       width = 12,
       bg = "white",
       dpi = 300)


# Exercise 2
# multi-layer map by provinces and regions
# regions: NAME_1, ID_1
# provinces: NAME_2, ID_2

esp_adm2_without_canary <- esp_adm2 %>%
  dplyr::filter(NAME_1 != "Islas Canarias")

glimpse(esp_adm2_without_canary)
glimpse(canary_island_g)

# fix unmatched columns between mainland and canary island
canary_island_g <- canary_island_g %>%
  dplyr::rename(geometry = x)

esp_adm2_without_canary <- esp_adm2_without_canary %>%
  dplyr::select(geometry, NAME_1, NAME_2)

# combined previously created mainland and island maps
province_map <- rbind(esp_adm2_without_canary, canary_island_g)
region_map <- province_map %>%
  dplyr::group_by(NAME_1) %>%
  dplyr::summarise(geometry = st_union(geometry)) %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") 
# note: the st_buffer function helps us cleaning sliver, 
# however it can create some misalignment between layers. 

ggplot() +
  geom_sf(data = region_map)

ggplot() +
  geom_sf(data = province_map)


# centroids to center region name in the polygons
centroids_region <- st_centroid(region_map)
centroids_region <- cbind(region_map, st_coordinates(st_centroid(centroids_region$geometry)))
centroids_region <- centroids_region %>%
  mutate(NAME_1_wrapped = str_wrap(NAME_1, width = 12))

ggplot() +
  # Province borders (lighter and dashed)
  geom_sf(data = region_map, aes(fill = NAME_1), color = "black", linewidth = 0.5) +
  geom_sf(data = province_map, fill = NA, color = "black", linewidth = .1) +
  geom_text(
    data = centroids_region,
    aes(
      x = X,
      y = Y,
      label = NAME_1_wrapped,
      color = ifelse(
        NAME_1 %in% c("Islas Canarias", "Ceuta y Melilla", "Islas Baleares"), "navyblue", "white"
      )
    ),
    size = 3,
    fontface = "bold",
    check_overlap = TRUE
  ) +
  labs(
    title = "Map of regions in Spain",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      size = 20, 
      face="bold", 
      hjust = 0.5
    ),
    panel.grid = element_blank(),
    axis.text = element_blank()
  ) +
  scale_fill_manual(values = region_colors) +
  scale_color_identity()

ggsave("./HW_IMAGE/2_exe_esp_adm2_name_2_region_map.png",
       scale = 1,
       height = 8,
       width = 12,
       bg = "white",
       dpi = 300)
