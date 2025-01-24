# Load libraries
library(tidyverse)
library(sf)
library(terra)
library(rgeoboundaries) #https://www.geoboundaries.org/index.html
library(climateR) #https://github.com/mikejohnson51/climateR/
library(biscale)
# Adicionar biblioteca
library(ggspatial)

#install.packages("remotes")
#remotes::install_github("wmgeolab/rgeoboundaries")
#remotes::install_github("mikejohnson51/climateR")

# Load United Kingdom boundary data at the country level (administrative level 0)
br0 <- geoboundaries(country = c("Brazil"), adm_lvl = "adm1")

# Load United Kingdom boundary data at the first administrative level (e.g., regions)
br1 <- geoboundaries(country = c("Brazil"), adm_lvl = "adm2")

city <- 'Amazonas'

# Filtrar Minas Gerais
mg0 <- br0 %>%
  filter(shapeName == city)

# Identificar municípios que intersectam MG
mg1 <- br1 %>%
  mutate(
    intersects_mg = st_intersects(., mg0) %>% lengths > 0
  ) %>%
  filter(intersects_mg) %>%
  mutate(
    area_original = st_area(.),
    intersecao = st_intersection(., mg0),
    area_intersecao = st_area(intersecao)
  ) %>%
  filter(as.numeric(area_intersecao/area_original) > 0.5) %>%
  select(-c(area_original, intersecao, area_intersecao, intersects_mg))

# Plotar MG
plot(st_geometry(mg1), border = "gray")
plot(st_geometry(mg0), add = TRUE, border = "black", lwd = 2)


# Retrieve TerraClimate data for the United Kingdom with specified variables (max and min temperature)
temp_terra = getTerraClim(AOI = mg0, 
                          varname = c("tmax", "tmin"),
                          startDate = "1994-01-01",
                          endDate  = "2023-12-01")

# Calculate the monthly mean temperature by averaging maximum and minimum temperature
mean_temp_monthly <- (temp_terra$tmax + temp_terra$tmin) / 2

# Calculate the annual mean temperature by averaging the monthly means across all months
annual_mean_temp <- mean(mean_temp_monthly, na.rm = TRUE)

# Plot the annual mean temperature
plot(annual_mean_temp)

# Resample the annual mean temperature raster to a finer resolution (approximately 1 km)
new_res <- 0.025  # Set the new resolution
new_raster <- rast(ext(annual_mean_temp), resolution = new_res, crs = crs(annual_mean_temp))
annual_mean_temp <- resample(x = annual_mean_temp, y = new_raster, method="bilinear")

# Crop the resampled temperature data to match the boundaries of the Brazil
br_temp <- terra::crop(annual_mean_temp, y = mg0, mask = TRUE)

# Plot the cropped annual mean temperature for the Brazil
plot(br_temp)

####### Precipitação ######
# Retrieve TerraClimate data for precipitation in the Brazil
ppt_terra = getTerraClim(AOI = mg0, 
                         varname = "ppt",
                         startDate = "1994-01-01",
                         endDate  = "2023-12-01")

# Calculate the annual mean precipitation by averaging the monthly precipitation values across all months
annual_mean_ppt <- mean(ppt_terra$ppt, na.rm = TRUE)

# Resample the annual mean precipitation raster to a finer resolution (approximately 1 km)
new_raster <- rast(ext(annual_mean_ppt), resolution = new_res, crs = crs(annual_mean_ppt))
annual_mean_ppt <- resample(x = annual_mean_ppt, y = new_raster, method = "bilinear")

# Crop the resampled precipitation data to match the boundaries of the United Kingdom
br_ppt <- terra::crop(annual_mean_ppt, y = mg0, mask = TRUE)

# Plot the cropped annual mean precipitation for the United Kingdom
plot(br_ppt)

#### Empilhando Rasters ####
# Combine temperature and precipitation rasters into a single raster stack
temp_ppt <- c(br_temp, br_ppt)

# Assign descriptive names to each raster layer in the stack
names(temp_ppt) <- c("temp", "ppt")

#### Convertendo para Dataframe ####
# Project the combined temperature and precipitation raster stack to match the projection of the UK boundary
# Then convert the raster data to a data frame, retaining the x and y coordinates for mapping
temp_ppt_df <- temp_ppt |> 
  project(mg0) |> 
  as.data.frame(xy = TRUE)

# Display the first few rows of the resulting data frame to verify the data
head(temp_ppt_df)

#### Bivariate Map ####
# Classify the temperature and precipitation data into bivariate classes using the 'biscale' package
# 'style = "quantile"' divides data into quantiles, and 'dim = 4' creates 4 classes for each variable, resulting in 16 bivariate categories
data <- bi_class(temp_ppt_df,
                 x = temp, 
                 y = ppt, 
                 style = "quantile", dim = 4)

# Plot the distribution of the bivariate classes to visualize the frequency of each class
data |> 
  count(bi_class) |> 
  ggplot(aes(x = bi_class, y = n)) +
  geom_col() +  # Create a bar plot to show the count of each bivariate class
  labs(title = "Distribution of Bivariate Classes", x = "Bivariate Class", y = "Frequency")

#### Mapping ####
# Set the color palette for the bivariate map
pallet <- "GrPink2"

# Obter limites com margem
bbox_mg <- st_bbox(mg0)
margin <- 0.01  # margem em graus

# Create the bivariate map using ggplot2
map <- ggplot() +
  theme_void(base_size = 14) +  # Set a minimal theme for the map
  xlim(bbox_mg[1] - margin, bbox_mg[3] + margin) +  # xmin e xmax com margem
  ylim(bbox_mg[2] - margin, bbox_mg[4] + margin) +  # ymin e ymax com margem
  # Overlay the country-level boundary of the United Kingdom
  geom_sf(data = br0, fill = NA, color = "black", linewidth = 0.40) +
  # Overlay the first administrative level boundaries of the Minas Gerais
  # Plot the bivariate raster data with appropriate fill color based on bivariate classes
  geom_raster(data = data, mapping = aes(x = x, y = y, fill = bi_class), color = NA, linewidth = 0.1, show.legend = FALSE) +
  # Overlay the first administrative level boundaries of the Minas Gerais
  geom_sf(data = mg1, fill = NA, color = "white", linewidth = 0.05) +
  # Apply the bivariate color scale using the selected palette and dimensions
  bi_scale_fill(pal = pallet, dim = 4, flip_axes = FALSE, rotate_pal = FALSE) +
  # Adicionar barra de escala
  annotation_scale(
    location = "bl",           # bottom left
    width_hint = 0.25,         # largura da barra
    style = "bar",
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),  # padding x
    pad_y = unit(0.5, "cm")   # padding y
  ) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(2, "cm"),
    pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  # Add labels for the map
  labs(title = "Amazonas: Padrões de Temperatura e Precipitação", 
       subtitle = "Média de Temperatura e Precipitação dos últimos 29 anos",
       caption = "Fonte: Terra Climate Data - Autor: Thiago Quinaia") +
  # Customize the appearance of the title, subtitle, and caption
  theme(
    plot.title = element_text(hjust = 0.4, face = "bold"),
    plot.subtitle = element_text(hjust = 0.4),
    plot.caption = element_text(size = 9, face = "italic", hjust = 0.4),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25),
    plot.margin = margin(t = 10, r = 0, b = 10, l = 10, unit = "pt")
  )

# Create the legend for the bivariate map
legend <- bi_legend(pal = pallet,   
                    flip_axes = FALSE,
                    rotate_pal = FALSE,
                    dim = 4,
                    xlab = "Temperatura (Cº)",
                    ylab = "Precipitação (mm)",
                    size = 9,
                    pad_color = "black")

# Combine the map and legend using cowplot
library(cowplot)


# Ajustar margens e criar viewport centralizado
finalPlot <- ggdraw(xlim = c(-0.01, 1.01), ylim = c(-0.01, 1.01)) +
  draw_plot(map, 0.02, 0, 0.7, 1) +  # Pequeno ajuste na posição x do mapa
  draw_plot(legend, 0.75, 0.35, 0.25, 0.25)  # Ajuste na posição x da legenda



# Display the final map with legend
finalPlot

# Calcular proporção do mapa
width <- bbox_mg[3] - bbox_mg[1]
height <- bbox_mg[4] - bbox_mg[2]
aspect_ratio <- bbox_width/bbox_height

# Ajustar proporções para salvamento
width_with_legend <- width * 1.1  # Aumentar para 40% extra
height_with_margins <- height * 1.1  # Adicionar 10% na altura



ggsave(
  "BR_Temp_PPT_amazonas.png", 
  finalPlot, 
  dpi = 600, 
  width = width_with_legend, 
  height = height_with_margins,
  units = "in",
  bg = "white",
  limitsize = FALSE
)

# Definir dimensões da página em polegadas (28cm x 21cm)
page_width_cm <- 28
page_height_cm <- 21
cm_to_inches <- 0.393701
page_width_in <- page_width_cm * cm_to_inches
page_height_in <- page_height_cm * cm_to_inches
page_aspect <- page_width_cm/page_height_cm

# Salvar com dimensões da página
ggsave(
  "BR_Temp_PPT_amazonas.png", 
  finalPlot, 
  width = page_width_in * 0.8,  # 80% da largura da página
  height = page_height_in * 0.8, # 80% da altura da página
  units = "in",
  dpi = 600,
  bg = "white",
  limitsize = FALSE
)
 