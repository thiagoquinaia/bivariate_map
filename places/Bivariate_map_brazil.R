# Load libraries
library(tidyverse)
library(sf)
library(terra)
library(rgeoboundaries)
library(climateR)
library(biscale)

#install.packages("remotes")
#remotes::install_github("wmgeolab/rgeoboundaries")
#remotes::install_github("mikejohnson51/climateR")

# Load United Kingdom boundary data at the country level (administrative level 0)
br0 <- geoboundaries(country = c("Brazil"))

# Load United Kingdom boundary data at the first administrative level (e.g., regions)
br1 <- geoboundaries(country = c("Brazil"), adm_lvl = "adm1")

# Plot the geometry of administrative level 1 boundaries with a gray border
plot(st_geometry(br1), border = "gray")

# Add the country boundary (administrative level 0) to the existing plot in bold
plot(st_geometry(br0), add = TRUE, size = 2)


# Retrieve TerraClimate data for the United Kingdom with specified variables (max and min temperature)
temp_terra = getTerraClim(AOI = br0, 
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
br_temp <- terra::crop(annual_mean_temp, y = br0, mask = TRUE)

# Plot the cropped annual mean temperature for the Brazil
plot(br_temp)

####### Precipitação ######
# Retrieve TerraClimate data for precipitation in the Brazil
ppt_terra = getTerraClim(AOI = br0, 
                         varname = "ppt",
                         startDate = "1994-01-01",
                         endDate  = "2023-12-01")

# Calculate the annual mean precipitation by averaging the monthly precipitation values across all months
annual_mean_ppt <- mean(ppt_terra$ppt, na.rm = TRUE)

# Resample the annual mean precipitation raster to a finer resolution (approximately 1 km)
new_raster <- rast(ext(annual_mean_ppt), resolution = new_res, crs = crs(annual_mean_ppt))
annual_mean_ppt <- resample(x = annual_mean_ppt, y = new_raster, method = "bilinear")

# Crop the resampled precipitation data to match the boundaries of the United Kingdom
br_ppt <- terra::crop(annual_mean_ppt, y = br0, mask = TRUE)

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
  project(br0) |> 
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

# Create the bivariate map using ggplot2
map <- ggplot() +
  theme_void(base_size = 14) +  # Set a minimal theme for the map
  xlim(-76.44, -32.7) +  # Set the x-axis limits for the map (longitude range)
  ylim(-35.4, 7.9) +  # Set the y-axis limits for the map (latitude range)
  # Plot the bivariate raster data with appropriate fill color based on bivariate classes
  geom_raster(data = data, mapping = aes(x = x, y = y, fill = bi_class), color = NA, linewidth = 0.1, show.legend = FALSE) +
  # Apply the bivariate color scale using the selected palette and dimensions
  bi_scale_fill(pal = pallet, dim = 4, flip_axes = FALSE, rotate_pal = FALSE) +
  # Overlay the first administrative level boundaries of the United Kingdom
  geom_sf(data = br1, fill = NA, color = "white", linewidth = 0.20) +
  # Overlay the country-level boundary of the United Kingdom
  geom_sf(data = br0, fill = NA, color = "black", linewidth = 0.40) +
  # Add labels for the map
  labs(title = "Brasil: Padrões de Temperatura e Precipitação", 
       subtitle = "Média de Temperatura e Precipitação dos últimos 29 anos",
       caption = "Fonte: Terra Climate Data - Autor: Thiago Quinaia") +
  # Customize the appearance of the title, subtitle, and caption
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(size = 9, face = "italic", hjust = 0.5))

# Create the legend for the bivariate map
legend <- bi_legend(pal = pallet,   
                    flip_axes = FALSE,
                    rotate_pal = FALSE,
                    dim = 4,
                    xlab = "Temperatura (Cº)",
                    ylab = "Precipitação (mm)",
                    size = 10,
                    pad_color = "black")

# Combine the map and legend using cowplot
library(cowplot)

# Calcular médias por classe bivariada
class_means <- data %>%
  group_by(bi_class) %>%
  summarise(
    temp_mean = round(max(temp, na.rm = TRUE), 1),
    ppt_mean = round(max(ppt, na.rm = TRUE), 1)
  )

# Verificar ordenação dos dados
print(data %>% 
      select(bi_class, temp, ppt) %>% 
      group_by(bi_class) %>%
      summarise(
        temp_mean = mean(temp),
        ppt_mean = mean(ppt)
      ) %>%
      arrange(bi_class))

# Obter cores da paleta bivariada
bi_colors <- bi_pal(pal = pallet, dim = 4, preview = FALSE)

# Criar dataframe com cores e valores
legend_data <- data.frame(
  bi_class = paste(rep(1:4, each=4), rep(1:4, times=4), sep="-"),
  x = rep(1:4, each=4),
  y = rep(1:4, times=4)
) %>%
  left_join(class_means, by="bi_class") %>%
  mutate(
    fill = bi_colors,
    label = paste0("T:", temp_mean, "°C\nP:", ppt_mean, "mm")
  )

# Criar nova legenda combinada
value_legend <- ggplot(legend_data, aes(x=y, y=x)) +
  geom_tile(aes(fill=fill)) +
  geom_label(aes(label=label), 
             size=2.5, 
             color="white",
             fill="black",
             alpha=0.3,        # Transparência do fundo
             label.padding=unit(0.1, "lines")) +
  scale_fill_identity() +
  coord_equal() +
  theme_void()

# Modificar o finalPlot para incluir a nova legenda
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  #draw_plot(value_legend, 0.05, 0.05, 0.35, 0.35)+
  draw_plot(legend, 0.05, 0.05, 0.35, 0.35)


# Display the final map with legend
finalPlot

ggsave("BR_Temp_PPT.png", finalPlot, dpi = 400, width = 10, height = 7)
