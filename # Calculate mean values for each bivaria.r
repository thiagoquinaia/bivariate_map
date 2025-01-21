# Calculate mean values for each bivariate class
class_means <- data %>%
    group_by(bi_class) %>%
    summarise(
        mean_temp = round(mean(temp, na.rm = TRUE), 1),
        mean_ppt = round(mean(ppt, na.rm = TRUE), 1)
    ) %>%
    arrange(bi_class)

# Create a text table for the legend
text_legend <- data.frame(
    x = rep(1:4, each = 4),
    y = rep(1:4, times = 4),
    label = paste0("T:", class_means$mean_temp, "°C\nP:", class_means$mean_ppt, "mm")
)

# Create value legend
value_legend <- ggplot() +
    theme_void() +
    geom_tile(data = expand.grid(x = 1:4, y = 1:4), 
                        aes(x = x, y = y), fill = "white", color = "black") +
    geom_text(data = text_legend, 
                        aes(x = x, y = y, label = label), 
                        size = 2.5) +
    scale_y_reverse() +
    coord_equal() +
    theme(plot.margin = margin(0,0,0,0))

# Modify the finalPlot to include both legends
finalPlot <- ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.05, 0.05, 0.28, 0.28) +
    draw_plot(value_legend, 0.75, 0.05, 0.20, 0.20)

# Display and save
finalPlot
ggsave("BR_Temp_PPT_with_values.png", finalPlot, dpi = 400, width = 10, height = 7)