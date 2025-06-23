

library(sf)        # For handling spatial (vector) data
library(dplyr)
library(ggplot2)   # For creating elegant plots 
# install ggplot2 if using it the first time
#install.packages ("ggplot2")


air <- read.csv("work/teaching_2025/Geovisualisation/data/DENL17_selected_hr.csv")
head(air)

#For technical person 
air %>% group_by(hours, Countrycode) %>%
  summarise(mean_NO2 = mean(wkd_hr_value, na.rm = TRUE)) %>%
  ggplot(aes(x = hours, y = mean_NO2)) +
  geom_line(color = "steelblue") +
  facet_wrap(~Countrycode , scales = "free_y") +
  labs(title = "Annual average hourly NO2 Levels in Germany and NL",
       x = "Hours", y = "NO2 (microgram/cubic meters)") +
  theme_bw(base_size = 12)
ggsave("~/work/teaching_2025/Geovisualisation/lab4_tech.png")
# code explaination: 
# facet_wrap  = series in Rawgraphs 
# Geom_line = line chart in Rawgraphs 
#group_by and summarise allows you to summarise over groups, here by hours and by country, mean() means we are taking the average, na.rm = TRUE means we remove NA/missing values. 


# For residence 

# Step 1: Summarise to obtain the annual hourly average per country
df <- air %>%
  group_by(hours, Countrycode) %>%
  summarise(mean_NO2 = mean(wkd_hr_value, na.rm = TRUE), .groups = "drop")

# Step 2: Define rush hour shading area (for legend)
rush_rect <- tibble(
  xmin = 7,
  xmax = 9,
  ymin = -Inf,
  ymax = Inf,
  rush = "Rush Hour"
)

# Step 3: Plot
ggplot() +
  # Rush hour background
  geom_rect(data = rush_rect,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = rush),
            inherit.aes = FALSE,
            alpha = 0.15) +
  
  # Main data line
  geom_line(data = df, aes(x = hours, y = mean_NO2), color = "#D73027", size = 1.2) +
  
  # Facet by country
  facet_wrap(~Countrycode, ncol = 2, scales = "free_y") +
  
  # Custom fill legend
  scale_fill_manual(name = "", values = c("Rush Hour" = "gray50")) +
  
  # Labels and theme
  labs(
    title = "When Is the Air Most Polluted in Your Country?",
    subtitle = "Hourly NO2 levels — shaded areas indicate morning rush hour",
    x = "Hour of Day",
    y = "NO2 (µg/m³)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(color = "#D73027", size = 18, face = "bold")
  )

ggsave("~/work/teaching_2025/Geovisualisation/lab4_res.png")
