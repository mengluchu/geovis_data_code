# ──────────────────────────────────────────────────────────────────────────────
# 1)  LOAD LIBRARIES
# ──────────────────────────────────────────────────────────────────────────────
library(sf)        # For handling spatial (vector) data—not used below but handy
library(dplyr)     # For data wrangling verbs: group_by(), summarise(), …
library(ggplot2)   # For creating elegant plots based on the grammar of graphics

# install.packages("ggplot2")  # Uncomment if ggplot2 is not installed

# ──────────────────────────────────────────────────────────────────────────────
# 2)  READ RAW DATA
# ──────────────────────────────────────────────────────────────────────────────
air = read.csv("~/work/teaching_2025/Geovisualisation/data/DENL17_selected_hr.csv") # change into your own directory! 
#in pc-pool, likely you just do 
#air = read.csv("DENL17_selected_hr.csv")
head(air)          # Quick peek at the first 6 rows to verify import

# ──────────────────────────────────────────────────────────────────────────────
# 3)  TECHNICAL‑STYLE PLOT  (single panel, coloured by country)
# ──────────────────────────────────────────────────────────────────────────────
air_summary = air %>%
  group_by(hours, Countrycode) %>%                       # Aggregate by hour & country
  summarise(mean_NO2 = mean(wkd_hr_value, na.rm = TRUE), # Compute hourly mean NO₂
            .groups = "drop")                            # Drop grouping for next steps

ggplot(air_summary, aes(x = hours, y = mean_NO2,
                        color = Countrycode)) +          # Map colour to country
  geom_line(size = 1.2) +                                # Draw thick line for each country
  geom_hline(yintercept = 25, linetype = "dashed",
             color = "black", size = 1) +                # WHO guideline reference
  scale_color_manual(values = c("DE" = "salmon",
                                "NL" = "cyan4"),
                     labels = c("DE", "NL")) +           # Manual country colours
  labs(title = "Annual Average Hourly NO2 Levels (Germany vs. Netherlands)",
       subtitle = "Includes WHO 1 hour NO2 exposure guidelinee",
       x = "Hour of Day",
       y = expression(NO[2]~"(µg/m"^3*")"),
       color = NULL) +                                   # Remove colour legend title
  annotate("text", x = 23, y = 25.6,
           label = "WHO Guideline (25 µg/m³)",
           hjust = 1, size = 4.5) +                      # Text label for guideline
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")                         # Legend above the plot

ggsave("~/work/teaching_2025/Geovisualisation/lab4_tech.png")
# Exports the technical figure as PNG

# ──────────────────────────────────────────────────────────────────────────────
# 4)  RESIDENCE‑STYLE PLOT  (two facets, rush‑hour shading, risk line)
# ──────────────────────────────────────────────────────────────────────────────
df = air %>%
  group_by(hours, Countrycode) %>%                       # Same aggregation
  summarise(mean_NO2 = mean(wkd_hr_value, na.rm = TRUE),
            .groups = "drop")

rush_rect = tibble(
  xmin = 6, xmax = 6,            # Shade from 06:00–08:00
  ymin = -Inf, ymax = Inf,       # Full height of each panel
  rush = "Rush Hour"             # Legend label
)

ggplot() +                                              # Empty canvas
  geom_rect(data = rush_rect,                           # 1. Shaded rush hour
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = rush),
            alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = df,                                  # 2. Main NO2 line
            aes(x = hours, y = mean_NO2),
            color = "#D73027", size = 1.2) +
  geom_hline(yintercept = 25,                           # 3. WHO guideline
             linetype = "dashed", color = "black",
             size = 1) +
  facet_wrap(~ Countrycode, ncol = 2) +                 # 4. Panels: DE | NL, same as "series" in Rawgraphs
  scale_fill_manual(name = "",                          # 5. Custom legend for shading
                    values = c("Rush Hour" = "gray50")) +
  labs(title = "When Is the Air Most Polluted in Your Country?",
       subtitle = "Hourly NO2 levels — shaded areas indicate morning rush hour",
       x = "Hour of Day",
       y = "NO2 (µg/m³)") +
  annotate("text", x = 23, y = 25.6,                    # Additional annotation
           label = "Risk alert", hjust = 1, size = 4.5) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top",                        # Legend on top
        strip.text = element_text(size = 14, face = "bold"),
        plot.title = element_text(color = "#D73027",
                                  size = 18, face = "bold"))

ggsave("~/work/teaching_2025/Geovisualisation/lab4_res.png")
# Exports the residence‑style figure
