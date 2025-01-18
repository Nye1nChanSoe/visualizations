library(tidyverse)
library(readxl)
library(ggthemes)

setwd('/Users/nyeinchan/Desktop/Harbour.Space/data_to_knowledge/SESSION_3/data')

ass3 <- read_csv('fertility.csv')

# glimpse data description
glimpse(ass3)



# Exercise 1
# Plot -> Line Plot
# Fertility Rate (FR) in Italy between 1950 - 2021
# annotate the maximum and minimum value within the plot
max_fr = ass3 %>%
  dplyr::filter(FR == max(FR))

min_fr = ass3 %>%
  dplyr::filter(FR == min(FR))

italy_data = ass3 %>%
  dplyr::filter(Entity == "Italy") %>%
  dplyr::filter(Year >= 1950 & Year <= 2021)

ggplot(
  data = italy_data,
  mapping = aes(x = Year, y = FR)) + 
  geom_line(color = "black", size = 1) +
  geom_point(data = max_fr, color = "darkgreen", size = 3) +
  geom_point(data = min_fr, color = "red", size = 3) +
  geom_text(data = max_fr, mapping = aes(label = paste("Max: ", FR)), color = "darkgreen", vjust = 2) +
  geom_text(data = min_fr, mapping = aes(label = paste("Min: ", FR)), color = "red", hjust = 1.2) +
  scale_x_continuous(
    limits = c(1950, NA),
    breaks = seq(1950, max(italy_data$Year), by = 10)
  ) +
  scale_y_continuous(
    limits = c(0, max_fr$FR),
    breaks = seq(0, max_fr$FR, by = 1.5),
  ) +
  labs(title = "Evolution of Fertility Rate in Italty (1950-2021)", x = "Year", y = "Fertility Rate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("./HW_IMAGES/1_lineplot_fr_italy_1950_2021.png", 
  scale = 1,
  height = 8,
  width = 12,
  dpi = 300,
  bg = "white")



# Exercise 2
# Plot -> multiple-line plot
# Fertility Rate in Myanmar, Thailand, Russia, El Salvador 1950-2021
# legend on top-right corner inside the plot

selected_countries <- c("Myanmar", "Thailand", "Russia", "El Salvador")
country_data <- ass3 %>%
  dplyr::filter(Entity %in% selected_countries)

ggplot(
  data = country_data,
  mapping = aes(x = Year, y = FR, color = Entity, group = Entity)) +
  geom_line(size = 1) +
  scale_x_continuous(
    limits = c(1950, NA),
    breaks = seq(1950, max(country_data$Year), by = 10)
  ) +
  scale_y_continuous(
    limits = c(0, max(country_data$FR)),
    breaks = seq(0, max(country_data$FR), by = 1.5),
  ) +
  labs(title = "FR in Myanmar, Thailand, Russia, El Salvador (1950-2021)", x = "Year", y = "Fertility Rate") +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("./HW_IMAGES/2_multiplot_fr_mm_th_ru_slv.png", 
       scale = 1,
       height = 8,
       width = 12,
       dpi = 300,
       bg = "white")



# Exercise 3
# Plot ->  box plot
# Fertility Rate by Continent (C3) in 2020
# order the box-plots according to the median

fertitly_c3_2020 <- ass3 %>%
  dplyr::filter(Year == 2020)

median_values <- fertility_2020_data %>%
  dplyr::group_by(C3) %>%
  dplyr::summarise(median_FR = median(FR), .groups = "drop")

ggplot(
  data = fertitly_c3_2020,
  mapping = aes(x = fct_reorder(C3, FR, median), y = FR)
) + geom_boxplot(
  fill = "lightblue",
  color = "black",
  outlier.color = "red"
) + geom_text(
  data = median_values,
  mapping = aes(x = C3, y = median_FR, label = round(median_FR, 2)),
  vjust = -0.5,
  size = 3.5,
  color = "blue"
) +
  scale_y_continuous(
  limits = c(0.0, max(fertitly_c3_2020$FR)),
  breaks = seq(0.0, max(fertitly_c3_2020$FR), by = 1.5),
) +
  labs(title = "Fertility Rate by Continent (C3) in 2020", x = "Year", y = "Fertility Rate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("./HW_IMAGES/3_boxplot_fr_c3_2020.png",
       scale = 1,
       height = 8,
       width = 12,
       dpi = 300,
       bg = "white")



# Exercise 4
# Plot -> bar plot
# Fertility Rate >= 4.5 in 2021 by continent C4
# order the bars in descendant order with FR labels (two decimals)

fertility_c4_2021 <- ass3 %>%
  dplyr::filter(Year == 2021 & FR >= 4.5)

ggplot(
  data = fertility_c4_2021,
  mapping = aes(x = fct_reorder(Entity, FR, .desc = TRUE), y = FR, fill = C4)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(mapping = aes(label = round(FR, 2)), vjust = -0.5, size = 4) +
  labs(
    title = "Countries with FR >= 4.5 in 2021 by C4",
    x = "Country",
    y = "Fertility Rate",
    fill = "Continent"
  ) + theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)       # Rotate x-axis labels for better readability
  )

ggsave("./HW_IMAGES/4_barplot_fr_c4_2021.png",
       scale = 1,
       height = 8,
       width = 12,
       dpi = 300,
       bg = "white")



# Exercise 5
# Plot -> facet box plot (facet = multiple subplots)
# All countries that end in year 0 or 5
# X-axis: Year, Y-axis: FR
# Use Variable C3 (continent) to produce facets with box plot

# Hint: factor(Year) => to categorize data in R

countries_end_year_0_and_5 <- ass3 %>%
  dplyr::filter(Year %% 5 == 0)

ggplot(
  data = countries_end_year_0_and_5,
  mapping = aes(x = factor(Year), y = FR, fill = factor(Year))
) + 
  geom_boxplot(outlier.color = "red") +
  scale_fill_viridis_d(option = "C") +
  scale_y_continuous(
    limits = c(0.0, max(countries_end_year_0_and_5$FR)),
    breaks = seq(0.0, max(countries_end_year_0_and_5$FR), by = 1.5),
  ) +
  labs(
    title = "Fertility Rates for Years ending in 0 or 5",
    x = "Year",
    y = "Fertility Rate",
    fill = "Year"
  ) +
  facet_wrap(~C3, scales = "free_x") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

ggsave("./HW_IMAGES/5_facet_boxplot_fr_years_ending_0_5.png",
       scale = 1,
       height = 8,
       width = 12,
       bg = "white",
       dpi = 300)



# Exercise 6
# Plot -> grouped bar plot
# Fertility Rate values in 1980, 2000, 2020 for Argentina, Brazil, Chile
# Label with a value of FR inside each bar

grouped_year_entity_data <- ass3 %>%
  dplyr::filter(
    Year %in% c(1980, 2000, 2020),
    Entity %in% c("Argentina", "Brazil", "Chile")
  )

ggplot(
  data = grouped_year_entity_data,
  mapping = aes(x = Entity, y = FR, fill = factor(Year))
) + geom_bar(
  stat = "identity", 
  position = position_dodge(width = 0.9), 
  color = "black"
) + geom_text(
  aes(label = round(FR, 2)),
  position = position_dodge(width = 0.9),
  vjust = -1,
  size = 4
) + scale_fill_viridis_d(option = "C") +
  labs(
    title = "FR values in Argentina, Brazil, Chile in 1980, 2000, 2020",
    x = "Country",
    y = "Fertility Rate",
    fill = "Year"
) + theme_ugly() +
  theme(
    legend.position = c(0.95, 0.9),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("./HW_IMAGES/6_grouped_barplot_fr_year.png",
       scale = 1,
       height = 8,
       width = 12,
       bg = "white",
       dpi = 300)



# Exercise 7
# Theme creation (ugly theme)

theme_ugly <- function() {
  theme(
    # Panel background and grid lines
    panel.background = element_rect(fill = "pink", color = "yellow"),
    panel.grid.major = element_line(color = "orange", size = 1),
    panel.grid.minor = element_line(color = "purple", size = 0.5),
    
    # Plot background
    plot.background = element_rect(fill = "orange", color = "black", linewidth = 3),
    
    # Axis lines and text
    axis.line = element_line(color = "red", size = 2),
    axis.text = element_text(color = "blue", size = 14, face = "bold", angle = 45),
    axis.title = element_text(color = "darkgreen", size = 16, face = "italic"),
    
    # Title and subtitle
    plot.title = element_text(color = "cyan", size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "magenta", size = 16, face = "italic", hjust = 0.5),
    
    # Legend
    legend.background = element_rect(fill = "gray", color = "black"),
    legend.text = element_text(color = "brown", size = 12),
    legend.title = element_text(color = "darkred", size = 14, face = "bold"),
    legend.key = element_rect(fill = "white", color = "darkblue"),
    
    # Facet strips
    strip.background = element_rect(fill = "cyan", color = "black"),
    strip.text = element_text(color = "red", size = 14, face = "bold")
  )
}

