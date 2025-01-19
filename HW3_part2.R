library(tidyverse)

setwd('/Users/nyeinchan/Desktop/Harbour.Space/data_to_knowledge/SESSION_3/data')

spain_data <- read_csv("data_s2.csv")

glimpse(spain_data)


# Exercise 1
# Plot -> facet donut-plot
# Percentage of Spanish and foreign born of COM (region) in 2016

spain_data_2016 <- spain_data %>%
  dplyr::filter(YEAR == 2016)

spain_summary_data_2016 <- spain_data_2016 %>%
  dplyr::group_by(COM) %>%
  dplyr::summarise(
    TOTAL_SPANISH_BORN = sum(POP_SPANISH),
    TOTAL_FOREIGN_BORN = sum(POP_LATINAMERICA + POP_WESTERNEUROPE
      + POP_EASTERNEUROPE + POP_AFRICA + POP_ASIA + POP_OTHERS 
      )
  ) %>%
  dplyr::mutate(
    SPANISH_BORN_PERC = TOTAL_SPANISH_BORN / (TOTAL_SPANISH_BORN + TOTAL_FOREIGN_BORN) * 100,
    FOREIGN_BORN_PERC = TOTAL_FOREIGN_BORN / (TOTAL_SPANISH_BORN + TOTAL_FOREIGN_BORN) * 100
  )
  
# transform data for plotting (wide to long format)
# COM    | Population   | Percentage
# region | Spanish-born | 90.12
donut_plot_data <- spain_summary_data_2016 %>%
  dplyr::select(COM, SPANISH_BORN_PERC, FOREIGN_BORN_PERC) %>%
  tidyr::pivot_longer(ends_with("PERC"), names_to = "Population", values_to = "Percentage") %>%
  dplyr::mutate(Population = recode(Population,
                                    "SPANISH_BORN_PERC" = "Spanish-born",
                                    "FOREIGN_BORN_PERC" = "Foreign-born"))

ggplot(
  data = donut_plot_data,
  mapping = aes(x = "", y = Percentage, fill = Population)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~COM) +
  geom_text(
    mapping = aes(label = paste0(round(Percentage, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3
  ) +
  labs(
    title = "Population in Spain by Region 2016",
    fill = "Population Type"
  ) + theme_void() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 12, t = 6)),
  )

ggsave("./HW_IMAGES_2/1_facet_donutplot_spain_foreign_perc_2016.png",
       scale = 1,
       height = 8,
       width = 12,
       bg = "white",
       dpi = 300)



# Exercise 2
# Plot -> joy plot
# filter e0M.csv with countries from countries.csv
# x-axis: values, y-axis: time intervals

setwd('/Users/nyeinchan/Desktop/Harbour.Space/data_to_knowledge/SESSION_4/data')

country_data <- read_csv("countries.csv")
e0M_data <- read_csv("e0M.csv")

glimpse(country_data)
glimpse(e0M_data)

country_data <- country_data %>%
  dplyr::rename(country = x)
e0M_data <- e0M_data %>%
  dplyr::filter(country %in% country_data$country)


# reshape from wide to long format
# Wide: Each year interval 1950-1955, 2010-2015 are in separate columns
# Long: Each year interval becomes a value in single column "TIME"
#       Another column holding the corresponding value "VALUE"
e0M_long <- e0M_data %>%
  tidyr::pivot_longer(
    cols = c(starts_with("19"), starts_with("20")),
    names_to = "TIME",
    values_to = "VALUE"
  )

install.packages("ggridges")
library(ggridges)

ggplot(
  data = e0M_long,
  mapping = aes(x = VALUE, y = as.factor(TIME), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Life Expectancy", option = "C") +
  scale_x_continuous(
    name = "Life Expectancy",
    breaks = seq(0, 90, 10)
  ) +
  labs(
    title = "Life Expectancy Over Time Intervals",
    x = "Life Expectancy",
    y = "Time Intervals"
  ) +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  )

ggsave("./HW_IMAGES/2_joyplot_ridges_life_expectancy_year.png",
       scale = 1,
       height = 8,
       width = 12,
       bg = "white",
       dpi = 300)



# Exercise 3
# Plot -> faceted lollipop plot
# Proportion of HS01, HS12, HS13, HS14 (person counts) in each country (C2)
# Express proportions as percentage:
# Hint: labels = scales::percent (in scale_x_continuous)

CoDB_2021_data <- read_csv("CoDB_2021.csv")

glimpse(CoDB_2021_data)


# reshape from wide to long format
# Wide: Cols for household types HS01, HS12, HS13, HS14
# Long: Household_type and Proportion

country_and_household <- CoDB_2021_data %>%
  dplyr::select(C2, HS01, HS12, HS13, HS14)

# check col names
colnames(country_and_household)

CoDB_2021_long <- country_and_household %>%
  tidyr::pivot_longer(
    cols = starts_with("HS"),
    names_to = "Household_Type",
    values_to = "Proportion"
  )

ggplot(
  data = CoDB_2021_long,
  mapping = aes(x = Proportion, y = C2, color = Household_Type)) +
  geom_segment(
    mapping = aes(x = 0, xend = Proportion, y = C2, yend = C2), 
    size = 0.5
  ) +
  geom_point(size = 3) + # lollipop head size
  facet_wrap(~Household_Type, scales = "fixed") +
  scale_x_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, 0.05)
  ) +
  labs(
    title = "Household proportions by type and country",
    x = "Proportion (%)",
    y = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

ggsave("./HW_IMAGES/3_lollipop_plot_hs_country.png",
       scale = 1,
       height = 8,
       width = 12,
       bg = "white",
       dpi = 300)



# Exercise 4
# Plot -> smoothed dot plot
# select cols from 2-6 and HS01-HS11
# keep for C3 (LATIN-AMERICA) if S2 is (IPUMS)
# use C4 for faceting
# x-axis: T1
# y-axis: values of HS01:HS11

CoDB_NDS_data <- read_csv("CoDB_NDS.csv")

glimpse(CoDB_NDS_data)
colnames(CoDB_NDS_data)

filtered_data <- CoDB_NDS_data %>%
  dplyr::filter(
    (C3 == "LATIN-AMERICA" & S2 == "IPUMS") |
    (C3 != "LATIN-AMERICA")
  ) %>%
  dplyr::select(2:6, HS01:HS11)

# reshape from wide to long format
# each row => household_type and value
reshaped_data <- filtered_data %>%
  tidyr::pivot_longer(
    cols = starts_with("HS"),
    names_to = "Household_Type",
    values_to = "Value"
  )

ggplot(
  data = reshaped_data,
  mapping = aes(x = T1, y = Value, color = Household_Type)) +
  geom_point(size = 1.5, alpha = 0.6) +
  # smooth trend line for each household type
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
  facet_wrap(~C4, scales = "free_y") +
  labs(
    title = "Smoothed Dot Plot of Household Proportions by subcontinent",
    x = "Year",
    y = "Household Proportion",
    color = "Household_Type"
  ) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(min(reshaped_data$T1), max(reshaped_data$T1), 10)
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis_d(option = "D", direction = -1, name = "Household Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold")
  )

ggsave("./HW_IMAGES/4_smooth_dotplot_hs_subcontinent.png",
       scale = 1,
       height = 8,
       width = 12,
       bg = "white",
       dpi = 300)
    