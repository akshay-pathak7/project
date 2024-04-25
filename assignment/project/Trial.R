install.packages("tidyverse")
install.packages("plotly")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("maps")
library(plotly)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(maps)

unicef_indicator_2_V2 <- read_csv("unicef_indicator_2_V2.csv")  
unicef_metadata_V2 <- read_csv("unicef_metadata_V2.csv")

#exercise 1
data_join <- full_join(unicef_indicator_2_V2, unicef_metadata_V2)
data_join <- full_join(unicef_indicator_2_V2, unicef_metadata_V2, by = join_by(continent, year))
data_join <- full_join(unicef_indicator_2_V2, unicef_metadata_V2, by = c("continent", "year"))

# final data
data_join <- unicef_indicator_2_V2 %>%
  full_join(unicef_metadata_V2, by = join_by(continent, year))

map_world <- map_data("world")

# map 1
map_data_join <- full_join(data_join, map_world, by = c("continent" = "region"))
filter(year == 2018)
ggplot(map_data_join) +
  aes(x = long, y = lat, group = group, fill = LifeExpectancy) +
  geom_polygon() 

# map 2
data_join_2018 <- data_join %>%
  filter(year == 2018)

map_data_join_2018 <- full_join(data_join_2018, map_world, by = c("country" = "region"))

ggplot(map_data_join_2018) +
  aes(x = long, y = lat, group = group, fill = LifeExpectancy) +
  geom_polygon() 

# map 3
data_join %>%
  full_join(map_world, by = c("country" = "region")) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = Pop)) +
  geom_polygon()





options(scipen = 999)

# timeseries 

  ggplot(data_join) +
aes(year, LifeExpectancy, group = country, color = continent) +
geom_line()

# timeseries 1
timeseries_plot_1 <- data_join %>%
  ggplot() +
    aes(year, LifeExpectancy, group = country.x, group = country.y, color = continent) +
  geom_line()

ggplotly(timeseries_plot_1, tooltip = "text")

# timeseries 2
timeseries_plot_2 <- data_join %>%
  ggplot() +
  aes(year, Pop, group = country.x, group = country.y, color = continent) +
  geom_line()

ggplotly(timeseries_plot_2, tooltip = "text")

# scatter plot 1
ggplot(data_join) +
  aes(GDPperCapita, obs_value, color = continent, size = Pop) +
  geom_point(alpha = 0.2) +
  labs (
    x = "GDP per Capita in USD",
    y = "Diarrhea Treatment",
    title = "Relationship of GDP per Capita in USD and Diarrhea Treatment"
  ) +
  guides(color = "none", size = "none")
  theme_classic()
  theme(text = element_text(family = "serif"))
  
  #scatter plot 2 
  ggplot(data_join) +
    aes(GDPperCapita, obs_value, color = continent, size = Pop) +
    geom_point(alpha = 0.2) +
    facet_wrap(~ year, nrow = 1) +
    scale_x_continuous(
      limits = c(0, 100000), 
      breaks = c(20000, 50000),
      labels = scales::unit_format(unit = "K", scale = 0.001) 
    ) +
    labs(
      x = "GDP per Capita in USD",
      y = "Diarrhea Treatment",
      title = "Relationship of GDP per Capita in USD and Diarrhea Treatment"
    ) +
    guides(color = "none", size = "none")
  theme_classic()
  theme(text = element_text(family = "serif"))


# bar chart
  data_join %>%
    group_by(continent, year) %>%
    summarise(m_diarrhea_treatment = mean(obs_value, na.rm = TRUE)) %>%
    ggplot() +
    aes(reorder(continent, m_diarrhea_treatment), m_diarrhea_treatment, fill = continent) +
    geom_col() +
  labs(
    x = "Continent",
    y = "Diarrhea Treatment",
    fill = "Continent",
    title = "Insights on Diarrhea Treatment"
  ) +
  theme_classic()
theme(
  text = element_text(family = "serif"),
  axis.text.x = element_blank()
) 


