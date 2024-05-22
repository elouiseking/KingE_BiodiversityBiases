#Set up
getwd()

#packages
library(dplyr)
library(ggplot2)
library(terra)
library(geodata)
#install.packages("rnaturalearthdata")
library(rnaturalearthdata)
library(rnaturalearth)
library(car)
library(sf)
library(cowplot)
library(RColorBrewer)

dark2 <- brewer.pal(8, "Dark2")
#import literature data
lit_location <- read.csv("Literature.csv")

#aggregate data by location
location_counts <- lit_location %>%
  group_by(location_short, taxa) %>%
  summarise(count = n())

#load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")
plot(world)

#merge the data
world_lit <- merge(world, location_counts, by.x = "continent", by.y = "location", all.x = TRUE)

#plot the map 
ggplot() +
  geom_sf(data = world_lit, aes(fill = count)) +
  scale_fill_gradient(low = "#ADD8E6", high = "#00008B", na.value = "grey", name = "Count") +
  labs(title = "Geographic spread of published research") +
  theme_minimal()

world_bar <- ggplot(location_counts, aes(x = location_short, y = count, fill = taxa)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = dark2) +
  labs(x = "Location", y = "Number of Research articles") +
  theme_light() + 
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14,  color = "black"),
  axis.text.y = element_text(size = 14, color = "black")) +
  theme(axis.title = element_text(size = 16, color = "black"),   # Set size of axis labels
        axis.line = element_line(color = "black"),   # Set color of axis lines
        plot.title = element_text(size = 16, face = "bold", color = "black")) +
  ggtitle("(a)")

#filter data for Europe
europe_data <- lit_location %>%
  filter(location == "Europe")

#aggregate data by couuntry
country_counts <- europe_data %>%
  group_by(country_code, taxa) %>%
  summarise(count = n())

#plot europe data
europe_bar <- ggplot(country_counts, aes(x = country_code, y = count, fill = taxa)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = dark2) +
  labs(x = "Country", y = "Number of Papers") +
  theme_light() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black")) +
  theme(axis.title = element_text(size = 16, color = "black"),   # Set size of axis labels
        axis.line = element_line(color = "black"),   # Set color of axis lines
        plot.title = element_text(size = 16, face = "bold", color = "black")) +
  ggtitle("(b)")

combined_bars <- plot_grid(world_bar, europe_bar, align = "v", hjust = -1)

ggsave("geographic_literature_bars.png", plot = combined_bars, dpi = 1000, height = 8, width = 16)

#calculations
na <- lit_location %>%
  filter(location == "North America")
na_counts <- na %>%
  group_by(country, taxa) %>%
  summarise(count = n())
sum(na_counts$count)

multiple <- lit_location %>%
  filter(location == "Multiple")
multiple_counts <- multiple %>%
  group_by(country, taxa) %>%
  summarise(count = n())
sum(multiple_counts$count)
