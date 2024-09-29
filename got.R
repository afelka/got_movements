#read libraries
library(sf)
library(ggplot2)
library(readxl)
library(dplyr)
library(ggimage)
library(magick)
library(ggtext)

#read different shp files. 
#credit
#link: https://www.cartographersguild.com/showthread.php?t=30472
#creator: cadaei https://www.cartographersguild.com/member.php?u=95244
got_map <- read_sf("continents.shp")

got_map_locations <- read_sf("locations.shp")

got_map_wall <- read_sf("wall.shp")

got_rivers <- read_sf("rivers.shp")

got_roads <- read_sf("roads.shp")

got_lakes <- read_sf("lakes.shp")

got_islands <- read_sf("islands.shp")

got_landscape <- read_sf("landscape.shp")

got_forest <- got_landscape %>% filter(type == 'forest')

#read xlsx files for got characters movements (only Tyrion for time being)
got_characters_movement <- read_xlsx("got_movements.xlsx")

#filter Tyrion
tyrions_movements <- got_characters_movement %>% filter(character == 'Tyrion Lannister') %>% mutate(order = row_number())

#group Tyrion's movements to find first and last season/episode he has spent in a given location
tyrion_visited <- got_map_locations %>% inner_join(tyrions_movements, by = c("name" = "location")) %>%
                  arrange(order) %>% 
                  mutate(lag_name = lag(name, default = '')) %>%
                  mutate(different_location = ifelse(lag_name != name , 1, 0)) %>%
                  mutate(count_of_locations = cumsum(different_location)) %>% 
                  group_by(count_of_locations, name, character) %>%
                  summarise(first_season = first(season_no),
                            last_season = last(season_no),
                            first_episode = first(episode_no),
                            last_episode = last(episode_no),
                            count_of_episodes = n()) %>% ungroup()

#bounding box for the map 
bbox <- sf::st_bbox(got_map$geometry)

#create theme for ggplot
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5,),
  plot.subtitle = element_text(hjust = 0.5,)
)

#create ggplots for each movement
for (i in 1:nrow(tyrion_visited)) {
  
  selected_row <- tyrion_visited %>% slice(i) %>% mutate(count = 1, img_tyrion = "./tyrion.jpg")

  tyrion_movement <- ggplot(got_map) +
  geom_sf() +
  geom_sf(data = got_map_wall, color = "darkblue", linewidth  = 3) +
  geom_sf(data = got_islands) +
  geom_sf(data = got_forest, color = 'darkgreen', fill = 'darkgreen') +
  geom_sf(data = got_lakes, color ='lightblue', fill = 'lightblue') +
  geom_sf(data = got_roads, color = 'orange') +
  geom_sf(data = got_rivers, color = 'lightblue') +
  geom_sf(data = selected_row, color = "red") +
  geom_point(data = selected_row, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]), 
               shape = 22, size = 5, color = "red") +  # Custom point shape
  geom_image(data = selected_row , aes(x = 50, y = 46 , group = 0, image=img_tyrion), size= .15, position = "identity") +
    ggtitle(
      paste0(selected_row$character, " is in <span style='color:red;'>", 
             selected_row$name, "</span> <br> from Season ", 
             selected_row$first_season, " Episode ", 
             selected_row$first_episode, " to Season ", 
             selected_row$last_season, " Episode ", 
             selected_row$last_episode)
    )  +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  plain +
    theme(plot.title = element_markdown())

#create dymanic naming
plot_name <- paste0("tyrion_", i, ".png")

# Save the ggplot as a PNG file
ggsave(plot_name, plot = tyrion_movement, width = 6, height = 4, dpi = 300)

}


#create data_frame with images
image_data <- data.frame(
  image_id = 1:nrow(tyrion_visited),
  image_path = paste0("./tyrion_", 1:nrow(tyrion_visited), ".png")
)

# read images in a list
img_list <- lapply(image_data$image_path, image_read)

# join the images together
img_joined <- image_join(img_list)

# animate at 0.5 frames per second
img_animated <- image_animate(img_joined, fps = 0.5)

#write the gif
image_write(image = img_animated,
            path = "tyrion_movement.gif")