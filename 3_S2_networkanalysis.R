library(tidyverse)
library(ggraph)
library(tidygraph)
library(glue)
library(magick)
library(RColorBrewer)
library(tinter)

dat <- readRDS("data.RDS")

characters <- read_csv("characters_data.csv") |> 
  mutate(category = fct_relevel(category, "Law", "Police", "Politician", "Docks", "Gang", "The Greek's Crew", "Stickup", "Civilian"))

dat_clean <- dat |> 
  filter(season == 2) |> 
  select(scene_no, characters, time) |> 
  mutate(characters = str_replace_all(characters, "\\\n", " ")) |> 
  separate_rows(characters, sep = ",")

total_char_time <- dat_clean |> 
  group_by(characters) |> 
  summarize(time = sum(time)) |> 
  rename(name = characters) |> 
  left_join(characters)

chr_combs <- combn(characters$name, 2) |> 
  t() |> 
  as_tibble(.name_repair = "unique") |> 
  rename(name1 = ...1, name2 = ...2) |> 
  left_join(total_char_time, by = c("name1" = "name")) |> 
  left_join(total_char_time, by = c("name2" = "name")) |> 
  mutate(across(starts_with("time"), ~ replace_na(.x, hms::hms(0)))) |> 
  filter(time.x > 0, time.y > 0)

get_screentime <- function(character1, character2) {
  data_extracted <- dat_clean |> 
    group_by(scene_no) |> 
    filter(characters %in% c(character1, character2)) |> 
    filter(n() > 1)
  
  if (nrow(data_extracted > 0)) {
    data_extracted |> 
      mutate(variable = glue("character{row_number()}")) |> 
      ungroup() |> 
      pivot_wider(names_from = variable, values_from = characters) |> 
      drop_na() |> 
      summarize(time = sum(time)) |> 
      pull(time) |> 
      as.numeric()
  } else {
    0
  }
}

chr_comb_times <- mutate(chr_combs, times = map2_dbl(name1, name2, get_screentime))

edges <- chr_comb_times |> 
  filter(times > 0) |> 
  left_join(characters, by = c("name1" = "name")) |> 
  left_join(characters, by = c("name2" = "name")) |> 
  rename(from = id.x, to = id.y) |> 
  select(from, to, times) |> 
  mutate(across(c(from, to), as.character))

nodes <- total_char_time |> 
  rename(characters = name) |> 
  filter(id %in% edges$from | id %in% edges$to) |> 
  mutate(id = as.character(id), total_time = as.numeric(time))

net_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE, node_key = "id")

nodes_colours <- brewer.pal(7, "Set1")
nodes_colours[8] <- "#808080"
nodes_colours[6] <- darken(nodes_colours[6], 0.2)
names(nodes_colours) <- c("Law", "Police", "Politician", "Docks", "Gang", "The Greek's Crew", "Stickup", "Civilian")
# text_colours <- darken(nodes_colours, 0.5)

img <- image_read("www/TheWire-Logo_CR.jpg")

graph_s2 <- ggraph(net_tidy) + 
  geom_edge_link(aes(width = times), alpha = 0.8, colour = "lightgrey", show.legend = FALSE) + 
  geom_node_point(aes(size = total_time, fill = category), colour = "white", shape = 21) +
  geom_node_text(aes(label = characters, colour = category), repel = TRUE, fontface = "bold", show.legend = FALSE) +
  scale_edge_width(range = c(0.001, 5)) +
  scale_size(range = c(1, 15)) +
  scale_fill_manual(values = nodes_colours) +
  scale_colour_manual(values = text_colours) +
  labs(fill = "", colour = "", subtitle = "Season Two") +
  guides(size = "none", fill = guide_legend(nrow = 1, override.aes = list(size = 10))) +
  theme_graph() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  annotation_raster(img,
                    xmin = -2, xmax = -0,
                    ymin = -2.75, ymax = -1.75)

ggsave("season2.png", graph_s1, device = "png", dpi = 450, width = 13, height = 10, units = "in", scale = 1.3)
