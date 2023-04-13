library(tidyverse)
library(ggraph)
library(tidygraph)
library(glue)
library(magick)
library(RColorBrewer)
library(tinter)

dat <- readRDS("data.RDS")

characters <- read_csv("characters_data.csv") |> 
  mutate(category = case_when(
    name %in% c("Prop Joe", "Cheese Wagstaff", "Tree", "Fat-Face Rick", "Phil Boy") ~ "Other Gang",
    name %in% c("Marlo Stanfield", "Fruit", "Justin", "Jamal", "Snoop", "Chris Partlow") ~ "Marlo Crew",
    category == "Gang" ~ "Barksdale Crew",
    name %in% c("Grace Sampson", "Jeffrey Price") ~ "Other",
    TRUE ~ category),
    category = fct_relevel(category, "Law", "Police", "Politician", "Barksdale Crew", "Marlo Crew", "Other Gang", "Stickup", "Addict", "Civilian"))

dat_clean <- dat |> 
  filter(season == 3) |>
  select(scene_no, characters, time) |> 
  mutate(characters = str_replace_all(characters, "\\\n", " ")) |> 
  separate_rows(characters, sep = ",") |> 
  filter(characters != "")

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
  mutate(id = as.character(id),
         total_time = as.numeric(time),
         characters = fct_reorder(characters, time))

net_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE, node_key = "id")

nodes_colours <- brewer.pal(9, "Set1")
nodes_colours[5] <- darken(nodes_colours[4], 0.4)
nodes_colours[6] <- lighten(nodes_colours[4], 0.3)
nodes_colours[9] <- "#808080"

names(nodes_colours) <- c("Law", "Police", "Politician", "Barksdale Crew", "Marlo Crew", "Other Gang", "Stickup", "Addict", "Civilian")
text_colours <- darken(nodes_colours, 0.5)

img <- image_read("www/TheWire-Logo_CR.png")

graph_s3 <- ggraph(net_tidy) + 
  geom_edge_link(aes(width = times), alpha = 0.8, colour = "lightgrey", show.legend = FALSE) + 
  geom_node_point(aes(size = total_time, fill = category), colour = "white", shape = 21) +
  geom_node_text(aes(label = characters, colour = category), repel = TRUE, fontface = "bold", show.legend = FALSE) +
  scale_edge_width(range = c(0.001, 5)) +
  scale_size(range = c(1, 15)) +
  scale_fill_manual(values = nodes_colours) +
  scale_colour_manual(values = text_colours) +
  labs(fill = "", colour = "", subtitle = "Season Three", caption = "/u/AllezCannes") +
  guides(size = "none", fill = guide_legend(nrow = 1, override.aes = list(size = 10))) +
  theme_graph() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  annotation_raster(img, xmin = -2.7, xmax = 0,
                    ymin = -3.3, ymax = -2.5)

ggsave("season3.png", graph_s3, device = "png", dpi = 450, width = 13, height = 10, units = "in", scale = 1.3)
