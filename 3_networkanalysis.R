library(tidyverse)
library(ggraph)
library(tidygraph)
library(glue)

dat <- readRDS("data.RDS")

characters <- read_csv("characters_data.csv")

dat_clean <- dat |> 
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

ggraph(net_tidy, layout = "kk") + #graphopt or kk
  geom_edge_link(aes(width = times), alpha = 0.8, colour = "lightgrey") + 
  geom_node_point(aes(size = total_time, colour = category)) +
  geom_node_text(aes(label = characters, colour = category), repel = TRUE, fontface = "bold", show.legend = FALSE) +
  scale_edge_width(range = c(0.1, 5)) +
  scale_size(range = c(1, 15)) +
  labs(edge_width = "times") +
  theme_graph()
