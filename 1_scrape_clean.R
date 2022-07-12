library(glue)
library(httr)
library(rvest)
library(jsonlite)
library(purrr)
library(stringr)
library(dplyr)
library(tibble)
library(readr)

# IMDB info

imdb_key <- "k_z8onhxso"

seasons_episodes <- tribble(~season, ~episodes,
                            1, 13,
                            2, 12,
                            3, 12,
                            4, 13,
                            5, 10)

api_links <- glue("https://imdb-api.com/en/API/SeasonEpisodes/{imdb_key}/tt0306414/{1:5}")

scraped_data <- map(api_links, GET)

scraped_data |> 
  pluck(1) |> 
  parse_json() |> 
  pluck("episodes")

# Wiki info

# "https://en.wikipedia.org/wiki/Law_enforcement_characters_of_The_Wire"
# "https://en.wikipedia.org/wiki/Police_of_The_Wire"
# "https://en.wikipedia.org/wiki/Politicians_of_The_Wire"
# "https://en.wikipedia.org/wiki/Street-level_characters_of_The_Wire"
# "https://en.wikipedia.org/wiki/School_system_of_The_Wire"
# "https://en.wikipedia.org/wiki/Characters_from_the_docks_of_The_Wire"
# "https://en.wikipedia.org/wiki/Journalists_of_The_Wire"

all_characters <- glue("https://thewire.fandom.com/wiki/Category:Characters?from={LETTERS}") |> 
  map(read_html) |> 
  map(html_elements, css = ".category-page__member-link") |>
  flatten() |> 
  map_chr(html_text)

all_characters <- c(all_characters, "Johnny Weeks") |> 
  sort()

no_shows_chars <- c("Anthony Little", "Arnold D. Paulette", "Ashley", "Banisky", "Big Roy", "Brian Baker", "CCO", "Chantay",
                    "Characters from the docks", "Chipper", "Crawford", "Creswich", "Crime Scene Technician", "Darcia Wallace",
                    "Dawkins", "Deirdre Kresson", "Dennis Carpenter", "DeShawn Fredericks", "Edward Bowers", "Frank Barlow", 
                    "Fredo Braddock", "George Smith", "Hoskins", "Hucklebuck", "Jerome Lewandowski", "Jerome Lewandewski", 
                    "Jury Forewoman", "Karim Williams", "Keisha Michaels", "LaTroy", "Little Bunk", "Mackey", "Madame LaRue", 
                    "Marcell", "Marcus Lemmell", "Mau Mau Willis", "Maurice Scroggins", "McCulloh Homes addict", 
                    "Michael \"Little Mike\"", "Moonshot", "Mrs. Bratton", "Mrs. Broadus", "Mugs", "Nadiva Bryant", "Nakeesha Lyles",
                    "Nay Nay", "Omar Betts's Friend", "Peanut", "Pete Dixon", "Poe Homes Homicide Victim", "Pooh Blanchard", 
                    "Priscilla Catlow", "Ringo", "Robert Norris", "Roberto", "Roc Roc", "Roland Leggett", "Roman", "Ronald Watkins",
                    "Roy Brown", "Royce's Assistant", "Salmond", "Sam Choksey", "Scar", "School Lecturer", "School Receptionist",
                    "Security Supervisor", "Shooting Witness", "Snot Boogie", "Sullivan", "SWAT Team Leader", "Taryn Hansen", "Tater",
                    "Tiffany", "Toreen Boyd", "Tote", "Tyrell Barksdale", "Tywanda", "Warren Frazier", "Western District Officer",
                    "William Gant", "Winona", "Wintell Royce", "Young Tony")

changed_names <- c("Anton Artis" = "Strinkum", "Chester Sobotka" = "Ziggy Sobotka", "Curtis Anderson" = "Lex", "Darius Hill" = "O-Dog", "Felicia Pearson" = "Snoop",
  "Jay Landsman (character)" = "Jay Landsman", "Joseph Stewart" = "Prop Joe", "Malik Carr" = "Poot", "Marquis Hilton" = "Bird", 
  "Melvin Wagstaff" = "Cheese", "Michael McArdle" = "White Mike", "Nathaniel Manns" = "Hungry Man", "Preston Broadus" = "Bodie",
  "Reginald Cousins" = "Bubbles", "Ricardo Hendrix" = "Fat-Face Rick", "Roland Brice" = "Wee-Bey", "Russell Bell" = "Stringer", 
  "Shaun McGinty" = "Shamrock", "William Moreland" = "Bunk Moreland") |> 
  enframe()

all_characters <- all_characters |>
  str_subset(paste(no_shows_chars, collapse = "|"), negate = TRUE) |> 
  as_tibble() |> 
  distinct() |> 
  rename(name = value) |> 
  left_join(changed_names) |> 
  mutate(name = if_else(is.na(value), name, value)) |> 
  select(-value) |> 
  mutate(category = NA_character_)

write_csv(all_characters, "character_data.csv")

### EDITED DATA BY HAND ###


## Creates a raw data frame to enter in shiny app
dat <- tribble(~scene_no, ~season, ~episode, ~characters, ~time) |> 
  mutate(across(1:3, as.numeric),
         characters = as.character(characters),
         time = hms::as_hms(time))

saveRDS(dat, "data.RDS")
