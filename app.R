library(shiny)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(hms)

seasons_episodes <- tribble(~season, ~episodes,
                            1, 13,
                            2, 12,
                            3, 12,
                            4, 13,
                            5, 10)

characters <- read_csv("characters_data.csv")

dat <- readRDS("data.RDS")

ui <- fluidPage(
  useShinyjs(),
  column(5,
         fluidRow(img(src = 'TheWire-Logo_CR.jpg', align = "left", height = "50%", width = "50%")),
         pickerInput("season", "Season", choices = 1:5, selected = 3),
         uiOutput("episode_ui"),
         hr(),
         textOutput("timer"),
         hr(),
         actionButton("start", "Start"),
         actionButton("stop", "Stop"),
         actionButton("reset", "Reset time"),
         hr(),
         pickerInput(
           inputId = "characters",
           label = "Characters", 
           choices = characters$name,
           multiple = TRUE,
           options = list(`live-search` = TRUE)),
         actionButton("reset2", "Reset characters"),
         hr(),
         actionButton("enter", "Enter"),
         actionButton("reset_all", "Reset all")
  ),
  column(7, 
         dataTableOutput("Table"),
         actionButton("save", "Save"))
)

server <- function(input, output, session) {
  store <- reactiveValues(timer_start = NULL, time_acc = 0, dat = dat)
  
  timer <- reactiveVal(0)
  
  active <- reactiveVal(FALSE)
  
  output$episode_ui <- renderUI({
    episode <- seasons_episodes |> 
      filter(season == input$season) |> 
      pull(episodes)
    
    pickerInput("episode", "Episode", choices = seq_len(episode))
  })
  
  observeEvent(input$start, {
    store$timer_start <- Sys.time()
    active(TRUE)
  })
  
  observeEvent(input$stop, {
    store$time_acc <- (Sys.time() - store$timer_start) + store$time_acc
    active(FALSE)
  })
  
  observe({
    invalidateLater(1000, session)
    isolate({
      if (active()) {
        timer(timer() + 1)
      }
    })
  })
  
  observeEvent(input$reset, {
    store$timer_start <- NULL
    timer(0)
    store$time_acc <- 0
  })
  
  observeEvent(input$reset2, {
    reset("characters")
  })
  
  observeEvent(input$reset_all, {
    store$timer_start <- NULL
    timer(0)
    store$time_acc <- 0
    reset("characters")
  })
  
  output$timer <- renderText({
    paste("Time taken: ", timer(), " seconds")
  })
  
  observeEvent(input$enter, {
    store$dat <- add_row(store$dat, .before = 1,
                         scene_no = nrow(store$dat) + 1,
                         season = as.numeric(input$season),
                         episode = as.numeric(input$episode),
                         characters = str_wrap(paste(input$characters, collapse = ","), 30),
                         time = as_hms(store$time_acc))
  })
  
  output$Table <- renderDataTable({
    store$dat |> 
      mutate(time = round_hms(time, secs = 1)) |> 
      datatable()
  })
  
  observeEvent(input$save, {
    store$dat |> 
      filter(time > 0) |> 
      mutate(characters = str_replace_all(characters, "\\n", " ")) |> 
      saveRDS("data.RDS")
  })
}

shinyApp(ui, server)