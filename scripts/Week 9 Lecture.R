# Week 9: Lecture 9: Interactive Shinylive 1 ----

# story here: Lakers as a basketball team
# how much of an impact had a new player have

## load the packages and data ----

library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)

luka_trade_date <- as.Date("2025-02-01")

zone_order <- c(
  "Restricted Area",
  "In The Paint (Non-RA)",
  "Mid-Range",
  "Left Corner 3",
  "Right Corner 3",
  "Above the Break 3"
)

lakers_shots <- readRDS("data/cleaned/lakers_shots.rds") |>
  mutate(
    game_date = as.Date(game_date),
    era = factor(
      if_else(game_date < luka_trade_date, "AD Era", "Luka Era"),
      levels = c("AD Era", "Luka Era")
    )
  ) |>
  filter(shot_zone_basic %in% zone_order) |>
  mutate(shot_zone_basic = factor(shot_zone_basic, levels = zone_order))

## static plot ----

# test with just rui
rui_by_zone <- lakers_shots |>
  filter(player_name == "Rui Hachimura") |>
  count(era, shot_zone_basic) |>
  group_by(era) |>
  mutate(share = n / sum(n)) |>
  ungroup()

rui_by_zone

# plot it
lakers_colours <- c("AD Era" = "#FDB927", "Luka Era" = "#552583")

ggplot(rui_by_zone,
       aes(x = shot_zone_basic, y = share, fill = era)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = lakers_colours) +
  labs(
    title    = "Rui Hachimura: shot distribution by zone",
    subtitle = "Share of attempts from each zone, AD Era vs Luka Era",
    x = NULL, y = "Share of attempts", fill = NULL,
    caption  = "Data: NBA Stats. AD Era = before 1 Feb 2025, Luka Era = from 1 Feb 2025."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "top",
    plot.title         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 20, hjust = 1),
    panel.grid.major.x = element_blank()
  )

## plotly ----
rui_plot <- ggplot(rui_by_zone,
                   aes(x = shot_zone_basic, y = share, fill = era,
                       text = paste0(
                         era, "<br>", # <br> = space/ indent
                         shot_zone_basic, "<br>", 
                         "Attempts: ", n, "<br>",
                         "Share: ", round(share * 100, 1), "%"
                       ))) + #this text equals define to show when hover over mouse
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = lakers_colours) +
  labs(fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 20, hjust = 1))

ggplotly(rui_plot, tooltip = "text") #text refers to parameter before


## clean for shiny ----
shot_zones <- lakers_shots |>
  count(player_name, era, shot_zone_basic) |>
  group_by(player_name, era) |>
  mutate(share = n / sum(n)) |>
  ungroup()

shot_zones

saveRDS(shot_zones, "app/shot_zones.rds")

# independent practice ----

## 3. FG% per Zone ----
lakers_shots <- readRDS("data/cleaned/lakers_shots.rds")

shot_zones_fg <- lakers_shots |>
  group_by(player_name, era, shot_zone_basic) |>
  summarise(
    n      = n(),
    fg_pct = mean(shot_made_flag),   # makes field-goal %
    .groups = "drop"
  )

shot_zones_fg
saveRDS(shot_zones_fg, "app/shot_zones_fg.rds")

## 4. add checkboxInput in the UI called "made_only"
lakers_shots <- readRDS("data/cleaned/lakers_shots.rds")

shot_zones_made <- lakers_shots |>
  count(player_name, era, shot_zone_basic, shot_made_flag) |>  # keep made/missed split
  group_by(player_name, era) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  mutate(made = shot_made_flag == 1L)

shot_zones_made
saveRDS(shot_zones_made, "app/shot_zones_made.rds")

## what was in app from lecture 9, shinylive I
# rshiny ----

## library and data ----
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)

lakers_colours <- c("AD Era" = "#FDB927", "Luka Era" = "#552583")

shot_zones <- readRDS("shot_zones.rds")

player_choices <- sort(unique(shot_zones$player_name))

## minimal
ui <- fluidPage(
  titlePanel("Lakers Evolution Dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        inputId  = "player",
        label    = "Choose a player",
        choices  = player_choices,
        selected = "Rui Hachimura"
      ),
      p("Hover over a bar to see the exact share and number of attempts.")
    ),
    mainPanel(
      width = 9,
      plotlyOutput("zone_bars", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  output$zone_bars <- renderPlotly({
    plot_data <- shot_zones |>
      filter(player_name == input$player)
    
    gg <- ggplot(plot_data,
                 aes(x = shot_zone_basic, y = share, fill = era,
                     text = paste0(
                       era, "<br>",
                       shot_zone_basic, "<br>",
                       "Attempts: ", n, "<br>",
                       "Share: ", round(share * 100, 1), "%"
                     ))) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_fill_manual(values = lakers_colours) +
      labs(title = paste0(input$player, ": shot distribution by zone"),
           x = NULL, y = "Share of attempts", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 20, hjust = 1))
    
    ggplotly(gg, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)

shiny::runApp("app")

# push it to github (check in workbook, chapter 2)


# independent practice

## 1. change selected player ----
ui <- fluidPage(
  titlePanel("Lakers Evolution Dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        inputId  = "player",
        label    = "Choose a player",
        choices  = player_choices,
        selected = "Austin Reaves" #changed this here
      ),
      p("Hover over a bar to see the exact share and number of attempts.")
    ),
    mainPanel(
      width = 9,
      plotlyOutput("zone_bars", height = "500px")
    )
  )
)


# server: stays the same
shinyApp(ui = ui, server = server)

shiny::runApp("app")

## 2. swap share for the raw attempt count (n) on the y-axis ----

# ui stays the same

# change server
server <- function(input, output, session) {
  output$zone_bars <- renderPlotly({
    plot_data <- shot_zones |>
      filter(player_name == input$player)
    
    gg <- ggplot(plot_data,
                 aes(x = shot_zone_basic, y = n, fill = era,   # from y = share to y = n
                     text = paste0(
                       era, "<br>",
                       shot_zone_basic, "<br>",
                       "Attempts: ", n, "<br>",
                       "Share: ", round(share * 100, 1), "%"
                     ))) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      scale_y_continuous() +  # <-- no percent formatter
      scale_fill_manual(values = lakers_colours) +
      labs(title = paste0(input$player, ": shot attempts by zone"),
           x = NULL, y = "Attempts", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 20, hjust = 1))
    
    ggplotly(gg, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)

shiny::runApp("app")

# compare what the two versions emphasise
# share: controls for volume: fairer comparison
# n: looks at raw volume

## 3. compute FG% per zone instead of share ----
# rebuild the pre-aggregation step from lakers_shots 
# using summarise(fg_pct = mean(shot_made_flag)), 
# save that as a new RDS and plot it;

shot_zones_fg <- readRDS("app/shot_zones_fg.rds")

player_choices <- sort(unique(shot_zones_fg$player_name))

# ui: same

# server:
server <- function(input, output, session) {
  output$zone_bars <- renderPlotly({
    plot_data <- shot_zones_fg |>
      filter(player_name == input$player)
    
    gg <- ggplot(plot_data,
                 aes(x = shot_zone_basic, y = fg_pct, fill = era,   # from y = share to y = fg_pct
                     text = paste0(
                       era, "<br>",
                       shot_zone_basic, "<br>",
                       "Attempts: ", n, "<br>",
                       "fg_pct: ", round(fg_pct * 100, 1), "%"
                     ))) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                         limits = c(0, 1)) +
      scale_fill_manual(values = lakers_colours) +
      labs(title = paste0(input$player, ": shot attempts by zone"),
           x = NULL, y = "FG%", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 20, hjust = 1))
    
    ggplotly(gg, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)

shiny::runApp("app")

## 4. add a checkboxInput in the UI called "made_only"
# you will need to rebuild the pre-aggregation with a made flag
# so the app can switch between “all shots” and “made only”;
shot_zones_made <- readRDS("app/shot_zones_made.rds")

player_choices <- sort(unique(shot_zones_made$player_name))

# UI
ui <- fluidPage(
  titlePanel("Lakers Evolution Dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("player", "Choose a player",
                  choices = player_choices, selected = "Rui Hachimura"),
      checkboxInput("made_only", "Made shots only", value = FALSE),  # new
      p("Hover over a bar to see the exact share and number of attempts.")
    ),
    mainPanel(width = 9, plotlyOutput("zone_bars", height = "500px"))
  )
)

# server
server <- function(input, output, session) {
  output$zone_bars <- renderPlotly({
    
    plot_data <- shot_zones |>
      filter(player_name == input$player)
    
    # apply the checkbox filter
    if (input$made_only) {
      plot_data <- plot_data |> filter(made == TRUE)
    } else {
      plot_data <- plot_data |>
        group_by(player_name, era, shot_zone_basic) |>
        summarise(n = sum(n), share = sum(share), .groups = "drop")
    }
    
    gg <- ggplot(plot_data,
                 aes(x = shot_zone_basic, y = share, fill = era,
                     text = paste0(
                       era, "<br>",
                       shot_zone_basic, "<br>",
                       "Attempts: ", n, "<br>",
                       "Share: ", round(share * 100, 1), "%"
                     ))) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_fill_manual(values = lakers_colours) +
      labs(
        title = paste0(input$player, ": shot distribution by zone",
                       if (input$made_only) " (made only)" else ""),
        x = NULL, y = "Share of attempts", fill = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 20, hjust = 1))
    
    ggplotly(gg, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)

shiny::runApp("app")

## 5. change the colour palette and the title to match a different team
warriors_colours <- c("AD Era" = "#FFC72C", "XY Era" = "#1D428A")

# server
shinyApp(ui = ui, server = server)

shiny::runApp("app")
