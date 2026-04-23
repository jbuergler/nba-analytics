## Week 9 Seminar

# football e.g xG for particular player/ teams or other variables (widgets)
# treemap, bar chart of 40 categories, proportion of football event
# barchart difficult to tell what happens
# how they scored
# why did you decide this, what story does it tell
# justify choices, reflect and evaluate
# data limitation
# he wants like a dashboard he showed in the seminar
# recreate: prepare the data
# code in a different file, then in the end: saveRDS into the App

# libraries and data ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shiny)
library(bslib)
install.packages("treemapify")
library(treemapify)

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

shot_baseline <- lakers_shots |>
  group_by(shot_zone_basic, action_type) |>
  summarise(xefg_baseline = sum(shot_made_flag * shot_value) / (2 * n()),
            .groups = "drop")

lakers_shots <- lakers_shots |>
  left_join(shot_baseline, by = c("shot_zone_basic", "action_type"))

saveRDS(lakers_shots, "app/lakers_shots.rds")

# static visuals ----
lakers_colours <- c("AD Era" = "#FDB927", "Luka Era" = "#552583")

rui_by_zone <- lakers_shots |>
  filter(player_name == "Rui Hachimura") |>
  count(era, shot_zone_basic) |>
  group_by(era) |>
  mutate(share = n / sum(n)) |>
  ungroup()

## Sketch 1: Shot-Zone Bar Chart ----
ggplot(rui_by_zone, aes(x = shot_zone_basic, y = share, fill = era)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = lakers_colours) +
  labs(x = NULL, y = "Share of attempts", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 20, hjust = 1))

## Sketch 2: Zone Efficiency (Volume’s Companion) ----
rui_zone_eff <- lakers_shots |>
  filter(player_name == "Rui Hachimura") |>
  group_by(era, shot_zone_basic) |>
  summarise(efg      = sum(shot_made_flag * shot_value) / (2 * n()),
            attempts = n(),
            .groups  = "drop")

ggplot(rui_zone_eff, aes(x = shot_zone_basic, y = efg, fill = era)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = lakers_colours) +
  labs(x = NULL, y = "eFG%", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 20, hjust = 1))

## Sketch 3: Action Families as Tree Maps ----
# Step 1 — the family function.
action_family_of <- function(x) {
  dplyr::case_when(
    grepl("Dunk",     x, ignore.case = TRUE) ~ "Dunk",
    grepl("Layup",    x, ignore.case = TRUE) ~ "Layup",
    grepl("Fadeaway", x, ignore.case = TRUE) ~ "Fadeaway",
    grepl("Jump",     x, ignore.case = TRUE) ~ "Jump Shot",
    TRUE                                     ~ "Other"
  )
}

# Step 2 — the per-era, per-action table
rui_family <- lakers_shots |>
  filter(player_name == "Rui Hachimura") |>
  mutate(action_family = action_family_of(action_type)) |>
  group_by(era, action_family, action_type) |>
  summarise(attempts = n(),
            efg      = sum(shot_made_flag * shot_value) / (2 * n()),
            .groups  = "drop") |>
  group_by(era) |>
  mutate(proportion = attempts / sum(attempts)) |>
  ungroup()

rui_family

# Step 3 — the static sketch
ggplot(rui_family,
       aes(area    = proportion,
           fill    = efg,
           label   = action_type,
           subgroup = action_family)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 3) +
  geom_treemap_text(colour = "white", place = "centre",
                    grow = FALSE, reflow = TRUE, min.size = 6) +
  scale_fill_viridis_c(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1), name = "eFG%") +
  facet_wrap(~ era) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

# Sketch 4: The Three Headline Numbers, Per Era
rui_era_metrics <- lakers_shots |>
  filter(player_name == "Rui Hachimura") |>
  group_by(era) |>
  summarise(attempts = n(),
            efg      = sum(shot_made_flag * shot_value) / (2 * n()),
            xefg     = mean(xefg_baseline),
            diff     = efg - xefg,
            .groups  = "drop")

rui_era_metrics

## Build Dashboard ----
## Step 1 — Swap to page_sidebar() + card() ----
ui <- page_sidebar(
  title   = "Lakers Evolution Dashboard",
  sidebar = sidebar(
    width = 260,
    selectInput("player", "Player",
                choices = player_choices, selected = "Rui Hachimura")
  ),
  card(
    card_header("Shot Distribution by Zone"),
    plotlyOutput("zone_bars", height = "440px")
  )
)

## Step 2 — Turn the Card Into a Tabbed Card ----
navset_card_tab(
  title = "Shot Selection by Zone",
  full_screen = TRUE,
  nav_panel("Volume",     plotlyOutput("zone_bars", height = "360px")),
  nav_panel("Efficiency", plotlyOutput("zone_eff",  height = "360px"))
)

## Step 3 — Add a Headline Row of Value Boxes ----
layout_columns(
  col_widths = c(4, 4, 4),
  value_box(title    = "eFG%",
            value    = textOutput("efg_value"),
            textOutput("efg_delta"),
            showcase = bsicons::bs_icon("bullseye"),
            theme    = "primary"),
  value_box(title    = "xeFG% (shot quality)",
            value    = textOutput("xefg_value"),
            textOutput("xefg_delta"),
            showcase = bsicons::bs_icon("map"),
            theme    = "secondary"),
  value_box(title    = "eFG% − xeFG% (shot-making)",
            value    = textOutput("diff_value"),
            textOutput("diff_delta"),
            showcase = bsicons::bs_icon("graph-up"),
            theme    = "success")
)

## Step 4 — Add the Action-Family Tree Map Side by Side ----
layout_columns(
  col_widths = c(6, 6),
  navset_card_tab(
    title = "Shot Selection by Zone",
    full_screen = TRUE,
    nav_panel("Volume",     plotlyOutput("zone_bars", height = "320px")),
    nav_panel("Efficiency", plotlyOutput("zone_eff",  height = "320px"))
  ),
  card(
    card_header("Action Families — Efficiency"),
    full_screen = TRUE,
    tags$small(style = "color: #666; padding: 0 0.75rem;",
               "Tile size = share of attempts within each era, ",
               "tile colour = eFG%. Hover for details."),
    plotlyOutput("type_tree_efficiency", height = "320px")
  )
)

## Step 5 — Theme and Polish ----
lakers_theme <- bs_theme(
  version   = 5,
  primary   = "#552583",
  secondary = "#FDB927",
  success   = "#2E7D32",
  base_font = font_google("Inter")
)

vb_title <- function(x) tags$span(style = "font-size: 0.8rem;",  x)
vb_value <- function(x) tags$span(style = "font-size: 1.3rem; font-weight: 600;", x)
vb_delta <- function(x) tags$span(style = "font-size: 0.75rem;", x)
vb_icon  <- function(name) tags$span(style = "font-size: 0.4rem;",
                                     bsicons::bs_icon(name))

# in page_sidebar: theme = lakers_theme, sidebar = sidebar(width = 200, ...)
# in value_box:    wrap title/value/delta/showcase with the helpers above
