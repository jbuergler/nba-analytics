library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(bslib)

lakers_colours  <- c("AD Era" = "#FDB927", "Luka Era" = "#552583")
luka_trade_date <- as.Date("2025-02-01")

lakers_shots   <- readRDS("lakers_shots.rds")
player_choices <- sort(unique(lakers_shots$player_name))

action_family_of <- function(x) {
  case_when(
    grepl("Dunk",     x, ignore.case = TRUE) ~ "Dunk",
    grepl("Layup",    x, ignore.case = TRUE) ~ "Layup",
    grepl("Fadeaway", x, ignore.case = TRUE) ~ "Fadeaway",
    grepl("Jump",     x, ignore.case = TRUE) ~ "Jump Shot",
    TRUE                                     ~ "Other"
  )
}

lakers_theme <- bs_theme(
  version   = 5,
  primary   = "#552583",
  secondary = "#FDB927",
  success   = "#2E7D32",
  base_font = font_collection(
    "-apple-system",
    "BlinkMacSystemFont",
    "Segoe UI",
    "Helvetica Neue",
    "Arial",
    "sans-serif"
  )
)

vb_title <- function(x) tags$span(style = "font-size: 0.8rem;",  x)
vb_value <- function(x) tags$span(style = "font-size: 1.3rem; font-weight: 600;", x)
vb_delta <- function(x) tags$span(style = "font-size: 0.75rem;", x)
vb_icon  <- function(name) tags$span(style = "font-size: 0.4rem;",
                                     bsicons::bs_icon(name))

ui <- page_sidebar(
  title   = "Lakers Evolution Dashboard",
  theme   = lakers_theme,
  sidebar = sidebar(
    width = 200,
    selectInput("player", "Player",
                choices  = player_choices,
                selected = "Rui Hachimura"),
    p("Value boxes compare the Luka Era to the AD Era.
       The left card splits shots by court zone.
       The right card is an interactive tree map — hover any tile
       for attempts, share, eFG% and the change vs the other era.")
  ),
  # --- Pattern C: headline numbers with era deltas (slim) ---
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(title    = vb_title("eFG% (efficiency)"),
              value    = vb_value(textOutput("efg_value", inline = TRUE)),
              vb_delta(textOutput("efg_delta", inline = TRUE)),
              showcase = vb_icon("bullseye"),
              theme    = "primary"),
    value_box(title    = vb_title("xeFG% (shot quality)"),
              value    = vb_value(textOutput("xefg_value", inline = TRUE)),
              vb_delta(textOutput("xefg_delta", inline = TRUE)),
              showcase = vb_icon("map"),
              theme    = "secondary"),
    value_box(title    = vb_title("eFG% − xeFG% (shot-making)"),
              value    = vb_value(textOutput("diff_value", inline = TRUE)),
              vb_delta(textOutput("diff_delta", inline = TRUE)),
              showcase = vb_icon("graph-up"),
              theme    = "success")
  ),
  # --- Pattern D: two cards side by side ---
  layout_columns(
    col_widths = c(6, 6),
    # Pattern B: zone — Volume + Efficiency in a tabbed card
    navset_card_tab(
      title = "Shot Selection by Zone",
      full_screen = TRUE,
      nav_panel("Volume",     plotlyOutput("zone_bars", height = "320px")),
      nav_panel("Efficiency", plotlyOutput("zone_eff",  height = "320px"))
    ),
    # Action families — single-chart card, tree map of original action_types
    card(
      card_header("Action Families — Efficiency"),
      full_screen = TRUE,
      tags$small(style = "color: #666; padding: 0 0.75rem;",
                 "Tile size = share of attempts within each era, ",
                 "tile colour = eFG%. Hover for details."),
      plotlyOutput("type_tree_efficiency", height = "320px")
    )
  )
)

server <- function(input, output, session) {
  player_shots <- reactive({
    lakers_shots |> filter(player_name == input$player)
  })
  # --- Era metrics (Sketch 4) ---
  era_metrics <- reactive({
    player_shots() |>
      group_by(era) |>
      summarise(efg  = sum(shot_made_flag * shot_value) / (2 * n()),
                xefg = mean(xefg_baseline),
                diff = efg - xefg,
                .groups = "drop") |>
      pivot_wider(names_from = era,
                  values_from = c(efg, xefg, diff))
  })
  fmt_pct   <- function(x) paste0(round(x * 100, 1), "%")
  fmt_pp    <- function(x) paste0(if (x >= 0) "+" else "",
                                  round(x * 100, 1), " pp")
  fmt_delta <- function(luka, ad) {
    d <- luka - ad
    arrow <- if (d > 0) "▲" else if (d < 0) "▼" else "—"
    paste0(arrow, " ", fmt_pp(d), " vs AD (", fmt_pct(ad), ")")
  }
  # Headline values = Luka Era
  output$efg_value  <- renderText({ fmt_pct(era_metrics()$`efg_Luka Era`)  })
  output$xefg_value <- renderText({ fmt_pct(era_metrics()$`xefg_Luka Era`) })
  output$diff_value <- renderText({ fmt_pp(era_metrics()$`diff_Luka Era`)  })
  # Delta lines = Luka − AD
  output$efg_delta  <- renderText({
    m <- era_metrics(); fmt_delta(m$`efg_Luka Era`,  m$`efg_AD Era`)
  })
  output$xefg_delta <- renderText({
    m <- era_metrics(); fmt_delta(m$`xefg_Luka Era`, m$`xefg_AD Era`)
  })
  output$diff_delta <- renderText({
    m <- era_metrics(); fmt_delta(m$`diff_Luka Era`, m$`diff_AD Era`)
  })
  # --- Volume: share of attempts per zone (Sketch 1) ---
  output$zone_bars <- renderPlotly({
    plot_data <- player_shots() |>
      count(era, shot_zone_basic) |>
      group_by(era) |>
      mutate(share = n / sum(n)) |>
      ungroup()
    gg <- ggplot(plot_data,
                 aes(x = shot_zone_basic, y = share, fill = era,
                     text = paste0(era, "<br>", shot_zone_basic, "<br>",
                                   "Attempts: ", n, "<br>",
                                   "Share: ", round(share * 100, 1), "%"))) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_fill_manual(values = lakers_colours) +
      labs(x = NULL, y = "Share of attempts", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 20, hjust = 1))
    ggplotly(gg, tooltip = "text")
  })
  # --- Efficiency: eFG% per zone (Sketch 2) ---
  output$zone_eff <- renderPlotly({
    plot_data <- player_shots() |>
      group_by(era, shot_zone_basic) |>
      summarise(efg      = sum(shot_made_flag * shot_value) / (2 * n()),
                attempts = n(),
                .groups  = "drop")
    gg <- ggplot(plot_data,
                 aes(x = shot_zone_basic, y = efg, fill = era,
                     text = paste0(era, "<br>", shot_zone_basic, "<br>",
                                   "Attempts: ", attempts, "<br>",
                                   "eFG%: ", round(efg * 100, 1), "%"))) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_fill_manual(values = lakers_colours) +
      labs(x = NULL, y = "eFG%", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 20, hjust = 1))
    ggplotly(gg, tooltip = "text")
  })
  # --- Action-family long table, one row per (era × family × action_type).
  #     proportion is the share of attempts *within each era*, so the two
  #     era sub-trees stay comparable even when one has fewer total shots. ---
  family_data <- reactive({
    player_shots() |>
      mutate(action_family = action_family_of(action_type)) |>
      group_by(era, action_family, action_type) |>
      summarise(attempts = n(),
                efg      = sum(shot_made_flag * shot_value) / (2 * n()),
                .groups  = "drop") |>
      group_by(era) |>
      mutate(proportion = attempts / sum(attempts)) |>
      ungroup()
  })
  # --- Build one treemap-node table per era: families at the root,
  #     original action_types as leaves. The era label is handled as a
  #     subplot title (not as a tile) so the drill-down starts at family. ---
  era_tree_nodes <- reactive({
    d <- family_data()
    wide <- d |>
      mutate(era_key = if_else(era == "AD Era", "ad", "luka")) |>
      select(-era) |>
      pivot_wider(id_cols     = c(action_family, action_type),
                  names_from  = era_key,
                  values_from = c(attempts, proportion, efg))
    enriched <- d |>
      left_join(wide, by = c("action_family", "action_type")) |>
      mutate(
        other_era        = if_else(era == "AD Era", "Luka Era", "AD Era"),
        proportion_other = if_else(era == "AD Era",
                                   coalesce(proportion_luka, 0),
                                   coalesce(proportion_ad,   0)),
        efg_other        = if_else(era == "AD Era", efg_luka, efg_ad),
        proportion_delta = proportion - proportion_other,
        efg_delta        = efg - efg_other
      )
    sign_pp <- function(x) paste0(if_else(x >= 0, "+", ""),
                                  round(x * 100, 1), " pp")
    build_one <- function(era_name) {
      e <- enriched |> filter(era == era_name)
      if (nrow(e) == 0) return(NULL)
      leaves <- e |>
        transmute(
          id      = paste(action_family, action_type, sep = "/"),
          label   = action_type,
          parent  = action_family,
          value   = proportion,
          efg_val = efg,
          hover   = paste0(
            "<b>", action_type, "</b><br>",
            "Attempts: ", attempts, "<br>",
            "Share: ", round(proportion * 100, 1), "%",
            "  (", sign_pp(proportion_delta), " vs other era)<br>",
            "eFG%: ", round(efg * 100, 1), "%",
            if_else(!is.na(efg_delta),
                    paste0("  (", sign_pp(efg_delta), " vs other era)"),
                    "")
          )
        )
      families <- e |>
        group_by(action_family) |>
        summarise(attempts_total   = sum(attempts),
                  proportion_total = sum(proportion),
                  efg_val          = weighted.mean(efg, attempts,
                                                   na.rm = TRUE),
                  .groups          = "drop") |>
        transmute(
          id      = action_family,
          label   = action_family,
          parent  = "",
          value   = proportion_total,
          efg_val = efg_val,
          hover   = paste0("<b>", action_family, "</b><br>",
                           "Attempts: ", attempts_total, "<br>",
                           "Share: ", round(proportion_total * 100, 1), "%<br>",
                           "eFG%: ", round(efg_val * 100, 1), "%")
        )
      bind_rows(families, leaves)
    }
    list(ad = build_one("AD Era"), luka = build_one("Luka Era"))
  })
  # --- Tree map: Efficiency.
  #     Two traces (AD Era, Luka Era) side by side with era as the title;
  #     tile area = share of attempts within that era;
  #     tile colour = eFG% (shared gradient); default labels show on tiles
  #     big enough to hold them, full detail is on hover. ---
  output$type_tree_efficiency <- renderPlotly({
    trees <- era_tree_nodes()
    validate(need(!is.null(trees$ad) || !is.null(trees$luka),
                  "No shots to show for this player."))
    efg_marker <- function(efg_vals) list(
      colors     = efg_vals,
      colorscale = "Viridis",
      cmin       = 0,
      cmax       = 1,
      showscale  = TRUE,
      colorbar   = list(title = list(text = "eFG%"),
                        tickformat = ".0%")
    )
    add_era_trace <- function(p, nodes, column) {
      if (is.null(nodes)) return(p)
      add_trace(p,
                type          = "treemap",
                ids           = nodes$id,
                labels        = nodes$label,
                parents       = nodes$parent,
                values        = nodes$value,
                branchvalues  = "total",
                textinfo      = "label",
                text          = nodes$hover,
                hovertemplate = "%{text}<extra></extra>",
                marker        = efg_marker(nodes$efg_val),
                domain        = list(column = column)
      )
    }
    annotations <- list(
      list(text = "<b>AD Era</b>",
           x = 0.22, y = 1.0, xref = "paper", yref = "paper",
           xanchor = "center", yanchor = "bottom",
           showarrow = FALSE, font = list(size = 13)),
      list(text = "<b>Luka Era</b>",
           x = 0.78, y = 1.0, xref = "paper", yref = "paper",
           xanchor = "center", yanchor = "bottom",
           showarrow = FALSE, font = list(size = 13))
    )
    plot_ly() |>
      add_era_trace(trees$ad,   0) |>
      add_era_trace(trees$luka, 1) |>
      layout(
        grid        = list(columns = 2, rows = 1),
        annotations = annotations,
        margin      = list(t = 60, l = 5, r = 5, b = 5)
      )
  })
}

shinyApp(ui = ui, server = server)