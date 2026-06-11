# shiny/app.R ─────────────────────────────────────────────────────────────────
# Cholera Outbreak Explorer
#
# Launch from project root:
#   shiny::runApp("shiny")
#
# Requires (install once):
#   install.packages(c("leaflet", "plotly", "bsicons", "leaflet.extras"))
# ─────────────────────────────────────────────────────────────────────────────

# ── 0. Check hard dependencies ────────────────────────────────────────────────
needed  <- c("leaflet", "plotly", "bslib", "bsicons")
missing <- needed[!sapply(needed, requireNamespace, quietly = TRUE)]
if (length(missing) > 0)
  stop(paste0("Missing packages — run:\n  install.packages(c(",
              paste0('"', missing, '"', collapse = ", "), "))"),
       call. = FALSE)

# ── 1. Libraries ──────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(leaflet)
  library(leaflet.extras)
  library(plotly)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(stringr)
  library(lubridate)
  library(forcats)
  library(rnaturalearth)
  library(sf)
  library(here)
})

# ── 2. Load & pre-process data (runs once at startup) ─────────────────────────
message("[ 1/4 ] Loading combined_outbreaks_cholera.csv ...")

new_raw <- read.csv(
  here("analysis/generated_data/combined_outbreaks_cholera.csv"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    TL   = as.Date(TL),
    TR   = as.Date(TR),
    year = year(TL),
    # Normalise country name
    country_name = case_when(
      str_detect(country, "\\.") ~ str_replace(country, "^[A-Z0-9:]+\\.", ""),
      country_iso3 == "TZA::Mainland" ~ "Tanzania (Mainland)",
      country_iso3 == "TZA::Zanzibar" ~ "Tanzania (Zanzibar)",
      TRUE ~ country
    ),
    # Merge TZA sub-territories for join compatibility
    country_iso3_cmp = ifelse(str_detect(country_iso3, "TZA"), "TZA", country_iso3)
  )

# Scale normalisation (mirrors notebook)
normalise_scale <- function(x) {
  case_when(
    x == "country"           ~ "Country",
    str_detect(x, "^admin1") ~ "Admin 1",
    str_detect(x, "^admin2") ~ "Admin 2",
    str_detect(x, "^admin3") ~ "Admin 3",
    TRUE                     ~ NA_character_
  )
}

message("[ 2/4 ] Deriving outbreak summaries ...")
new_ob_admin <- new_raw %>%
  filter(outbreak_number > 0) %>%
  mutate(scale = normalise_scale(spatial_scale)) %>%
  # TZA special case: Mainland / Zanzibar are ADM0 analogues (union territories),
  # not true provinces.  Shift every TZA scale down by one level so that:
  #   admin1 (Mainland / Zanzibar)  → "Country"
  #   admin2 (regions, e.g. Arusha) → "Admin 1"
  #   admin3 (districts)            → "Admin 2"
  mutate(scale = case_when(
    str_detect(country_iso3, "^TZA::") & scale == "Admin 1" ~ "Country",
    str_detect(country_iso3, "^TZA::") & scale == "Admin 2" ~ "Admin 1",
    str_detect(country_iso3, "^TZA::") & scale == "Admin 3" ~ "Admin 2",
    TRUE ~ scale
  )) %>%
  filter(!is.na(scale)) %>%
  group_by(location, scale, country_iso3, who_region,
           run_id, time_lower_bound, time_upper_bound, outbreak_number) %>%
  summarise(
    ob_start     = min(TL),
    ob_end       = max(TR),
    total_cases  = sum(sCh,    na.rm = TRUE),
    total_deaths = sum(deaths, na.rm = TRUE),
    n_weeks      = n(),
    pop          = first(pop),
    peak_cases   = max(sCh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    year          = year(ob_start),
    cfr_pct       = ifelse(total_cases > 0 & !is.na(total_deaths) & total_deaths > 0,
                           total_deaths / total_cases * 100, NA_real_),
    attack_rate   = ifelse(!is.na(pop) & pop > 0, total_cases / pop * 1e3, NA_real_),
    country_iso3_cmp = ifelse(str_detect(country_iso3, "TZA"), "TZA", country_iso3),
    scale         = factor(scale, levels = c("Country", "Admin 1", "Admin 2", "Admin 3"))
  )

# Country name lookup (one canonical name per ISO3-cmp)
name_lut <- new_raw %>%
  mutate(cname = case_when(
    str_detect(country_iso3, "TZA") ~ "Tanzania",
    str_detect(country, "\\.")      ~ str_replace(country, "^[A-Z0-9:]+\\.", ""),
    TRUE                            ~ country
  )) %>%
  filter(str_detect(cname, "[a-z]")) %>%
  distinct(country_iso3_cmp, .keep_all = FALSE) %>%  # just the keys
  left_join(
    new_raw %>%
      mutate(cname = case_when(
        str_detect(country_iso3, "TZA") ~ "Tanzania",
        str_detect(country, "\\.")      ~ str_replace(country, "^[A-Z0-9:]+\\.", ""),
        TRUE                            ~ country
      )) %>%
      filter(str_detect(cname, "[a-z]")) %>%
      distinct(country_iso3_cmp, cname),
    by = "country_iso3_cmp"
  ) %>%
  rename(country_name = cname) %>%
  group_by(country_iso3_cmp) %>%
  slice(1) %>%
  ungroup()

message("[ 3/4 ] Loading spatial data ...")

# Centroid lookup (built by data_prep.R)
centroids <- tryCatch(
  readRDS(here("shiny/data/centroids.rds")),
  error = function(e) {
    message("  [WARN] centroids.rds not found — run `Rscript shiny/data_prep.R` first. ",
            "Admin markers will be unavailable until then.")
    tibble(location = character(), lon = numeric(), lat = numeric())
  }
)

# World polygons (country outlines)
world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3, name_long, continent)

message("[ 4/4 ] Ready.")

# ── 3. App-wide constants ─────────────────────────────────────────────────────
REGIONS <- sort(unique(new_ob_admin$who_region))

REGION_LABELS <- c(
  AFR  = "African Region (AFR)",
  AMR  = "Americas (AMR)",
  EMR  = "Eastern Mediterranean (EMR)",
  SEAR = "South-East Asia (SEAR)"
)

# Matches Set2 palette used throughout the notebook
REGION_COLOURS <- c(
  AFR  = "#66C2A5",
  AMR  = "#FC8D62",
  EMR  = "#8DA0CB",
  SEAR = "#E78AC3"
)

all_countries <- new_ob_admin %>%
  distinct(country_iso3_cmp, who_region) %>%
  left_join(name_lut, by = "country_iso3_cmp") %>%
  mutate(label = coalesce(country_name, country_iso3_cmp)) %>%
  arrange(label)

ALL_YEARS <- range(new_ob_admin$year, na.rm = TRUE)

SCALE_LEVELS <- c("Country", "Admin 1", "Admin 2", "Admin 3")

# ── 4. UI ─────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = tagList(
    bs_icon("virus2", size = "1.1em", class = "me-2"),
    "Cholera Outbreak Explorer  2010–2024"
  ),
  theme = bs_theme(
    bootswatch = "flatly",
    base_font  = font_google("Source Sans Pro")
  ),

  # ── Sidebar ─────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 285,
    open  = TRUE,
    title = "Filters",

    checkboxGroupInput(
      "who_region", "WHO Region",
      choices  = setNames(REGIONS, REGION_LABELS[REGIONS]),
      selected = REGIONS
    ),

    selectizeInput(
      "countries", "Countries",
      choices  = setNames(all_countries$country_iso3_cmp, all_countries$label),
      selected = all_countries$country_iso3_cmp,
      multiple = TRUE,
      options  = list(
        plugins         = list("remove_button"),
        placeholder     = "Select countries …",
        maxOptions      = 200
      )
    ),

    radioButtons(
      "admin_level", "Admin level",
      choices  = SCALE_LEVELS,
      selected = "Country"
    ),

    sliderInput(
      "year_range", "Year range",
      min = ALL_YEARS[1], max = ALL_YEARS[2],
      value = ALL_YEARS, step = 1, sep = ""
    ),

    numericInput(
      "min_cases", "Min. cases per outbreak",
      value = 0, min = 0, step = 10
    ),

    hr(style = "margin: 8px 0"),

    actionButton(
      "reset_filters", "Reset all filters",
      icon  = icon("rotate-left"),
      class = "btn-sm btn-outline-secondary w-100"
    ),

    hr(style = "margin: 8px 0"),
    p(tags$small(tags$em(
      "Click a country / marker on the map to show its weekly time series below."
    )), style = "color: #888; font-size: 0.8em;")
  ),

  # ── KPI value boxes ──────────────────────────────────────────────────────────
  layout_columns(
    fill = FALSE,
    col_widths = c(3, 3, 3, 3),

    value_box(
      title    = "Outbreaks",
      value    = textOutput("kpi_ob",     inline = TRUE),
      showcase = bs_icon("virus"),
      theme    = "primary"
    ),
    value_box(
      title    = "Total cases",
      value    = textOutput("kpi_cases",  inline = TRUE),
      showcase = bs_icon("people-fill"),
      theme    = "info"
    ),
    value_box(
      title    = "Total deaths",
      value    = textOutput("kpi_deaths", inline = TRUE),
      showcase = bs_icon("heartbreak-fill"),
      theme    = "danger"
    ),
    value_box(
      title    = "Locations affected",
      value    = textOutput("kpi_locs",   inline = TRUE),
      showcase = bs_icon("geo-alt-fill"),
      theme    = "success"
    )
  ),

  # ── Main tabs ────────────────────────────────────────────────────────────────
  navset_card_underline(

    # ── Tab 1: Map ─────────────────────────────────────────────────────────────
    nav_panel(
      title = tagList(bs_icon("map"), " Map"),
      leafletOutput("map", height = "460px"),
      hr(style = "margin: 5px 0"),
      div(
        style = "color: #555; font-size: 0.85em; margin-bottom: 3px;",
        textOutput("ts_header", inline = TRUE)
      ),
      plotlyOutput("ts_plot", height = "195px")
    ),

    # ── Tab 2: Statistics ──────────────────────────────────────────────────────
    nav_panel(
      title = tagList(bs_icon("table"), " Statistics"),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Outbreak characteristics by country"),
          full_screen = TRUE,
          DTOutput("stats_table")
        ),
        card(
          card_header("Top 15 locations by total cases"),
          plotOutput("top_locs_plot", height = "420px")
        )
      )
    ),

    # ── Tab 3: Heatmap ─────────────────────────────────────────────────────────
    nav_panel(
      title = tagList(bs_icon("grid-3x3-gap-fill"), " Heatmap"),
      layout_columns(
        col_widths = c(2, 10),
        card(
          card_header("Options"),
          radioButtons(
            "heatmap_metric", "Metric",
            choices  = c("Outbreaks (n)" = "n_outbreaks",
                         "Total cases"   = "total_cases"),
            selected = "n_outbreaks"
          ),
          hr(style = "margin: 6px 0"),
          p(tags$small("Rows sorted by total at the selected admin level."),
            style = "color:#888; font-size:0.8em;")
        ),
        card(
          card_header(textOutput("heatmap_title", inline = TRUE)),
          full_screen = TRUE,
          plotlyOutput("heatmap", height = "530px")
        )
      )
    )
  )
)

# ── 5. Server ─────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Country choices follow WHO region selection ────────────────────────────
  observeEvent(input$who_region, {
    sub  <- all_countries %>% filter(who_region %in% input$who_region)
    keep <- intersect(input$countries, sub$country_iso3_cmp)
    if (length(keep) == 0) keep <- sub$country_iso3_cmp
    updateSelectizeInput(
      session, "countries",
      choices  = setNames(sub$country_iso3_cmp, sub$label),
      selected = keep
    )
  }, ignoreInit = TRUE)

  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "who_region", selected = REGIONS)
    updateSelectizeInput(
      session, "countries",
      choices  = setNames(all_countries$country_iso3_cmp, all_countries$label),
      selected = all_countries$country_iso3_cmp
    )
    updateRadioButtons(session, "admin_level", selected = "Country")
    updateSliderInput(session, "year_range", value = ALL_YEARS)
    updateNumericInput(session, "min_cases", value = 0)
    selected_loc(NULL)
  })

  # ── Core reactive: filtered outbreak data ──────────────────────────────────
  filtered_ob <- reactive({
    req(length(input$who_region) > 0, length(input$countries) > 0)
    new_ob_admin %>%
      filter(
        who_region       %in% input$who_region,
        country_iso3_cmp %in% input$countries,
        as.character(scale) == input$admin_level,
        year             >= input$year_range[1],
        year             <= input$year_range[2],
        total_cases      >= input$min_cases
      )
  })

  # ── KPIs ──────────────────────────────────────────────────────────────────
  output$kpi_ob     <- renderText(format(nrow(filtered_ob()),                           big.mark = ","))
  output$kpi_cases  <- renderText(format(sum(filtered_ob()$total_cases,  na.rm = TRUE), big.mark = ","))
  output$kpi_deaths <- renderText(format(sum(filtered_ob()$total_deaths, na.rm = TRUE), big.mark = ","))
  output$kpi_locs   <- renderText(format(n_distinct(filtered_ob()$location),            big.mark = ","))

  # ── Map ───────────────────────────────────────────────────────────────────
  selected_loc <- reactiveVal(NULL)   # iso_a3 (country) or location string (admin)

  # Per-location aggregation for the map
  map_sum <- reactive({
    filtered_ob() %>%
      group_by(location, country_iso3_cmp, who_region) %>%
      summarise(
        n_outbreaks  = n(),
        total_cases  = sum(total_cases,  na.rm = TRUE),
        total_deaths = sum(total_deaths, na.rm = TRUE),
        cfr_pct      = ifelse(sum(total_cases, na.rm = TRUE) > 0,
                              sum(total_deaths, na.rm = TRUE) /
                              sum(total_cases,  na.rm = TRUE) * 100, NA_real_),
        med_attack   = median(attack_rate, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(name_lut, by = "country_iso3_cmp")
  })

  # Initialise the leaflet widget once
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = tileOptions(maxZoom = 12)) %>%
      setView(lng = 18, lat = 3, zoom = 3)
  })

  # Update map layers when filters / admin level change
  observe({
    ms    <- map_sum()
    level <- input$admin_level
    proxy <- leafletProxy("map") %>%
      clearShapes() %>% clearMarkers() %>% clearControls()

    if (nrow(ms) == 0) return()

    if (level == "Country") {
      # ── Choropleth ─────────────────────────────────────────────────────────
      map_sf <- world_sf %>%
        left_join(
          ms %>% group_by(country_iso3_cmp) %>%
            summarise(
              n_outbreaks  = sum(n_outbreaks,  na.rm = TRUE),
              total_cases  = sum(total_cases,  na.rm = TRUE),
              total_deaths = sum(total_deaths, na.rm = TRUE),
              cfr_pct      = ifelse(sum(total_cases, na.rm = TRUE) > 0,
                                    sum(total_deaths, na.rm = TRUE) /
                                    sum(total_cases,  na.rm = TRUE) * 100,
                                    NA_real_),
              who_region   = first(who_region),
              country_name = first(country_name),
              .groups = "drop"
            ),
          by = c("iso_a3" = "country_iso3_cmp")
        )

      has_data  <- !is.na(map_sf$n_outbreaks)
      pal_choro <- colorNumeric("viridis", domain = ms$n_outbreaks, na.color = "#e4e4e4")

      proxy %>%
        addPolygons(
          data        = map_sf[!has_data, ],
          fillColor   = "#e8e8e8", fillOpacity = 0.4,
          color = "#cccccc", weight = 0.5, options = pathOptions(interactive = FALSE)
        ) %>%
        addPolygons(
          data             = map_sf[has_data, ],
          fillColor        = ~pal_choro(n_outbreaks),
          fillOpacity      = 0.78,
          color            = "white", weight = 0.8,
          highlightOptions = highlightOptions(
            weight = 2.5, color = "#444", fillOpacity = 0.95, bringToFront = TRUE
          ),
          popup = ~paste0(
            "<b>", name_long, "</b><br>",
            "<span style='color:#777'>", who_region, "</span><br>",
            "Outbreaks: <b>", n_outbreaks, "</b><br>",
            "Cases: <b>",     format(total_cases, big.mark = ","), "</b><br>",
            "Deaths: <b>",    ifelse(total_deaths > 0,
                                     format(total_deaths, big.mark = ","), "—"), "</b><br>",
            "CFR: <b>",       ifelse(is.na(cfr_pct) | cfr_pct == 0, "—",
                                     sprintf("%.1f%%", cfr_pct)), "</b>"
          ),
          layerId = ~iso_a3
        ) %>%
        addLegend(
          pal      = pal_choro,
          values   = ms$n_outbreaks,
          title    = "Outbreaks (n)",
          position = "bottomright",
          opacity  = 0.9
        )

    } else {
      # ── Admin circle markers ────────────────────────────────────────────────
      ms_pts <- ms %>%
        inner_join(centroids, by = "location") %>%
        filter(is.finite(lon), is.finite(lat))

      # Grey base outlines
      proxy %>%
        addPolygons(
          data    = world_sf,
          fillColor   = "#f4f4f4", fillOpacity = 0.5,
          color   = "#bbbbbb", weight = 0.5,
          options = pathOptions(interactive = FALSE)
        )

      if (nrow(ms_pts) == 0) {
        showNotification(
          tagList(icon("triangle-exclamation"),
                  " No centroid data — run: Rscript shiny/data_prep.R"),
          type = "warning", duration = 8
        )
        return()
      }

      pal_region <- colorFactor(
        palette = unname(REGION_COLOURS),
        domain  = names(REGION_COLOURS)
      )

      proxy %>%
        addCircleMarkers(
          data        = ms_pts,
          lng = ~lon, lat = ~lat,
          radius      = ~pmax(4, pmin(24, 4 + sqrt(total_cases / 400))),
          fillColor   = ~pal_region(who_region),
          fillOpacity = 0.75,
          color       = "white", weight = 1,
          popup = ~paste0(
            "<b>", location, "</b><br>",
            "<span style='color:#777'>", who_region, " | ", level, "</span><br>",
            "Outbreaks: <b>", n_outbreaks, "</b><br>",
            "Cases: <b>",     format(total_cases, big.mark = ","), "</b><br>",
            "Deaths: <b>",    ifelse(is.na(total_deaths) | total_deaths == 0,
                                     "—", format(total_deaths, big.mark = ",")), "</b><br>",
            "CFR: <b>",       ifelse(is.na(cfr_pct), "—",
                                     sprintf("%.1f%%", cfr_pct)), "</b><br>",
            "Attack rate: <b>", ifelse(is.na(med_attack), "—",
                                       sprintf("%.2f /1,000", med_attack)), "</b>"
          ),
          layerId = ~location
        ) %>%
        addLegend(
          colors   = unname(REGION_COLOURS[names(REGION_COLOURS) %in%
                                             unique(ms_pts$who_region)]),
          labels   = unname(REGION_LABELS[names(REGION_COLOURS) %in%
                                            unique(ms_pts$who_region)]),
          title    = "WHO Region",
          position = "bottomright",
          opacity  = 0.9
        )
    }
  })

  # Capture map click → update selected location
  observeEvent(input$map_shape_click,  selected_loc(input$map_shape_click$id))
  observeEvent(input$map_marker_click, selected_loc(input$map_marker_click$id))

  # ── Weekly time series ──────────────────────────────────────────────────────
  output$ts_header <- renderText({
    loc <- selected_loc()
    if (is.null(loc)) return("Weekly time series — click a location on the map")
    lbl <- if (input$admin_level == "Country") {
      nm <- name_lut$country_name[name_lut$country_iso3_cmp == loc]
      if (length(nm)) nm[1] else loc
    } else loc
    paste0("Weekly time series — ", lbl)
  })

  ts_raw <- reactive({
    loc <- selected_loc()
    if (is.null(loc)) return(NULL)

    if (input$admin_level == "Country") {
      # TZA has no "country" spatial_scale rows; its ADM0-equivalent data is at
      # "admin1" (Mainland + Zanzibar).  For all other countries use "country".
      ts_scale <- if (loc == "TZA") "admin1" else "country"
      new_raw %>%
        filter(spatial_scale == ts_scale,
               country_iso3_cmp == loc,
               year >= input$year_range[1],
               year <= input$year_range[2]) %>%
        group_by(TL) %>%
        summarise(sCh            = sum(sCh, na.rm = TRUE),
                  deaths         = sum(deaths, na.rm = TRUE),
                  outbreak_flag  = as.integer(any(outbreak_number > 0)),
                  .groups = "drop")
    } else {
      new_raw %>%
        filter(location == loc,
               year >= input$year_range[1],
               year <= input$year_range[2]) %>%
        mutate(outbreak_flag = as.integer(outbreak_number > 0)) %>%
        select(TL, sCh, deaths, outbreak_flag)
    }
  })

  output$ts_plot <- renderPlotly({
    empty_msg <- function(txt) {
      ggplotly(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = txt, hjust = 0.5, vjust = 0.5,
                   size = 3.5, colour = "grey60") +
          theme_void()
      ) %>% layout(margin = list(t = 5, b = 5))
    }

    ts <- ts_raw()
    if (is.null(ts) || nrow(ts) == 0)
      return(empty_msg("Click a location on the map to see its weekly time series"))

    # Outbreak shading bands: group consecutive outbreak weeks
    ts <- ts %>% arrange(TL) %>%
      mutate(grp = cumsum(c(1, diff(outbreak_flag) != 0)))
    bands <- ts %>%
      filter(outbreak_flag == 1) %>%
      group_by(grp) %>%
      summarise(xmin = min(TL), xmax = max(TL), .groups = "drop")

    p <- ggplot(ts, aes(x = TL, y = sCh)) +
      geom_rect(
        data        = bands,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        fill        = "#fc8d62", alpha = 0.25, inherit.aes = FALSE
      ) +
      geom_line(colour = "#2c7bb6", linewidth = 0.7, na.rm = TRUE) +
      geom_point(size = 0.5, colour = "#2c7bb6", na.rm = TRUE) +
      scale_y_continuous(labels = comma_format(),
                         expand = expansion(mult = c(0, 0.08))) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(x = NULL, y = "Cases / week",
           caption = "Orange bands = outbreak periods") +
      theme_bw(base_size = 10) +
      theme(axis.text.x  = element_text(angle = 45, hjust = 1),
            plot.caption = element_text(size = 7, colour = "#888"))

    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE, margin = list(t = 10, b = 10, l = 50, r = 20))
  })

  # ── Statistics table ────────────────────────────────────────────────────────
  table_df <- reactive({
    filtered_ob() %>%
      left_join(name_lut, by = "country_iso3_cmp") %>%
      mutate(country_label = coalesce(country_name, country_iso3_cmp)) %>%
      group_by(scale, country_label, who_region) %>%
      summarise(
        n_locations  = n_distinct(location),
        n_outbreaks  = n(),
        total_cases  = sum(total_cases,  na.rm = TRUE),
        total_deaths = sum(total_deaths, na.rm = TRUE),
        cfr_pct      = ifelse(sum(total_cases, na.rm = TRUE) > 0,
                              sum(total_deaths, na.rm = TRUE) /
                              sum(total_cases,  na.rm = TRUE) * 100, NA_real_),
        med_dur      = median(n_weeks,        na.rm = TRUE),
        q1_dur       = quantile(n_weeks, 0.25, na.rm = TRUE),
        q3_dur       = quantile(n_weeks, 0.75, na.rm = TRUE),
        med_cases_ob = median(total_cases,        na.rm = TRUE),
        q1_cases_ob  = quantile(total_cases, 0.25, na.rm = TRUE),
        q3_cases_ob  = quantile(total_cases, 0.75, na.rm = TRUE),
        med_ar       = median(attack_rate, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        dur_iqr  = sprintf("%.0f (%.0f–%.0f)", med_dur, q1_dur, q3_dur),
        ob_iqr   = sprintf("%s (%s–%s)",
                            format(round(med_cases_ob), big.mark = ","),
                            format(round(q1_cases_ob),  big.mark = ","),
                            format(round(q3_cases_ob),  big.mark = ",")),
        cfr_str  = ifelse(!is.na(cfr_pct) & cfr_pct > 0,
                          sprintf("%.1f", cfr_pct), "—"),
        ar_str   = ifelse(!is.na(med_ar), sprintf("%.2f", med_ar), "—"),
        cases_f  = format(total_cases,  big.mark = ","),
        deaths_f = ifelse(total_deaths > 0,
                          format(total_deaths, big.mark = ","), "—")
      ) %>%
      arrange(scale, desc(n_outbreaks)) %>%
      select(scale, country_label, who_region, n_locations, n_outbreaks,
             cases_f, deaths_f, cfr_str, dur_iqr, ob_iqr, ar_str)
  })

  output$stats_table <- renderDT({
    df <- table_df()
    datatable(
      df,
      colnames = c(
        "Level", "Country", "WHO Region", "Locations",
        "Outbreaks", "Total cases", "Total deaths", "CFR (%)",
        "Duration wk (IQR)", "Cases/outbreak (IQR)", "Attack rate /1k (med)"
      ),
      rownames   = FALSE,
      extensions = c("Buttons", "Scroller"),
      options = list(
        dom         = "Bfrtip",
        buttons     = list(list(extend = "csv", filename = "cholera_outbreaks",
                                text   = "⬇ Download CSV")),
        scrollX     = TRUE,
        deferRender = TRUE,
        scrollY     = "420px",
        scroller    = TRUE,
        pageLength  = 50
      )
    ) %>%
      formatStyle(
        "n_outbreaks",
        background         = styleColorBar(c(0, max(df$n_outbreaks)), "#a8c7e8"),
        backgroundSize     = "90% 70%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # ── Top locations bar chart ─────────────────────────────────────────────────
  output$top_locs_plot <- renderPlot({
    top15 <- filtered_ob() %>%
      group_by(location, who_region) %>%
      summarise(total_cases = sum(total_cases, na.rm = TRUE), .groups = "drop") %>%
      slice_max(total_cases, n = 15, with_ties = FALSE) %>%
      mutate(
        loc_short = str_trunc(location, 32),
        loc_short = fct_reorder(loc_short, total_cases)
      )

    if (nrow(top15) == 0) {
      return(ggplot() + annotate("text", x=0.5, y=0.5, label="No data", colour="grey60") +
               theme_void())
    }

    ggplot(top15, aes(x = total_cases, y = loc_short, fill = who_region)) +
      geom_col(alpha = 0.85, width = 0.7) +
      geom_text(aes(label = format(total_cases, big.mark = ",")),
                hjust = -0.1, size = 2.6, colour = "#333") +
      scale_fill_manual(values = REGION_COLOURS, labels = REGION_LABELS, name = NULL) +
      scale_x_continuous(labels = comma_format(),
                         expand = expansion(mult = c(0, 0.18))) +
      labs(x = "Total cases", y = NULL) +
      theme_bw(base_size = 10) +
      theme(legend.position  = "bottom",
            legend.text      = element_text(size = 7),
            axis.text.y      = element_text(size = 8),
            panel.grid.major.y = element_blank())
  })

  # ── Heatmap ──────────────────────────────────────────────────────────────────
  output$heatmap_title <- renderText({
    m <- switch(input$heatmap_metric,
                "n_outbreaks" = "Outbreak count",
                "total_cases" = "Total cases")
    paste0(m, " by country × year  —  ", input$admin_level, " level")
  })

  heatmap_df <- reactive({
    metric <- input$heatmap_metric
    yr     <- input$year_range

    base <- filtered_ob() %>%
      left_join(name_lut, by = "country_iso3_cmp") %>%
      mutate(country_label = coalesce(country_name, country_iso3_cmp)) %>%
      group_by(country_label, year) %>%
      summarise(
        n_outbreaks = n(),
        total_cases = sum(total_cases, na.rm = TRUE),
        .groups = "drop"
      )

    # Full year × country grid so absent years appear as NA (grey)
    full_grid <- expand.grid(
      country_label = unique(base$country_label),
      year          = seq(yr[1], yr[2]),
      stringsAsFactors = FALSE
    )
    left_join(full_grid, base, by = c("country_label", "year"))
  })

  output$heatmap <- renderPlotly({
    hd     <- heatmap_df()
    metric <- input$heatmap_metric
    mlabel <- switch(metric, n_outbreaks = "Outbreaks (n)", total_cases = "Total cases")

    if (nrow(hd) == 0)
      return(plotly_empty(type = "scatter") %>% layout(title = "No data for current filters"))

    # Sort countries: most active at the top of the plot (highest y-axis value)
    country_order <- hd %>%
      group_by(country_label) %>%
      summarise(total = sum(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
      arrange(total) %>%      # ascending → top of y-axis is the largest
      pull(country_label)

    hd <- hd %>%
      mutate(
        country_label = factor(country_label, levels = country_order),
        val_disp      = ifelse(is.na(.data[[metric]]), "—",
                               format(.data[[metric]], big.mark = ",")),
        tooltip       = paste0("<b>", country_label, "</b> (", year, ")<br>",
                               mlabel, ": ", val_disp)
      )

    p <- ggplot(hd, aes(x = year, y = country_label,
                         fill = .data[[metric]], text = tooltip)) +
      geom_tile(colour = "white", linewidth = 0.25) +
      scale_fill_viridis_c(
        option   = "viridis",
        na.value = "#f0f0f0",
        name     = mlabel,
        labels   = comma_format()
      ) +
      scale_x_continuous(
        breaks = seq(input$year_range[1], input$year_range[2],
                     by = max(1L, as.integer((diff(input$year_range) + 1L) / 8L)))
      ) +
      labs(x = "Year", y = NULL) +
      theme_bw(base_size = 10) +
      theme(
        axis.text.x    = element_text(angle = 45, hjust = 1),
        axis.text.y    = element_text(size  = 8),
        panel.grid     = element_blank(),
        legend.title   = element_text(size = 9)
      )

    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(l = 130, b = 60, t = 10, r = 20),
        yaxis  = list(tickfont = list(size = 9))
      )
  })
}

# ── 6. Run ────────────────────────────────────────────────────────────────────
shinyApp(ui, server)
