# scratch_stage2_summary.R
# Scratchpad: coverage and descriptive statistics of Stage 2 outbreak outputs
#
# Run from project root:
#   Rscript analysis/scratch_stage2_summary.R

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(scales)
library(lubridate)
library(forcats)
library(stringr)

# ── 0. Load & combine ─────────────────────────────────────────────────────────

stage2_files <- list.files(
  here("analysis/generated_data"),
  pattern    = "^stage2_.*\\.rds$",
  full.names = TRUE
)

message("Loading ", length(stage2_files), " stage2 files …")

combined <- lapply(stage2_files, function(f) {
  tryCatch({
    df <- readRDS(f)
    if (nrow(df) == 0) return(NULL)
    df
  }, error = function(e) {
    warning("Failed: ", basename(f), " — ", conditionMessage(e)); NULL
  })
}) |> bind_rows()

message("Combined: ", nrow(combined), " rows, ", n_distinct(combined$country_iso3), " countries")

# ── 1. Derived fields ─────────────────────────────────────────────────────────

scale_levels   <- c("country", "admin1", "admin2", "admin3", "admin4 or lower")
existing_scale <- intersect(scale_levels, unique(as.character(combined$spatial_scale)))

combined <- combined |>
  mutate(
    year          = year(TL),
    in_outbreak   = outbreak_number > 0,
    spatial_scale = fct_relevel(as.character(spatial_scale), existing_scale)
  )

real <- combined |> filter(!phantom)

# Full observation span per country (min TL → max TR across all time windows)
# Gap weeks fall inside this span; they contribute 0 to all numerators.
country_span <- combined |>
  group_by(who_region, country_iso3) |>
  summarise(
    span_start = min(TL, na.rm = TRUE),
    span_end   = max(TR, na.rm = TRUE),
    span_weeks = as.numeric(difftime(max(TR, na.rm = TRUE),
                                     min(TL, na.rm = TRUE), units = "weeks")),
    .groups = "drop"
  )

# ── 2. Panel A — Outbreak burden by country ───────────────────────────────────
# Denominator = full span_weeks (gaps count as 0 outbreak-weeks).

# Use country-scale rows only so that outbreak_weeks counts unique *calendar*
# weeks, not location-weeks across all admin levels. Mixing all-scale
# location-week counts with a calendar-week denominator (span_weeks) inflates
# pct_outbreak by 10-28× for countries with dense sub-national data (COD →
# 2800%, IRQ → 1949%, etc.).
country_summary <- real |>
  filter(spatial_scale == "country") |>
  group_by(who_region, country_iso3) |>
  summarise(
    outbreak_weeks = n_distinct(TL[in_outbreak]),   # unique calendar weeks in outbreak
    n_outbreaks    = max(outbreak_number, na.rm = TRUE),
    total_sCh      = sum(sCh, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(country_span, by = c("who_region", "country_iso3")) |>
  mutate(
    pct_outbreak = outbreak_weeks / span_weeks,
    who_region   = factor(who_region, levels = c("AFR", "EMR", "SEAR", "AMR", "EUR", "WPR"))
  )

region_order <- country_summary |>
  group_by(who_region) |>
  summarise(med = median(pct_outbreak, na.rm = TRUE)) |>
  arrange(desc(med)) |>
  pull(who_region)

pa <- country_summary |>
  mutate(who_region = fct_relevel(who_region, as.character(region_order))) |>
  ggplot(aes(x = fct_reorder(country_iso3, pct_outbreak), y = pct_outbreak,
             colour = who_region)) +
  geom_segment(aes(xend = country_iso3, y = 0, yend = pct_outbreak),
               linewidth = 0.5, alpha = 0.6) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_colour_brewer(palette = "Dark2", name = "WHO region") +
  coord_flip() +
  facet_wrap(~who_region, scales = "free_y", ncol = 3) +
  labs(
    title    = "A  Outbreak burden by country",
    subtitle = "Outbreak-weeks ÷ full observation span (data gaps treated as 0)",
    x = NULL, y = "% of span-weeks in outbreak"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position    = "none",
    panel.grid.major.y = element_blank(),
    strip.text         = element_text(face = "bold"),
    axis.text.y        = element_text(size = 6.5)
  )

# ── 3. Panel B — Temporal heatmap with explicit data gaps ─────────────────────
# Three visual states:
#   grey         = no data at all (gap)
#   light yellow = data present but no outbreak detected
#   yellow→red   = outbreak detected (intensity = log1p outbreak-weeks)

year_range <- range(combined$year, na.rm = TRUE)
all_years  <- seq(year_range[1], year_range[2])

# Use ALL spatial scales (non-phantom) so that countries whose early years
# only have admin2/admin3 data (e.g. COD 2010-2013) are not falsely shown as
# gaps. outbreak_weeks = distinct calendar weeks where ANY location was in
# outbreak — n_distinct(TL) avoids double-counting a week covered by multiple
# admin levels.
country_yr <- combined |>
  filter(!phantom) |>
  group_by(who_region, country_iso3, year) |>
  summarise(
    has_data       = TRUE,
    outbreak_weeks = n_distinct(TL[in_outbreak]),
    .groups = "drop"
  )

ctry_order <- country_span |>
  arrange(who_region, country_iso3) |>
  pull(country_iso3)

year_grid <- expand_grid(
  country_iso3 = unique(combined$country_iso3),
  year         = all_years
) |>
  left_join(distinct(combined, who_region, country_iso3), by = "country_iso3") |>
  left_join(country_yr, by = c("who_region", "country_iso3", "year")) |>
  mutate(
    # NA  → gap (no rows for this country-year at country scale)
    # 0   → covered, no outbreak
    # >0  → covered, with outbreaks
    fill_val = if_else(is.na(has_data), NA_real_, as.numeric(outbreak_weeks))
  )

pb <- year_grid |>
  mutate(country_iso3 = factor(country_iso3, levels = ctry_order)) |>
  ggplot(aes(x = year, y = country_iso3, fill = log1p(fill_val))) +
  geom_tile(colour = "white", linewidth = 0.15) +
  scale_fill_gradient(
    low      = "#ffffcc",
    high     = "#b10026",
    na.value = "grey72",
    name     = "log1p(outbreak-weeks)"
  ) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  facet_grid(who_region ~ ., scales = "free_y", space = "free_y") +
  labs(
    title    = "B  Outbreak activity over time",
    subtitle = "Yellow = data, no outbreak  |  Red = outbreak  |  Grey = data gap",
    x = "Year", y = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.y      = element_text(size = 6),
    legend.position  = "bottom",
    legend.key.width = unit(1.2, "cm"),
    strip.text.y     = element_text(face = "bold", angle = 0)
  )

# ── 4. Panel C — Outbreak prevalence by spatial scale ────────────────────────
# Denominator = ALL rows (phantom + non-phantom) per WHO region × scale.
# Phantom rows are the zero-case weeks already filled in; this makes
# "no-data weeks within a window" zero-numerator but positive-denominator.

scale_colors <- c(
  "country"         = "#4e79a7",
  "admin1"          = "#59a14f",
  "admin2"          = "#f28e2b",
  "admin3"          = "#e15759",
  "admin4 or lower" = "#b07aa1"
)

scale_summary <- combined |>
  group_by(who_region, spatial_scale) |>
  summarise(
    total_loc_weeks    = n(),
    outbreak_loc_weeks = sum(in_outbreak, na.rm = TRUE),
    pct_outbreak       = outbreak_loc_weeks / total_loc_weeks,
    .groups = "drop"
  )

pc <- scale_summary |>
  mutate(who_region = fct_relevel(who_region, as.character(region_order))) |>
  ggplot(aes(x = who_region, y = pct_outbreak, fill = spatial_scale)) +
  geom_col(position = "dodge", width = 0.75) +
  scale_fill_manual(values = scale_colors, name = "Spatial scale") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "C  Outbreak prevalence by spatial scale",
    subtitle = "% of all covered location-weeks (incl. phantom zero-weeks) in outbreak",
    x = "WHO region", y = "% of location-weeks in outbreak"
  ) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "right")

# ── 5. Panel D — Outbreak sCh per span-week ───────────────────────────────────
# Normalise outbreak-period sCh by the full observation span (gaps = 0 sCh).
# Countries with more data gaps will appear lower, as intended.

cases_country <- real |>
  filter(in_outbreak) |>
  group_by(who_region, country_iso3) |>
  summarise(total_ob_sCh = sum(sCh, na.rm = TRUE), .groups = "drop") |>
  right_join(country_span, by = c("who_region", "country_iso3")) |>
  mutate(
    total_ob_sCh       = coalesce(total_ob_sCh, 0),
    ob_sCh_per_span_wk = total_ob_sCh / span_weeks
  ) |>
  filter(ob_sCh_per_span_wk > 0)

pd <- cases_country |>
  mutate(who_region = fct_relevel(who_region, as.character(region_order))) |>
  ggplot(aes(x = who_region, y = ob_sCh_per_span_wk, colour = who_region)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7, size = 2) +
  geom_boxplot(alpha = 0, outlier.shape = NA, linewidth = 0.6) +
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title    = "D  Outbreak sCh per span-week",
    subtitle = "Total outbreak-period sCh ÷ full observation span (gaps = 0); log scale",
    x = "WHO region", y = "sCh / span-week (log)"
  ) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "none")

# ── 6. Compose & save ─────────────────────────────────────────────────────────

layout <- "
AABB
AABB
AABB
CCDD
"

fig <- pa + pb + pc + pd +
  plot_layout(design = layout) +
  plot_annotation(
    title    = "Stage 2 outputs — coverage and outbreak descriptive statistics",
    subtitle = paste0(
      n_distinct(combined$country_iso3), " countries · ",
      n_distinct(combined$who_region),   " WHO regions · ",
      min(year(combined$TL), na.rm = TRUE), "–", max(year(combined$TR), na.rm = TRUE)
    ),
    theme = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 9,  colour = "grey40")
    )
  )

out_path <- here("analysis/generated_data/stage2_summary_fig.pdf")
ggsave(out_path, fig, width = 16, height = 14, units = "in", device = cairo_pdf)
message("Saved: ", out_path)

# ── 7. Quick console summary ──────────────────────────────────────────────────

cat("\n── Stage 2 summary ──────────────────────────────────────────────────────\n")
cat("Files loaded:         ", length(stage2_files), "\n")
cat("Total rows:           ", format(nrow(combined),         big.mark = ","), "\n")
cat("Non-phantom rows:     ", format(nrow(real),             big.mark = ","), "\n")
cat("Countries:            ", n_distinct(combined$country_iso3), "\n")
cat("WHO regions:          ", paste(sort(unique(combined$who_region)), collapse = ", "), "\n")
cat("Date range:           ",
    as.character(min(combined$TL, na.rm = TRUE)), "to",
    as.character(max(combined$TR, na.rm = TRUE)), "\n")
cat("Outbreak-period rows: ", format(sum(real$in_outbreak, na.rm = TRUE), big.mark = ","), "\n")
cat("Total sCh (all):      ", format(sum(combined$sCh,      na.rm = TRUE), big.mark = ","), "\n")
cat("Total sCh (outbreaks):",
    format(sum(real$sCh[real$in_outbreak], na.rm = TRUE), big.mark = ","), "\n")
cat("─────────────────────────────────────────────────────────────────────────\n")

print(
  country_summary |>
    group_by(who_region) |>
    summarise(
      n_countries    = n(),
      with_outbreaks = sum(n_outbreaks > 0),
      med_pct_ob     = median(pct_outbreak, na.rm = TRUE),
      total_sCh      = sum(total_sCh),
      .groups        = "drop"
    ) |>
    arrange(desc(total_sCh))
)
