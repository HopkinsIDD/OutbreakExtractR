# scratch_coverage.R  —  Data coverage across countries from Stage 1 flat files
#
# Reads every stage1_flat_*.rds, summarises per (country, time-window):
#   - whether the file is empty
#   - number of non-phantom observed rows
#   - total suspected cases (sCh)
#   - number of unique sub-national locations (admin1+)
#
# Produces three figures saved to analysis/generated_data/:
#   1. Presence/absence heatmap        (coverage_presence.png)
#   2. Observed-row-count heatmap      (coverage_obs_rows.png)
#   3. Suspected-case-count heatmap    (coverage_sCh.png)

library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(forcats)

# ---------------------------------------------------------------------------
# 1. Collect all Stage 1 flat files and parse metadata from filenames
# ---------------------------------------------------------------------------

stage1_dir <- here("analysis/generated_data")

files_raw <- list.files(stage1_dir,
                        pattern = "^stage1_flat_.*\\.rds$",
                        full.names = TRUE)

# Use str_match so variable-length region codes (AFR/AMR/EMR/SEAR/WPR/EUR)
# are captured correctly — lookbehind can't handle variable width in R.
m <- str_match(basename(files_raw),
               "^stage1_flat_([A-Z]+)_([^_]+)_TL(\\d{8})_TR(\\d{8})")

files <- tibble(
  path       = files_raw,
  fname      = basename(files_raw),
  who_region = m[, 2],
  iso3_raw   = m[, 3],
  tl_str     = m[, 4],
  tr_str     = m[, 5]
) |>
  mutate(
    TL         = lubridate::ymd(tl_str),
    TR         = lubridate::ymd(tr_str),
    window_mid = TL + as.numeric(TR - TL) / 2,
    window_yr  = year(TL)
  ) |>
  filter(!is.na(TL), !is.na(TR), !is.na(who_region), !is.na(iso3_raw))

message("Found ", nrow(files), " Stage 1 files across ",
        n_distinct(files$iso3_raw), " country/sub-country units")

# ---------------------------------------------------------------------------
# 2. Read each file and summarise
# ---------------------------------------------------------------------------

message("Reading files … (this takes ~30-60 s)")

summarise_file <- function(path) {
  d <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) {
    return(tibble(n_rows = 0L, n_obs = 0L, total_sCh = 0, n_locations = 0L,
                  empty = TRUE))
  }
  obs <- filter(d, !phantom)
  tibble(
    n_rows      = nrow(d),
    n_obs       = nrow(obs),
    total_sCh   = sum(obs$sCh, na.rm = TRUE),
    n_locations = n_distinct(obs$location[obs$spatial_scale != "country"],
                              na.rm = TRUE),
    empty       = FALSE
  )
}

summaries <- map(files$path, summarise_file, .progress = TRUE)

coverage <- bind_cols(files, list_rbind(summaries)) |>
  mutate(
    status = case_when(
      empty | n_obs == 0 ~ "no data",       # empty file OR phantom rows only
      total_sCh == 0     ~ "zero cases",    # real obs but all sCh = 0
      TRUE               ~ "has data"       # real obs with sCh > 0
    ),
    status = factor(status, levels = c("has data", "zero cases", "no data"))
  )

# ---------------------------------------------------------------------------
# 3. Shared plot helpers
# ---------------------------------------------------------------------------

# WHO-region colour palette (for y-axis strip / row label colouring)
region_pal <- c(AFR = "#E05C2F", AMR = "#2E86AB", EMR = "#A23B72",
                SEAR = "#F18F01", WPR = "#C73E1D", EUR = "#3B1F2B")

# Order countries: by WHO region, then alphabetically within region
country_order <- coverage |>
  distinct(iso3_raw, who_region) |>
  arrange(who_region, iso3_raw) |>
  pull(iso3_raw)

coverage <- coverage |>
  mutate(iso3_f = factor(iso3_raw, levels = rev(country_order)))  # rev so top = first

# X-axis: use TL directly; tile width = actual window duration in days.
# This avoids the visual gap that arises from using floor_date(mid, "quarter"),
# which maps the 3 windows to Q1/Q3/Q4 (skipping Q2) and leaves a 3-month
# blank strip mid-year because geom_tile's default width = min spacing (92 d).
coverage <- coverage |>
  mutate(
    win_days = as.numeric(TR - TL)   # tile width in days
  )

# X-axis breaks: Jan 1 of each year
year_breaks <- tibble(
  break_dt = seq(
    floor_date(min(coverage$TL), "year"),
    floor_date(max(coverage$TL), "year"),
    by = "1 year"
  )
) |> mutate(label = year(break_dt))

base_theme <- theme_minimal(base_size = 10) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y  = element_text(size = 7),
    panel.grid   = element_blank(),
    legend.position = "bottom",
    plot.title   = element_text(face = "bold"),
    strip.text   = element_text(face = "bold", size = 8)
  )

# Y-axis coloured by WHO region
region_colour_scale <- function(df) {
  cols <- region_pal[df |> distinct(iso3_raw, who_region) |>
                       arrange(factor(iso3_raw, levels = rev(country_order))) |>
                       pull(who_region)]
  scale_y_discrete(labels = setNames(
    str_replace(rev(country_order), "\n", "::"),
    rev(country_order)
  ))
}

# ---------------------------------------------------------------------------
# 4. Figure 1 — Presence / absence heatmap
# ---------------------------------------------------------------------------

fig1_pal <- c(
  "has data"   = "#2C7BB6",   # blue  — real sCh data
  "zero cases" = "#FDB462",   # amber — obs present, sCh all zero
  "no data"    = "#DDDDDD"    # light grey — no real observations
)

p1 <- ggplot(coverage,
             aes(x = TL + win_days / 2, y = iso3_f, fill = status,
                 width = win_days)) +
  geom_tile(colour = "white", linewidth = 0.15) +
  scale_fill_manual(values = fig1_pal, name = "Stage 1 status",
                    drop = FALSE) +
  scale_x_date(
    breaks = year_breaks$break_dt,
    labels = year_breaks$label,
    expand = expansion(add = 0)
  ) +
  facet_grid(who_region ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "Stage 1 data coverage — presence / absence",
    x = NULL, y = NULL
  ) +
  base_theme

out1 <- here("analysis/generated_data/coverage_presence.png")
ggsave(out1, p1, width = 14, height = 10, dpi = 150)
message("Saved: ", basename(out1))

# ---------------------------------------------------------------------------
# 5. Figure 2 — Observed-row heatmap (log10 scale)
# ---------------------------------------------------------------------------

p2 <- coverage |>
  mutate(n_obs_plot = if_else(n_obs == 0, NA_integer_, n_obs)) |>
  ggplot(aes(x = TL + win_days / 2, y = iso3_f, fill = n_obs_plot,
             width = win_days)) +
  geom_tile(colour = "white", linewidth = 0.15) +
  scale_fill_viridis_c(
    name = "Observed rows\n(log10 + 1)",
    trans = "log1p",
    option = "plasma",
    na.value = "#BBBBBB",
    labels = scales::label_comma()
  ) +
  scale_x_date(
    breaks = year_breaks$break_dt,
    labels = year_breaks$label,
    expand = expansion(add = 0)
  ) +
  facet_grid(who_region ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "Stage 1 data coverage — observed (non-phantom) rows per window",
    x = NULL, y = NULL
  ) +
  base_theme

out2 <- here("analysis/generated_data/coverage_obs_rows.png")
ggsave(out2, p2, width = 14, height = 10, dpi = 150)
message("Saved: ", basename(out2))

# ---------------------------------------------------------------------------
# 6. Figure 3 — Suspected-case heatmap (log10 scale)
# ---------------------------------------------------------------------------

p3 <- coverage |>
  mutate(sCh_plot = if_else(total_sCh == 0, NA_real_, total_sCh)) |>
  ggplot(aes(x = TL + win_days / 2, y = iso3_f, fill = sCh_plot,
             width = win_days)) +
  geom_tile(colour = "white", linewidth = 0.15) +
  scale_fill_viridis_c(
    name = "Suspected cases\n(log10 + 1)",
    trans = "log1p",
    option = "inferno",
    na.value = "#BBBBBB",
    labels = scales::label_comma()
  ) +
  scale_x_date(
    breaks = year_breaks$break_dt,
    labels = year_breaks$label,
    expand = expansion(add = 0)
  ) +
  facet_grid(who_region ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "Stage 1 data coverage — total suspected cases (sCh) per window",
    x = NULL, y = NULL
  ) +
  base_theme

out3 <- here("analysis/generated_data/coverage_sCh.png")
ggsave(out3, p3, width = 14, height = 10, dpi = 150)
message("Saved: ", basename(out3))

# ---------------------------------------------------------------------------
# 7. Quick console summary
# ---------------------------------------------------------------------------

cat("\n=== Coverage summary ===\n")
coverage |>
  count(who_region, status) |>
  tidyr::pivot_wider(names_from = status, values_from = n, values_fill = 0L) |>
  print()

cat("\nCountries with ≥1 window of real data:\n")
coverage |>
  filter(status == "has data") |>
  distinct(who_region, iso3_raw) |>
  count(who_region) |>
  print()

cat("\nCountries with NO real data in any window:\n")
no_data <- coverage |>
  group_by(iso3_raw) |>
  summarise(any_data = any(status == "has data")) |>
  filter(!any_data) |>
  pull(iso3_raw)
print(no_data)
