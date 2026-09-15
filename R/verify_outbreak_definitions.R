#' @title verify_outbreak_definitions
#' @description Post-hoc verification that the output of
#'   \code{\link{identify_outbreaks}()} is internally consistent with the
#'   outbreak-definition parameters used to produce it.
#'
#' @details
#' The function accepts the list returned by \code{identify_outbreaks()} (or a
#' pre-bound dataframe) and runs the following checks, returning one tidy row
#' per check per location (or per outbreak, for outbreak-level checks):
#'
#' \strong{Location-level checks}
#' \describe{
#'   \item{\code{risk_classification_consistency}}{Every row: \code{risk ==
#'     "high"} iff \code{sCh > 0} and \code{sCh / pop >= threshold}.}
#'   \item{\code{no_zero_case_epidemic_start}}{All \code{epidemic_start = TRUE}
#'     rows have \code{sCh > 0}.}
#'   \item{\code{epidemic_start_in_outbreak_period}}{Every epidemic start is
#'     assigned a positive \code{outbreak_number}.}
#'   \item{\code{consecutive_start_validity}}{(\code{consecutive} mode) Each
#'     epidemic start begins a run of at least \code{min_weeks_above}
#'     consecutive \code{"high"}-risk weeks.}
#'   \item{\code{dual_window_start_validity}}{(\code{dual_window} mode) Each
#'     epidemic start satisfies the sliding-window trigger, the cumulative
#'     trigger, or both.}
#'   \item{\code{epidemic_tail_validity}}{For every row where
#'     \code{epidemic_tail = TRUE}, the subsequent \code{tail_period}
#'     consecutive rows (including the flagged row itself) are all
#'     \code{risk == "low"} and \code{epidemic_start == FALSE}.}
#'   \item{\code{inter_outbreak_gap_weeks}}{The gap in weeks between
#'     consecutive outbreak periods is at least \code{tail_period}.
#'     One row per consecutive pair (\code{outbreak_number} column stores
#'     \code{"k-k+1"}).}
#' }
#'
#' \strong{Outbreak-level checks (one row per location × outbreak_number)}
#' \describe{
#'   \item{\code{min_high_risk_weeks_per_outbreak}}{Each outbreak period
#'     contains at least \code{min_weeks_above} high-risk weeks.}
#'   \item{\code{cumulative_cases_at_start}}{The sum of \code{sCh} over the
#'     first \code{cumulative_windows} weeks of the outbreak meets
#'     \code{cumulative_min_cases}. Skipped when \code{cumulative_min_cases}
#'     is \code{NULL}.}
#'   \item{\code{outbreak_weekly_continuity}}{All consecutive week-start dates
#'     within the outbreak are exactly 7 days apart (no internal gaps).}
#'   \item{\code{tail_period_after_outbreak}}{After the last row of the
#'     outbreak period, the immediately following non-outbreak weeks are all
#'     low-risk. SKIP is returned when no data follows the outbreak.}
#'   \item{\code{outbreak_summary}}{Informational (\code{status = "INFO"}):
#'     total cases, duration in weeks, and peak weekly cases.}
#' }
#'
#' @param outbreak_list Named list of dataframes returned by
#'   \code{\link{identify_outbreaks}()}, or a single dataframe produced by
#'   \code{purrr::list_rbind(outbreak_list)}.
#' @param outbreak_start_definition \code{"consecutive"} (default) or
#'   \code{"dual_window"}.
#' @param min_weeks_above Integer. Minimum consecutive / sliding-window
#'   high-risk weeks required to trigger an epidemic start (default 2).
#' @param tail_period Integer. Consecutive below-threshold weeks required to
#'   close an outbreak (default 6).
#' @param cumulative_min_cases Numeric or \code{NULL}. When non-NULL, each
#'   outbreak's first \code{cumulative_windows} weeks must sum to at least this
#'   many cases. Applies in any \code{outbreak_start_definition} mode.
#' @param cumulative_windows Integer. Width of the cumulative case window used
#'   in \code{dual_window} mode and in the \code{cumulative_cases_at_start}
#'   check (default 3).
#' @param window_weeks Integer. Width of the sliding high-risk-week window
#'   (\code{dual_window} mode, default 3).
#' @param cumulative_case_threshold_ratio Numeric. Multiplier on
#'   \code{threshold * pop} for the cumulative case trigger (default 1.5).
#' @param use_cumulative_trigger Logical. Whether the cumulative trigger is
#'   active in \code{dual_window} mode (default \code{TRUE}).
#' @param cumulative_trigger_type Character. One of
#'   \code{"cumulative_case_threshold"},
#'   \code{"cumulative_case_threshold_and_min_cases"}, or
#'   \code{"cumulative_case_threshold_and_nonzero_weeks"}.
#' @param nonzero_windows Integer or \code{NULL}. Minimum non-zero-case weeks
#'   in the cumulative window (used with
#'   \code{"cumulative_case_threshold_and_nonzero_weeks"}).
#'
#' @return A \code{tibble} with columns:
#' \describe{
#'   \item{\code{location}}{Location string.}
#'   \item{\code{outbreak_number}}{Outbreak identifier. \code{NA} for
#'     location-level checks; a string like \code{"1-2"} for inter-outbreak
#'     gap checks; a positive integer (as character) for outbreak-level
#'     checks.}
#'   \item{\code{check}}{Name of the verification check.}
#'   \item{\code{status}}{\code{"PASS"}, \code{"FAIL"}, \code{"SKIP"}, or
#'     \code{"INFO"}.}
#'   \item{\code{value}}{Measured quantity for the check (e.g. number of
#'     violations, total cases, gap in weeks).}
#'   \item{\code{detail}}{Human-readable description of the result, including
#'     specifics for failures.}
#' }
#'
#' @examples
#' \dontrun{
#' # Run outbreak detection
#' outbreak_list <- identify_outbreaks(
#'   threshold_type            = "mean weekly incidence rate",
#'   original_data             = my_data,
#'   outbreak_start_definition = "consecutive",
#'   min_weeks_above           = 2,
#'   tail_period               = 6
#' )
#'
#' # Verify the definitions were applied consistently
#' results <- verify_outbreak_definitions(
#'   outbreak_list             = outbreak_list,
#'   outbreak_start_definition = "consecutive",
#'   min_weeks_above           = 2,
#'   tail_period               = 6,
#'   cumulative_min_cases      = 50
#' )
#'
#' # Inspect failures
#' results[results$status == "FAIL", ]
#' }
#' @export
verify_outbreak_definitions <- function(
    outbreak_list,
    outbreak_start_definition       = c("consecutive", "dual_window"),
    min_weeks_above                  = 2L,
    tail_period                      = 6L,
    cumulative_min_cases             = NULL,
    cumulative_windows               = 3L,
    window_weeks                     = 3L,
    cumulative_case_threshold_ratio  = 1.5,
    use_cumulative_trigger           = TRUE,
    cumulative_trigger_type          = c(
      "cumulative_case_threshold",
      "cumulative_case_threshold_and_min_cases",
      "cumulative_case_threshold_and_nonzero_weeks"
    ),
    nonzero_windows                  = NULL
) {
  outbreak_start_definition <- match.arg(outbreak_start_definition)
  cumulative_trigger_type   <- match.arg(cumulative_trigger_type)
  min_weeks_above           <- as.integer(min_weeks_above)
  tail_period               <- as.integer(tail_period)
  cumulative_windows        <- as.integer(cumulative_windows)
  window_weeks              <- as.integer(window_weeks)

  # ---------------------------------------------------------------------------
  # Normalise input: accept list-of-dataframes or single bound dataframe
  # ---------------------------------------------------------------------------
  if (is.data.frame(outbreak_list)) {
    df_all <- outbreak_list
  } else {
    df_all <- purrr::list_rbind(
      purrr::keep(outbreak_list, \(x) is.data.frame(x) && nrow(x) > 0)
    )
  }

  if (nrow(df_all) == 0) {
    message("verify_outbreak_definitions: no data to verify — returning empty result.")
    return(tibble::tibble(
      location        = character(),
      outbreak_number = character(),
      check           = character(),
      status          = character(),
      value           = numeric(),
      detail          = character()
    ))
  }

  df_all <- df_all %>%
    dplyr::mutate(TL = as.Date(TL), TR = as.Date(TR))

  # ---------------------------------------------------------------------------
  # Helper: build one result row
  # ---------------------------------------------------------------------------
  make_row <- function(location,
                       outbreak_number = NA_character_,
                       check,
                       status,
                       value  = NA_real_,
                       detail = "") {
    tibble::tibble(
      location        = as.character(location),
      outbreak_number = as.character(outbreak_number),
      check           = as.character(check),
      status          = as.character(status),
      value           = as.numeric(value),
      detail          = as.character(detail)
    )
  }

  # Pre-allocate result list (grows as needed)
  results <- vector("list", 2000L)
  ri <- 0L
  push <- function(row) { ri <<- ri + 1L; results[[ri]] <<- row }

  # ===========================================================================
  # Loop over locations
  # ===========================================================================
  for (loc in unique(df_all$location)) {

    d <- df_all %>%
      dplyr::filter(location == loc) %>%
      dplyr::arrange(TL)

    has_cols <- function(...) all(c(...) %in% names(d))

    # -------------------------------------------------------------------------
    # CHECK 1 — risk_classification_consistency
    # risk == "high" iff (sCh > 0) AND (sCh / pop >= threshold)
    # -------------------------------------------------------------------------
    if (has_cols("sCh", "pop", "threshold", "risk")) {
      computable <- !is.na(d$pop) & !is.na(d$threshold) & d$pop > 0
      d_c <- d[computable, ]
      if (nrow(d_c) > 0) {
        expected_risk <- dplyr::if_else(
          d_c$sCh > 0 & (d_c$sCh / d_c$pop) >= d_c$threshold,
          "high", "low"
        )
        n_bad <- sum(d_c$risk != expected_risk, na.rm = TRUE)
        push(make_row(loc, NA_character_, "risk_classification_consistency",
          if (n_bad == 0) "PASS" else "FAIL",
          n_bad,
          if (n_bad == 0)
            paste0("All ", nrow(d_c), " row(s) have risk labels consistent with ",
                   "sCh/pop >= threshold & sCh > 0.")
          else
            paste0(n_bad, " of ", nrow(d_c),
                   " row(s) have a risk label inconsistent with sCh/pop vs threshold. ",
                   "Check for population or threshold anomalies.")))
      }
    }

    # -------------------------------------------------------------------------
    # CHECK 2 — no_zero_case_epidemic_start
    # All epidemic_start == TRUE rows must have sCh > 0
    # -------------------------------------------------------------------------
    if (has_cols("epidemic_start", "sCh")) {
      starts <- d[d$epidemic_start %in% TRUE, ]
      if (nrow(starts) > 0) {
        n_zero <- sum(is.na(starts$sCh) | starts$sCh == 0)
        push(make_row(loc, NA_character_, "no_zero_case_epidemic_start",
          if (n_zero == 0) "PASS" else "FAIL",
          n_zero,
          if (n_zero == 0)
            paste0("All ", nrow(starts), " epidemic start(s) have sCh > 0.")
          else
            paste0(n_zero, " of ", nrow(starts),
                   " epidemic start(s) have zero or missing case count (sCh == 0 or NA). ",
                   "Dates: ",
                   paste(head(as.character(starts$TL[is.na(starts$sCh) | starts$sCh == 0]), 5),
                         collapse = ", "),
                   if (n_zero > 5) " ..." else "")))
      }
    }

    # -------------------------------------------------------------------------
    # CHECK 3 — epidemic_start_in_outbreak_period
    # All epidemic_start == TRUE rows must have outbreak_number > 0
    # -------------------------------------------------------------------------
    if (has_cols("epidemic_start", "outbreak_number")) {
      starts <- d[d$epidemic_start %in% TRUE, ]
      if (nrow(starts) > 0) {
        n_out <- sum(is.na(starts$outbreak_number) | starts$outbreak_number == 0)
        push(make_row(loc, NA_character_, "epidemic_start_in_outbreak_period",
          if (n_out == 0) "PASS" else "FAIL",
          n_out,
          if (n_out == 0)
            "All epidemic starts are within a labeled outbreak period (outbreak_number > 0)."
          else
            paste0(n_out, " epidemic start(s) are not assigned to any outbreak period. ",
                   "Dates: ",
                   paste(head(as.character(starts$TL[is.na(starts$outbreak_number) |
                                                       starts$outbreak_number == 0]), 5),
                         collapse = ", "),
                   if (n_out > 5) " ..." else "")))
      }
    }

    # -------------------------------------------------------------------------
    # CHECK 4 — consecutive_start_validity  (consecutive mode only)
    # Each epidemic_start begins a run of >= min_weeks_above "high"-risk weeks.
    # -------------------------------------------------------------------------
    if (outbreak_start_definition == "consecutive" &&
        has_cols("epidemic_start", "risk")) {
      start_idx <- which(d$epidemic_start %in% TRUE)
      n_fail    <- 0L
      fail_tl   <- character()
      for (i in start_idx) {
        run_end <- min(i + min_weeks_above - 1L, nrow(d))
        run_len <- run_end - i + 1L
        if (run_len < min_weeks_above || !all(d$risk[i:run_end] == "high")) {
          n_fail  <- n_fail + 1L
          fail_tl <- c(fail_tl, as.character(d$TL[i]))
        }
      }
      push(make_row(loc, NA_character_, "consecutive_start_validity",
        if (n_fail == 0) "PASS" else "FAIL",
        n_fail,
        if (n_fail == 0)
          paste0("All ", length(start_idx), " epidemic start(s) are followed by >= ",
                 min_weeks_above, " consecutive high-risk weeks.")
        else
          paste0(n_fail, " epidemic start(s) are NOT followed by ", min_weeks_above,
                 " consecutive high-risk weeks. ",
                 "First offending TL: ",
                 paste(head(fail_tl, 5), collapse = ", "),
                 if (n_fail > 5) " ..." else "")))
    }

    # -------------------------------------------------------------------------
    # CHECK 5 — dual_window_start_validity  (dual_window mode only)
    # Each epidemic_start satisfies the sliding-window trigger (>= min_weeks_above
    # high-risk weeks in the forward window_weeks window) OR the cumulative
    # trigger (cumulative sCh over cumulative_windows weeks meets the threshold).
    # -------------------------------------------------------------------------
    if (outbreak_start_definition == "dual_window" &&
        has_cols("epidemic_start", "risk", "sCh", "pop", "threshold")) {
      start_idx <- which(d$epidemic_start %in% TRUE)
      n_fail    <- 0L
      fail_tl   <- character()
      for (i in start_idx) {
        # --- Sliding-window trigger ---
        w_end      <- min(i + window_weeks - 1L, nrow(d))
        n_high_win <- sum(d$risk[i:w_end] == "high", na.rm = TRUE)
        sliding_ok <- n_high_win >= min_weeks_above

        # --- Cumulative trigger ---
        c_end     <- min(i + cumulative_windows - 1L, nrow(d))
        cum_cases <- sum(d$sCh[i:c_end], na.rm = TRUE)
        cum_thresh <- if (!is.na(d$threshold[i]) && !is.na(d$pop[i]) && d$pop[i] > 0)
          d$threshold[i] * d$pop[i] * cumulative_case_threshold_ratio
        else
          Inf
        thresh_met <- is.finite(cum_thresh) && cum_cases >= cum_thresh

        cum_ok <- if (!use_cumulative_trigger) {
          FALSE
        } else if (cumulative_trigger_type == "cumulative_case_threshold") {
          thresh_met
        } else if (cumulative_trigger_type == "cumulative_case_threshold_and_min_cases") {
          thresh_met && !is.null(cumulative_min_cases) && cum_cases >= cumulative_min_cases
        } else if (cumulative_trigger_type == "cumulative_case_threshold_and_nonzero_weeks") {
          n_nz <- sum(d$sCh[i:c_end] > 0, na.rm = TRUE)
          thresh_met && !is.null(nonzero_windows) && n_nz >= nonzero_windows
        } else {
          FALSE
        }

        if (!sliding_ok && !cum_ok) {
          n_fail  <- n_fail + 1L
          fail_tl <- c(fail_tl, as.character(d$TL[i]))
        }
      }
      push(make_row(loc, NA_character_, "dual_window_start_validity",
        if (n_fail == 0) "PASS" else "FAIL",
        n_fail,
        if (n_fail == 0)
          paste0("All ", length(start_idx),
                 " epidemic start(s) satisfy at least one dual-window trigger ",
                 "(sliding window or cumulative).")
        else
          paste0(n_fail, " epidemic start(s) satisfy neither the sliding-window trigger ",
                 "(>= ", min_weeks_above, " high-risk weeks in ", window_weeks, " weeks) ",
                 "nor the cumulative trigger. ",
                 "First offending TL: ",
                 paste(head(fail_tl, 5), collapse = ", "),
                 if (n_fail > 5) " ..." else "")))
    }

    # -------------------------------------------------------------------------
    # CHECK 6 — epidemic_tail_validity
    # For every epidemic_tail == TRUE row at position i,
    # rows i through i + tail_period - 1 must all be risk == "low" and
    # epidemic_start == FALSE.  This verifies the tail flag semantics.
    # -------------------------------------------------------------------------
    if (has_cols("epidemic_tail", "risk", "epidemic_start")) {
      tail_idx <- which(d$epidemic_tail %in% TRUE)
      if (length(tail_idx) > 0) {
        n_fail  <- 0L
        fail_tl <- character()
        for (i in tail_idx) {
          run_end <- min(i + tail_period - 1L, nrow(d))
          run_rows <- d[i:run_end, ]
          if (!all(run_rows$risk == "low") || any(run_rows$epidemic_start %in% TRUE)) {
            n_fail  <- n_fail + 1L
            fail_tl <- c(fail_tl, as.character(d$TL[i]))
          }
        }
        push(make_row(loc, NA_character_, "epidemic_tail_validity",
          if (n_fail == 0) "PASS" else "FAIL",
          n_fail,
          if (n_fail == 0)
            paste0("All ", length(tail_idx), " epidemic_tail flag(s) correctly mark ",
                   "the start of a ", tail_period, "-week low-risk run.")
          else
            paste0(n_fail, " epidemic_tail row(s) are NOT followed by ",
                   tail_period, " consecutive low-risk non-epidemic-start weeks. ",
                   "First offending TL: ",
                   paste(head(fail_tl, 5), collapse = ", "),
                   if (n_fail > 5) " ..." else "")))
      }
    }

    # =========================================================================
    # OUTBREAK-LEVEL CHECKS
    # =========================================================================
    if ("outbreak_number" %in% names(d)) {
      ob_ids <- sort(unique(
        d$outbreak_number[!is.na(d$outbreak_number) & d$outbreak_number > 0]
      ))

      for (ob_id in ob_ids) {
        ob <- d[!is.na(d$outbreak_number) & d$outbreak_number == ob_id, ] %>%
          dplyr::arrange(TL)

        # ----------------------------------------------------------------------
        # CHECK 7 — min_high_risk_weeks_per_outbreak
        # Each outbreak must contain at least min_weeks_above high-risk weeks.
        # ----------------------------------------------------------------------
        if ("risk" %in% names(ob)) {
          n_high <- sum(ob$risk == "high", na.rm = TRUE)
          push(make_row(loc, ob_id, "min_high_risk_weeks_per_outbreak",
            if (n_high >= min_weeks_above) "PASS" else "FAIL",
            n_high,
            paste0("Outbreak ", ob_id, ": ", n_high, " high-risk week(s) in ",
                   nrow(ob), " total week(s) ",
                   "(minimum required: ", min_weeks_above, ").")))
        }

        # ----------------------------------------------------------------------
        # CHECK 8 — cumulative_cases_at_start
        # Sum of sCh over the first cumulative_windows weeks >= cumulative_min_cases.
        # Skipped when cumulative_min_cases is NULL.
        # ----------------------------------------------------------------------
        if (!is.null(cumulative_min_cases) && "sCh" %in% names(ob)) {
          n_win    <- min(cumulative_windows, nrow(ob))
          cum_sum  <- sum(ob$sCh[seq_len(n_win)], na.rm = TRUE)
          push(make_row(loc, ob_id, "cumulative_cases_at_start",
            if (cum_sum >= cumulative_min_cases) "PASS" else "FAIL",
            cum_sum,
            paste0("Outbreak ", ob_id, ": first ", n_win, " week(s) sum to ",
                   round(cum_sum, 1), " cases ",
                   "(minimum required: ", cumulative_min_cases, ").")))
        }

        # ----------------------------------------------------------------------
        # CHECK 9 — outbreak_weekly_continuity
        # Consecutive TL values within an outbreak must be exactly 7 days apart.
        # ----------------------------------------------------------------------
        if (nrow(ob) > 1) {
          gaps    <- as.integer(diff(ob$TL))
          n_gap_v <- sum(gaps != 7L)
          push(make_row(loc, ob_id, "outbreak_weekly_continuity",
            if (n_gap_v == 0) "PASS" else "FAIL",
            n_gap_v,
            if (n_gap_v == 0)
              paste0("Outbreak ", ob_id, ": all ", nrow(ob),
                     " weeks are consecutive (7-day spacing).")
            else
              paste0("Outbreak ", ob_id, ": ", n_gap_v,
                     " non-7-day gap(s) within the outbreak period. ",
                     "Min gap: ", min(gaps), " days, max gap: ", max(gaps), " days.")))
        }

        # ----------------------------------------------------------------------
        # CHECK 10 — tail_period_after_outbreak
        # Immediately after each outbreak period, the following non-outbreak
        # weeks should be low-risk (forming the washout / tail period).
        # The minimum number of such weeks depends on how the tail is split
        # between the outbreak period and the non-outbreak period; this check
        # simply verifies that the immediately-following weeks are not high-risk.
        # Returns SKIP when no data follows the outbreak.
        # ----------------------------------------------------------------------
        if ("risk" %in% names(d)) {
          ob_end_tl <- max(ob$TL)
          after     <- d[d$TL > ob_end_tl, ] %>% dplyr::arrange(TL)

          if (nrow(after) == 0) {
            push(make_row(loc, ob_id, "tail_period_after_outbreak",
              "SKIP", NA_real_,
              paste0("Outbreak ", ob_id,
                     ": no observations after this outbreak period — ",
                     "tail structure cannot be verified.")))
          } else {
            # Count how many consecutive non-outbreak low-risk weeks follow
            non_ob <- (is.na(after$outbreak_number) | after$outbreak_number == 0)
            low    <- after$risk == "low"
            non_ob_low <- non_ob & low

            rle_res       <- rle(non_ob_low)
            n_leading_low <- if (rle_res$values[1]) rle_res$lengths[1] else 0L

            # The algorithm places ~2 tail rows inside the outbreak period;
            # expect at least tail_period - 2 consecutive non-outbreak low-risk
            # weeks immediately after.
            min_expected <- max(0L, tail_period - 2L)
            push(make_row(loc, ob_id, "tail_period_after_outbreak",
              if (n_leading_low >= min_expected) "PASS" else "FAIL",
              n_leading_low,
              paste0("Outbreak ", ob_id, ": ",
                     n_leading_low, " consecutive non-outbreak low-risk week(s) ",
                     "immediately follow (expected >= ", min_expected,
                     " given tail_period = ", tail_period, ").")))
          }
        }

        # ----------------------------------------------------------------------
        # CHECK 11 — outbreak_summary  (INFO)
        # Informational summary: total cases, duration, peak weekly cases.
        # ----------------------------------------------------------------------
        if ("sCh" %in% names(ob)) {
          total_c <- sum(ob$sCh, na.rm = TRUE)
          peak_c  <- max(ob$sCh, na.rm = TRUE)
          push(make_row(loc, ob_id, "outbreak_summary",
            "INFO", total_c,
            paste0("Outbreak ", ob_id, ": ", nrow(ob), " week(s), ",
                   round(total_c, 1), " total suspected cases, ",
                   round(peak_c, 1), " peak weekly cases.")))
        }
      }  # end outbreak-level loop

      # -----------------------------------------------------------------------
      # CHECK 12 — inter_outbreak_gap_weeks
      # The gap in time between consecutive outbreaks must be >= tail_period
      # weeks (otherwise the algorithm would have merged them).
      # One result row per consecutive outbreak pair.
      # -----------------------------------------------------------------------
      if (length(ob_ids) > 1) {
        for (k in seq_len(length(ob_ids) - 1L)) {
          ob_a    <- d[!is.na(d$outbreak_number) & d$outbreak_number == ob_ids[k], ]
          ob_b    <- d[!is.na(d$outbreak_number) & d$outbreak_number == ob_ids[k + 1L], ]
          end_a   <- max(ob_a$TR)
          start_b <- min(ob_b$TL)
          gap_w   <- as.numeric(start_b - end_a) / 7
          pair_id <- paste0(ob_ids[k], "-", ob_ids[k + 1L])
          push(make_row(loc, pair_id, "inter_outbreak_gap_weeks",
            if (gap_w >= tail_period) "PASS" else "FAIL",
            gap_w,
            paste0("Outbreaks ", ob_ids[k], " \u2192 ", ob_ids[k + 1L], ": ",
                   "gap = ", round(gap_w, 1), " week(s) ",
                   "(minimum required: ", tail_period, ").")))
        }
      }

    }  # end "outbreak_number" block
  }    # end location loop

  # Combine and return
  dplyr::bind_rows(results[seq_len(ri)])
}
