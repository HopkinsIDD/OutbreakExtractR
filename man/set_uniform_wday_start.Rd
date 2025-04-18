% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_uniform_wday_start.R
\name{set_uniform_wday_start}
\alias{set_uniform_wday_start}
\title{set_uniform_wday_start}
\usage{
set_uniform_wday_start(original_data, week_start = "Mon", ...)
}
\arguments{
\item{original_data}{dataframe with location, epiweek, TL, TR, sCh, cCh, deaths}

\item{wk_start}{day of the week that should be considered the week start (default: "Mon")}
}
\description{
Change TL, TR and start_weekday to user-specified uniform day of the week
}
