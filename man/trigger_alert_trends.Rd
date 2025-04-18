% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trigger_alert_trends.R
\name{trigger_alert_trends}
\alias{trigger_alert_trends}
\title{trigger_alert_trends}
\usage{
trigger_alert_trends(original_data)
}
\arguments{
\item{original_data}{dataframe of pre-alert time series extractions}
}
\value{
dataframe
}
\description{
Triggers alerts according to patterns in suspected case trends. Returns a dataframe with alert indicators aligned with the pre-alert timeseries extractions across multiple locations. Alert is TRUE for the week when the alert would have been identified. One alert is currently identified: 1) 3 consecutive declining or constant weeks (observe 4 weeks total) followed by 2 consecutive increasing weeks (observe 3 weeks total) in the 2-week rolling mean of cases
}
