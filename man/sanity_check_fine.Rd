% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_checks.R
\name{sanity_check_fine}
\alias{sanity_check_fine}
\title{Fine QAQC processing}
\usage{
sanity_check_fine(
  df,
  Dates = unique(df$Date),
  Pre_Threshold_Temp_Upper = 0.2,
  Pre_Threshold_Temp_Lower = 0.2,
  Post_Threshold_Temp_Upper = 5,
  Post_Threshold_Temp_Lower = 0.2,
  Heat_Threshold_Temp = 0.5
)
}
\arguments{
\item{df}{The input data frame}

\item{Dates}{The dates / date range to process}

\item{Pre_Threshold_Temp_Upper}{Difference in mode temp (during the pre-pulse
period) greater than which a point will be considered an outlier}

\item{Pre_Threshold_Temp_Lower}{Difference in mode temp (during the pre-pulse
period) less than which a point will be considered an outlier}

\item{Post_Threshold_Temp_Upper}{Difference in mode temp (during the post-pulse
period) greater than which a point will be considered an outlier}

\item{Post_Threshold_Temp_Lower}{Difference in mode temp (during the post-pulse
period) less than which a point will be considered an outlier}

\item{Heat_Threshold_Temp}{Value which when added to mode of the
pre-pulse period becomes the threshold over which the max temp observed during heat pulse  must pass for the heater to be considered working}
}
\value{
A list containing data frame with outlines removed (replaced by NA),
two data frames corresponding to the upper and lower temperature
thresholds for each data point, and a data frame listing the status of each heater (Good/Bad)
for each pulse and date.
}
\description{
Fine QAQC processing. Breaks each pulse interval into two
periods, pre-pulse(00:00-02:00), and post post-pulse (02:01-15:00).
Identifies and flags outliers as temperature readings that exceed a
user specified upper and lower threshold temperature reading during that
period. Output is a list containing data frame with outliers removed, and
two data frames corresponding to the upper and lower temperature thresholds
for each data point.
}
\author{
Kenneth Davidson
}
