#' ---
#' title: "03_two_month_ahead_forecasting"
#' author: "K Joshi"
#' 
#' ---
#'
#' Overview
#' ========
#' Take most up to date data and generate forecasts for two months ahead, extrapolating from a seasonal baseline. 
#' Visualise forecasts using a radial barplot. 
#' Identify whether a year is expected to be 'good' or 'bad' and colour the barplot accordingly.


library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)


