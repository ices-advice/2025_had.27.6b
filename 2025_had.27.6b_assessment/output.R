## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)
library(tidyverse)
library(stockassessment)
library(ggplot2)

mkdir("output")
mkdir("output/tables")
source("output_tables.R")

mkdir("output/figures")
source("output_figures.R")

source("output_forecast_summary.R")

source("output_compare_last_advice.R")

