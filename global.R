library(shiny)
library(data.table)

naep <- fread("naep_clean.csv")
naep$year <- as.numeric(naep$year)
