

################################################################################
## Bayesian Joint Modelling for Allograft and patients' death
## Date: 2023-08-01
## Update: 2023-08-24
################################################################################

#rm(list = ls())

library("shiny")
library("shinythemes")
library("shinyalert")
library("plotly")
library("dplyr")
library("ggplot2")
library("rstan")
library("rstanarm")
options(mc.cores = parallel::detectCores())

source("appJM.R")
shinyApp(ui, server)
