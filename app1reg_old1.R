# Cleaning the working environment:
rm(list=ls(all=TRUE))

# Packages to be loaded:

library(ggplot2)   # Enhanced graphical exhibition.
# library(tidyverse) # A hub of packages for data manipulation.
library(dplyr)     # Main package for data manipulation.
library(bslib)     # Allows choosing different Shiny themes.
library(shiny)     # The Shiny package for presentations.

# Setting the environment to the project directory:

setwd("~/shinyCopRegEst")

# Reading locally the results (by reg. class and correlation):

r_25_ph = readRDS("res_exc_cop/res_reg_ph025.rds")
r_25_po = readRDS("res_exc_cop/res_reg_po025.rds")
r_25_yp = readRDS("res_exc_cop/res_reg_yp025.rds")
r_50_ph = readRDS("res_exc_cop/res_reg_ph050.rds")
r_50_po = readRDS("res_exc_cop/res_reg_po050.rds")
r_50_yp = readRDS("res_exc_cop/res_reg_yp050.rds")
r_75_ph = readRDS("res_exc_cop/res_reg_ph075.rds")
r_75_po = readRDS("res_exc_cop/res_reg_po075.rds")
r_75_yp = readRDS("res_exc_cop/res_reg_yp075.rds")

# Reading the summary tables:

tr = readRDS("res_exc_cop/tab_reg.rds")
tr = tr[-c(7201:7203),]

# Recoding parameter names to their corresponding "unicode" (only in
# the summary table):

r_25_ph$Parameter = case_when(
  r_25_ph$Parameter=="psi[1,1]" ~ "beta[1,1]",
  r_25_ph$Parameter=="psi[1,2]" ~ "beta[1,2]",
  r_25_ph$Parameter=="psi[2,1]" ~ "beta[2,1]",
  r_25_ph$Parameter=="psi[2,2]" ~ "beta[2,2]")
r_25_po$Parameter = case_when(
  r_25_po$Parameter=="psi[1,1]" ~ "beta[1,1]",
  r_25_po$Parameter=="psi[1,2]" ~ "beta[1,2]",
  r_25_po$Parameter=="psi[2,1]" ~ "beta[2,1]",
  r_25_po$Parameter=="psi[2,2]" ~ "beta[2,2]")
r_25_yp$Parameter = case_when(
  r_25_yp$Parameter=="psi[1,1]" ~ "beta^(S)[1,1]",
  r_25_yp$Parameter=="psi[1,2]" ~ "beta^(S)[1,2]",
  r_25_yp$Parameter=="psi[2,1]" ~ "beta^(S)[2,1]",
  r_25_yp$Parameter=="psi[2,2]" ~ "beta^(S)[2,2]",
  r_25_yp$Parameter=="phi[1,1]" ~ "beta^(L)[1,1]",
  r_25_yp$Parameter=="phi[1,2]" ~ "beta^(L)[1,2]",
  r_25_yp$Parameter=="phi[2,1]" ~ "beta^(L)[2,1]",
  r_25_yp$Parameter=="phi[2,2]" ~ "beta^(L)[2,2]")
r_50_ph$Parameter = case_when(
  r_50_ph$Parameter=="psi[1,1]" ~ "beta[1,1]",
  r_50_ph$Parameter=="psi[1,2]" ~ "beta[1,2]",
  r_50_ph$Parameter=="psi[2,1]" ~ "beta[2,1]",
  r_50_ph$Parameter=="psi[2,2]" ~ "beta[2,2]")
r_50_po$Parameter = case_when(
  r_50_po$Parameter=="psi[1,1]" ~ "beta[1,1]",
  r_50_po$Parameter=="psi[1,2]" ~ "beta[1,2]",
  r_50_po$Parameter=="psi[2,1]" ~ "beta[2,1]",
  r_50_po$Parameter=="psi[2,2]" ~ "beta[2,2]")
r_50_yp$Parameter = case_when(
  r_50_yp$Parameter=="psi[1,1]" ~ "beta^(S)[1,1]",
  r_50_yp$Parameter=="psi[1,2]" ~ "beta^(S)[1,2]",
  r_50_yp$Parameter=="psi[2,1]" ~ "beta^(S)[2,1]",
  r_50_yp$Parameter=="psi[2,2]" ~ "beta^(S)[2,2]",
  r_50_yp$Parameter=="phi[1,1]" ~ "beta^(L)[1,1]",
  r_50_yp$Parameter=="phi[1,2]" ~ "beta^(L)[1,2]",
  r_50_yp$Parameter=="phi[2,1]" ~ "beta^(L)[2,1]",
  r_50_yp$Parameter=="phi[2,2]" ~ "beta^(L)[2,2]")
r_75_ph$Parameter = case_when(
  r_75_ph$Parameter=="psi[1,1]" ~ "beta[1,1]",
  r_75_ph$Parameter=="psi[1,2]" ~ "beta[1,2]",
  r_75_ph$Parameter=="psi[2,1]" ~ "beta[2,1]",
  r_75_ph$Parameter=="psi[2,2]" ~ "beta[2,2]")
r_75_po$Parameter = case_when(
  r_75_po$Parameter=="psi[1,1]" ~ "beta[1,1]",
  r_75_po$Parameter=="psi[1,2]" ~ "beta[1,2]",
  r_75_po$Parameter=="psi[2,1]" ~ "beta[2,1]",
  r_75_po$Parameter=="psi[2,2]" ~ "beta[2,2]")
r_75_yp$Parameter = case_when(
  r_75_yp$Parameter=="psi[1,1]" ~ "beta^(S)[1,1]",
  r_75_yp$Parameter=="psi[1,2]" ~ "beta^(S)[1,2]",
  r_75_yp$Parameter=="psi[2,1]" ~ "beta^(S)[2,1]",
  r_75_yp$Parameter=="psi[2,2]" ~ "beta^(S)[2,2]",
  r_75_yp$Parameter=="phi[1,1]" ~ "beta^(L)[1,1]",
  r_75_yp$Parameter=="phi[1,2]" ~ "beta^(L)[1,2]",
  r_75_yp$Parameter=="phi[2,1]" ~ "beta^(L)[2,1]",
  r_75_yp$Parameter=="phi[2,2]" ~ "beta^(L)[2,2]")

tr$Parameter = case_when(
  tr$Parameter=="psi[1,1]" &
    tr$Reg.Class!="YP"~"\u03B2\U2081\U2081",
  tr$Parameter=="psi[1,2]" &
    tr$Reg.Class!="YP"~"\u03B2\U2081\U2082",
  tr$Parameter=="psi[2,1]" &
    tr$Reg.Class!="YP"~"\u03B2\U2082\U2081",
  tr$Parameter=="psi[2,2]" &
    tr$Reg.Class!="YP"~"\u03B2\U2082\U2082",
  tr$Parameter=="psi[1,1]" &
    tr$Reg.Class=="YP"~"\u03B2^(S)\U2081\U2081",
  tr$Parameter=="psi[1,2]" &
    tr$Reg.Class=="YP"~"\u03B2^(S)\U2081\U2082",
  tr$Parameter=="psi[2,1]" &
    tr$Reg.Class=="YP"~"\u03B2^(S)\U2082\U2081",
  tr$Parameter=="psi[2,2]" &
    tr$Reg.Class=="YP"~"\u03B2^(S)\U2082\U2082",
  tr$Parameter=="phi[1,1]" &
    tr$Reg.Class=="YP"~"\u03B2^(L)\U2081\U2081",
  tr$Parameter=="phi[1,2]" &
    tr$Reg.Class=="YP"~"\u03B2^(L)\U2081\U2082",
  tr$Parameter=="phi[2,1]" &
    tr$Reg.Class=="YP"~"\u03B2^(L)\U2082\U2081",
  tr$Parameter=="phi[2,2]" &
    tr$Reg.Class=="YP"~"\u03B2^(L)\U2082\U2082")

# Dividing summary tables by regression class and correlations:

tr_25_ph = tr %>% filter(Reg.Class=="PH" & TrueTau==0.25)
tr_25_po = tr %>% filter(Reg.Class=="PO" & TrueTau==0.25)
tr_25_yp = tr %>% filter(Reg.Class=="YP" & TrueTau==0.25)
tr_50_ph = tr %>% filter(Reg.Class=="PH" & TrueTau==0.50)
tr_50_po = tr %>% filter(Reg.Class=="PO" & TrueTau==0.50)
tr_50_yp = tr %>% filter(Reg.Class=="YP" & TrueTau==0.50)
tr_75_ph = tr %>% filter(Reg.Class=="PH" & TrueTau==0.75)
tr_75_po = tr %>% filter(Reg.Class=="PO" & TrueTau==0.75)
tr_75_yp = tr %>% filter(Reg.Class=="YP" & TrueTau==0.75)

rm(tr)

# Vectors for simulation scenarios (note that results are divided by
# regression class and true correlation):

# tau = c(0.25, 0.50, 0.75)
gcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
gbsl = c("EW", "W")
fcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
fbsl = c("BP", "PE", "W")
# reg = c("PH", "PO", "YP")

# Parameter vectors for results' presentation:

coef_ph = c("beta[1,1]", "beta[2,1]",
            "beta[1,2]", "beta[2,2]")
coef_ph_uc = c("\u03B2\U2081\U2081", "\u03B2\U2082\U2081",
               "\u03B2\U2081\U2082", "\u03B2\U2082\U2082")

coef_po = c("beta[1,1]", "beta[2,1]",
            "beta[1,2]", "beta[2,2]")
coef_po_uc = c("\u03B2\U2081\U2081", "\u03B2\U2082\U2081",
               "\u03B2\U2081\U2082", "\u03B2\U2082\U2082")

coef_yp = c("beta^(S)[1,1]", "beta^(S)[2,1]",
            "beta^(S)[1,2]", "beta^(S)[2,2]",
            "beta^(L)[1,1]", "beta^(L)[2,1]",
            "beta^(L)[1,2]", "beta^(L)[2,2]")
coef_yp_uc = c("\u03B2^(S)\U2081\U2081", "\u03B2^(S)\U2082\U2081",
               "\u03B2^(S)\U2081\U2082", "\u03B2^(S)\U2082\U2082",
               "\u03B2^(L)\U2081\U2081", "\u03B2^(L)\U2082\U2081",
               "\u03B2^(L)\U2081\U2082", "\u03B2^(L)\U2082\U2082")

# Loading all user interfaces to apply boxplots and tables:

source("copShinyUIs_Reg.R")

# Loading all corresponding servers:

source("copShinyServers_Reg.R")

# Run the following applications one at a time:

shinyApp(ui = ui_25_ph, server = server_25_ph)
shinyApp(ui = ui_25_po, server = server_25_po)
shinyApp(ui = ui_25_yp, server = server_25_yp)
shinyApp(ui = ui_50_ph, server = server_50_ph)
shinyApp(ui = ui_50_po, server = server_50_po)
shinyApp(ui = ui_50_yp, server = server_50_yp)
shinyApp(ui = ui_75_ph, server = server_75_ph)
shinyApp(ui = ui_75_po, server = server_75_po)
shinyApp(ui = ui_75_yp, server = server_75_yp)