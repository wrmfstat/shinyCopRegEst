####################################################################

# Loading all required packages:

library(ggplot2)   # Enhanced graphical exhibition.
# library(tidyverse) # A hub of packages for data manipulation.
library(dplyr)     # Main package for data manipulation.
library(bslib)     # Allows choosing different Shiny themes.
library(shiny)     # The Shiny package for presentations.

# Reading the summary tables:

# url_tr = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/tab_reg.rds"
# tr = try(readRDS(url(url_tr)), TRUE)

tr = readRDS("res_exc_cop/tab_reg.rds")
tr = tr[-c(7201:7203),]

# Recoding all parameter names, to their corresponding "unicode", in
# the summary table:

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

# Vectors for simulation scenarios (note that results are divided by
# regression class and true correlation):

# tau = c(0.25, 0.50, 0.75)
gcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
gbsl = c("EW", "W")
fcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
fbsl = c("BP", "PE", "W")
# reg = c("PH", "PO", "YP")

# Loading the user interface to apply boxplots and tables:

# source("https://github.com/wrmfstat/shinyCopRegEst/blob/master/ui.R")
source("ui.R")

# Loading the corresponding server:

# source("https://github.com/wrmfstat/shinyCopRegEst/blob/master/server.R")
source("server.R")

# Running all 9 sub datasets at a single application:

shinyApp(ui=ui, server=server)