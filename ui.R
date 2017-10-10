######### Packages #########
library(shiny) 
library(DT)
library(readxl)

library(ggplot2)
library(ggrepel)
library(vegan)
library(RColorBrewer)
library(Cairo)
library(data.table)
library(stringr)
library(dplyr)
library(scales)
library(plotly)
#library(webshot)
library(htmlwidgets)
library(shinyjs)

######### Sourcefiles #########
source("sourcefiles/choosedata_ui.R", local = FALSE)
source("sourcefiles/plot_ui.R", local = FALSE)
source("sourcefiles/functions.R", local = FALSE)

#Source ampvis2 functions from github release 2.2.5
#devtools::source_url("https://raw.githubusercontent.com/MadsAlbertsen/ampvis2/2.2.5/R/amp_load.R")
#devtools::source_url("https://raw.githubusercontent.com/MadsAlbertsen/ampvis2/2.2.5/R/amp_rename.R")
#devtools::source_url("https://raw.githubusercontent.com/MadsAlbertsen/ampvis2/2.2.5/R/amp_heatmap.R")
#devtools::source_url("https://raw.githubusercontent.com/MadsAlbertsen/ampvis2/2.2.5/R/amp_ordinate.R")
#devtools::source_url("https://raw.githubusercontent.com/MadsAlbertsen/ampvis2/2.2.5/R/amp_boxplot.R")

######### User Interface #########
navbarPage(
  title = h4(a(href="javascript:history.go(0)", style="color:#606060", "Amplicon Visualiser")),
  windowTitle = "Amplicon Visualiser",
  id = "currentTab",
  tabPanel("Data and Filtering", choosedata_sbp, choosedata_mp),
  navbarMenu("Analysis",
    ui_heatmap,
    ui_boxplot,
    ui_ordination
  )
)
