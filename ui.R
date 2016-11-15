######### Packages #########
library(shiny) 
library(ggplot2)
library(readxl)
library(ggrepel)
library(reshape2)
library(grid)
library(vegan)
library(knitr)
library(igraph)
library(RColorBrewer)
library(data.table)
library(DT)
library(DESeq2)
library(ggdendro)
library(stringr)
library(dplyr)
library(magrittr)
library(gridExtra)
library(scales)

######### Sourcefiles #########
source("sourcefiles/functions.R", local = FALSE)
source("sourcefiles/choosedata_ui.R", local = FALSE)
source("sourcefiles/plot_ui.R", local = FALSE)

######### User Interface #########
navbarPage(
  title = h4(a(href="javascript:history.go(0)", style="color:#606060", "Amplicon Visualiser")),
  tabPanel("Choose data", choosedata_sbp, choosedata_mp),
  tabPanel("Plots", plot_sbp, plot_mp),
  #navbarMenu("More",
  #           tabPanel("About", 
  #                    h4("About"),
  #                    p("Made by Kasper Skytte Andersen, contact: knaldhat@gmail.com")
  #                    )
  #           ),
  #collapsible = TRUE,
  windowTitle = "Amplicon Visualiser"
)
