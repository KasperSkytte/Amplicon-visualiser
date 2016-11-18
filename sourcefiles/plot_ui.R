######### for UI #########
#siderbarpanel
plot_sbp <- sidebarPanel(
  width = 3,
  position = "left",
  h4("Options: "),
  ################## Heatmap ##################
  conditionalPanel(
    condition = "input.plot_type == 'Heatmap'",
    uiOutput("heatmap_UI_tax.group"),
    checkboxInput(inputId = "heatmap_moreoptions",
                  label = "More options",
                  value = FALSE),
    conditionalPanel(
      condition = "input.heatmap_moreoptions",
      uiOutput("heatmap_UI_tax.add"),
      selectInput(inputId = "heatmap_tax.aggregate",
                  label = "Taxa aggregate level:",
                  choices = c("Class", "Order", "Family", "Genus", "Species"),
                  selected = "Genus"
      ),
      checkboxInput(inputId = "heatmap_cluster_x",
                    label = "Cluster x-axis",
                    value = FALSE
                    ),
      checkboxInput(inputId = "heatmap_cluster_y",
                    label = "Cluster y-axis",
                    value = FALSE
                    ),
      sliderInput(inputId = "heatmap_tax.show",
                  label = "Number of taxa to show:",
                  min = 1,
                  max = 50,
                  value = 20,
                  step = 1
      ),
      checkboxInput(inputId = "heatmap_plot.numbers",
                    label = "Plot numbers", 
                    value = TRUE
      )
    ),
    tags$hr(),
    conditionalPanel(
      condition = "!$('html').hasClass('shiny-busy')",
      actionButton("renderplot_heatmap", "Render plot")
    ),
    conditionalPanel(
      condition = "$('html').hasClass('shiny-busy')",
      p("Loading...")
    )
  ),
  ################## PCA ##################
  conditionalPanel(
    condition = "input.plot_type == 'Principal Component Analysis (PCA)'",
    uiOutput("PCA_UI_group"),
    checkboxInput(inputId = "pca_moreoptions",
                  label = "More options",
                  value = FALSE),
    conditionalPanel(
      condition = "input.pca_moreoptions",
      radioButtons(inputId = "pca_plot_group",
                   label = "Display group as",
                   choices = c(Chull = "chull", Centroid = "centroid"),
                   selected = "chull"
      ),
      sliderInput(inputId = "pca_plot_nspecies",
                  label = "Number of taxa to plot",
                  min = 0,
                  max = 20,
                  value = 0,
                  step = 1
      ),
      uiOutput("PCA_UI_shape")
      #checkboxInput(inputId = "PCA_showstats",
      #              label = "Show statistics",
      #              value = FALSE)
    ),
    tags$hr(),
    conditionalPanel(
      condition = "!$('html').hasClass('shiny-busy')",
      actionButton("renderplot_PCA", "Render plot")
    ),
    conditionalPanel(
      condition = "$('html').hasClass('shiny-busy')",
      p("Loading...")
    )
  ),
  ################## Rank Abundance ##################
  conditionalPanel(
    condition = "input.plot_type == 'Rank Abundance'",
    uiOutput("RA_UI_group"),
    checkboxInput(inputId = "RA_moreoptions",
                  label = "More options",
                  value = FALSE),
    conditionalPanel(
      condition = "input.RA_moreoptions",
      checkboxGroupInput(inputId = "RA_tax.add",
                         label = "Extra taxonomic information:",
                         choices = c("Class", "Order", "Family", "Species"),
                         selected = NULL
      ),
      sliderInput(inputId = "RA_tax.show",
                  label = "Number of taxa to show:",
                  min = 1,
                  max = 50,
                  value = 10,
                  step = 1
      ),
      checkboxInput(inputId = "RA_flip",
                    label = "Flip axes",
                    value = FALSE)
      ),
    tags$hr(),
    conditionalPanel(
      condition = "!$('html').hasClass('shiny-busy')",
      actionButton("renderplot_RA", "Render plot")
    ),
    conditionalPanel(
      condition = "$('html').hasClass('shiny-busy')",
      p("Loading...")
    )
    ),
  tags$hr(),
  downloadButton("saveplot", "Save plot")
)

#mainpanel
plot_mp <- mainPanel(
  width = 9,
  tabsetPanel(id = "plot_type",
              tabPanel("Heatmap", plotOutput("heatmap", height = 600)),
              tabPanel("Rank Abundance", plotOutput("RA", height = 600)),
              tabPanel("Principal Component Analysis (PCA)", 
                       plotOutput("PCA", height = 600), 
                       conditionalPanel(
                         condition = "input.PCA_showstats",
                       tags$hr(), 
                       h4("Statistics"), 
                       verbatimTextOutput("pca_stats"))
              )
              )
)