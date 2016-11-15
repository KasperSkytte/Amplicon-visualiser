######### for UI #########
plot_sbp <- sidebarPanel(
  width = 3,
  position = "left",
  h4("Options: "),
  conditionalPanel(
    condition = "input.plot_type == 'Heatmap'",
    uiOutput("heatmap_UI_tax.group"),
    checkboxGroupInput(inputId = "heatmap_tax.add",
                       label = "Extra taxonomic information:",
                       choices = c("Class", "Order", "Family", "Species"),
                       selected = NULL
    ),
    
    sliderInput(inputId = "heatmap_tax.show",
                label = "Number of taxa to show:",
                min = 1,
                max = 50,
                value = 20
    ),
    checkboxInput(inputId = "heatmap_plot.numbers",
                  label = "Plot numbers", 
                  value = TRUE
    )
  ),
  conditionalPanel(
    condition = "input.plot_type == 'PCA'",
    uiOutput("PCA_UI_group"),
    uiOutput("PCA_UI_shape"),
    radioButtons(inputId = "pca_plot_group",
                  label = "Display group as",
                  choices = c(Chull = "chull", Centroid = "centroid"),
                  selected = "chull"
                ),
    sliderInput(inputId = "pca_plot_nspecies",
                label = "Number of taxa to plot",
                min = 0,
                max = 20,
                value = 5
    )
  ),
  conditionalPanel(
    condition = "input.plot_type == 'Rank Abundance'",
    uiOutput("RA_UI_group"),
    checkboxGroupInput(inputId = "RA_tax.add",
                       label = "Extra taxonomic information:",
                       choices = c("Class", "Order", "Family", "Species"),
                       selected = NULL
    ),
    
    sliderInput(inputId = "RA_tax.show",
                label = "Number of taxa to show:",
                min = 1,
                max = 50,
                value = 10
    ),
    checkboxInput(inputId = "RA_flip",
                  label = "Flip axes",
                  value = FALSE)
  ),
  tags$hr(),
  actionButton("goplot", "Render plot"),
  tags$hr(),
  downloadButton("saveplot", "Save plot")
)

plot_mp <- mainPanel(
  width = 9,
  tabsetPanel(id = "plot_type",
              tabPanel("Heatmap", plotOutput("heatmap", height = 600)),
              tabPanel("Rank Abundance", plotOutput("RA", height = 600)),
              tabPanel("PCA", plotOutput("PCA", height = 600), tags$hr(), h4("Statistics"), verbatimTextOutput("pca_stats"))
              )
)