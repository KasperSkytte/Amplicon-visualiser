######### for UI #########
#siderbarpanel
plot_sbp <- sidebarPanel(
  width = 3,
  position = "left",
  h4("Options"),
  ################## Heatmap ##################
  conditionalPanel(
    condition = "input.plot_type == 'Heatmap'",
    uiOutput("heatmap_UI_tax_group"),
    checkboxInput(inputId = "heatmap_moreoptions",
                  label = "More options",
                  value = FALSE),
    conditionalPanel(
      condition = "input.heatmap_moreoptions",
      uiOutput("heatmap_UI_tax_add"),
      selectInput(inputId = "heatmap_tax_aggregate",
                  label = "Taxa aggregate level",
                  choices = c("Phylum", "Class", "Order", "Family", "Genus", "Species", "OTU"),
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
      sliderInput(inputId = "heatmap_tax_show",
                  label = "Number of taxa to show",
                  min = 1,
                  max = 50,
                  value = 20,
                  step = 1
      ),
      checkboxInput(inputId = "heatmap_plot_values",
                    label = "Plot values", 
                    value = TRUE
      ),
      textInput(inputId = "heatmap_colorvector",
                label = "Custom color palette (logscale)",
                value = "Deepskyblue, White, Tomato"),
      tags$a(href = "http://www.bxhorn.com/Downloads/RColors1_bxhorn.pdf", target = "_blank", "R color sheet")
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
    condition = "input.plot_type == 'Ordination'",
    uiOutput("ord_UI_group"),
    checkboxInput(inputId = "ord_moreoptions",
                  label = "More options",
                  value = FALSE),
    conditionalPanel(
      condition = "input.ord_moreoptions",
      selectizeInput(inputId = "ord_transform",
                     label = "Data transformation",
                     choices = c("Hellinger" = "hellinger", "Normalise" = "normalize", "Chi-square" = "chi.square", "Presence/absence" = "pa", "Logarithmic" = "log", "Square-root" = "sqrt", "None" = "none"),
                     selected = "hellinger",
                     multiple = FALSE),
      selectizeInput(inputId = "ord_type",
                  label = "Ordination type",
                  choices = list("Unconstrained Eigenanalyses" = c("PCA - Principal Components Analysis " = "pca", "CA - Correspondence Analysis" = "ca", "DCA - Detrended Correspondence Analysis" = "dca"),
                                 "Constrained Eigenanalyses" = c("RDA - Redundancy Analysis" = "rda", "CCA - Canonical Correspondence Analysis" = "cca"),
                                 "Distance-based Analyses" = c("PCoA - Principal Coordinates Analysis" = "pcoa", "nMDS - non-Metric Multidimensional Scaling" = "nmds")),
                  selected = "pca",
                  multiple = FALSE),
      conditionalPanel(
        condition = "input.ord_type == 'pcoa' | input.ord_type == 'nmds'",
        selectizeInput(inputId = "ord_distmeasure",
                    label = "Distance measure",
                    choices = c("Bray-Curtis" = "bray", "Manhattan" = "manhattan", "Euclidean" = "euclidean", "Canberra" = "canberra", "Kulczynski" = "kulczynski", "Jaccard" = "jaccard", "Gower" = "gower", "None" = "none"),
                    selected = "bray",
                    multiple = FALSE),
        helpText("Note: using both transformation AND a distance measure is not recommended for distance-based ordination.")
        ),
      conditionalPanel(
        condition = "input.ord_type == 'rda' | input.ord_type == 'cca'",
        uiOutput("ord_UI_constrain")
      ),
      conditionalPanel(
        condition = "input.ord_type == 'pca' | input.ord_type == 'rda'",
        tags$hr(),
        sliderInput(inputId = "ord_nspecies",
                    label = "Number of species to plot",
                    min = 0,
                    max = 20,
                    value = 0,
                    step = 1
        )
      )
    ),
    tags$hr(),
    conditionalPanel(
      condition = "!$('html').hasClass('shiny-busy')",
      actionButton("renderplot_ord", "Render plot")
    ),
    conditionalPanel(
      condition = "$('html').hasClass('shiny-busy')",
      p("Loading...")
    )
  ),
  ################## Boxplot ##################
  conditionalPanel(
    condition = "input.plot_type == 'Boxplot'",
    uiOutput("boxplot_UI_group"),
    checkboxInput(inputId = "boxplot_moreoptions",
                  label = "More options",
                  value = FALSE),
    conditionalPanel(
      condition = "input.boxplot_moreoptions",
      uiOutput("boxplot_UI_tax_add"),
      selectInput(inputId = "boxplot_tax_aggregate",
                  label = "Taxa aggregate level",
                  choices = c("Phylum", "Class", "Order", "Family", "Genus", "Species", "OTU"),
                  selected = "Genus"
      ),
      sliderInput(inputId = "boxplot_tax_show",
                  label = "Number of taxa to show",
                  min = 1,
                  max = 50,
                  value = 10,
                  step = 1
      ),
      checkboxInput(inputId = "boxplot_flip",
                    label = "Flip axes",
                    value = FALSE)
      ),
    tags$hr(),
    conditionalPanel(
      condition = "!$('html').hasClass('shiny-busy')",
      actionButton("renderplot_boxplot", "Render plot")
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
              tabPanel("Boxplot", plotOutput("boxplot", height = 600)),
              tabPanel("Ordination", 
                       plotOutput("ord", height = 600)
              )
              )
)