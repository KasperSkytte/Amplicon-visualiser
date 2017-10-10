######### HEATMAP UI #########
ui_heatmap <- tabPanel("Heatmap", 
         sidebarPanel(
           width = 3,
           position = "left",
           h4("Heatmap"),
           tags$hr(),
           uiOutput("heatmap_UI_group_by"),
           uiOutput("heatmap_UI_facet_by"),
           a(id = "toggleHeatmapAdvanced", "Toggle advanced options", href = "#"),
           shinyjs::useShinyjs(),
           shinyjs::hidden(
             div(id = "heatmap_advanced",
                 tags$hr(),
                 selectInput(inputId = "heatmap_tax_aggregate",
                             label = "Taxa aggregate level",
                             choices = c("Phylum", "Class", "Order", "Family", "Genus", "Species", "OTU"),
                             selected = "Genus"
                 ),
                 uiOutput("heatmap_UI_tax_add"),
                 checkboxGroupInput(inputId = "heatmap_cluster",
                                    label = "Cluster axes by similarity",
                                    choices = c("Cluster x-axis", "Cluster y-axis"),
                                    selected = NULL
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
           ),
           tags$hr(),
           downloadButton("saveHeatmap", "Save plot"),
           tags$br(),
           tags$br(),
           a(id = "toggleDimHeatmap", "Dimensions", href = "#"),
           p(),
           shinyjs::useShinyjs(),
           shinyjs::hidden(
             div(id = "save_dimensions_heatmap",
                 tags$hr(),
                 sliderInput(inputId = "save_width_heatmap",
                             label = "Width (inches)",
                             min = 1,
                             max = 20,
                             value = 10,
                             step = 1,
                             ticks = FALSE,
                             width = "200px"
                 ),
                 sliderInput(inputId = "save_height_heatmap",
                             label = "Height (inches)",
                             min = 1,
                             max = 20,
                             value = 10,
                             step = 1,
                             ticks = FALSE,
                             width = "200px"
                 )
             )
         )
         ),
         mainPanel(
           width = 9,
           plotOutput("heatmap", height = 800)
         )
)

######### BOXPLOT UI #########
ui_boxplot <- tabPanel("Boxplot", 
                       sidebarPanel(
                         width = 3,
                         position = "left",
                         h4("Boxplot"),
                         tags$hr(),
                         uiOutput("boxplot_UI_group_by"),
                         a(id = "toggleBoxplotAdvanced", "Toggle advanced options", href = "#"),
                         shinyjs::useShinyjs(),
                         shinyjs::hidden(
                           div(id = "boxplot_advanced",
                               tags$hr(),
                               selectInput(inputId = "boxplot_tax_aggregate",
                                           label = "Taxa aggregate level",
                                           choices = c("Phylum", "Class", "Order", "Family", "Genus", "Species", "OTU"),
                                           selected = "Genus"
                               ),
                               uiOutput("boxplot_UI_tax_add"),
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
                           )
                         ),
                         tags$hr(),
                         conditionalPanel(
                           condition = "!$('html').hasClass('shiny-busy')",
                           actionButton("renderplot_boxplot", "Render plot")
                         ),
                         conditionalPanel(
                           condition = "$('html').hasClass('shiny-busy')",
                           p("Loading...")
                         ),
                         tags$hr(),
                         downloadButton("saveBoxplot", "Save plot"),
                         tags$br(),
                         tags$br(),
                         a(id = "toggleDimBoxplot", "Dimensions", href = "#"),
                         p(),
                         shinyjs::useShinyjs(),
                         shinyjs::hidden(
                           div(id = "save_dimensions_boxplot",
                               tags$hr(),
                               sliderInput(inputId = "save_width_boxplot",
                                           label = "Width (inches)",
                                           min = 1,
                                           max = 20,
                                           value = 10,
                                           step = 1,
                                           ticks = FALSE,
                                           width = "200px"
                               ),
                               sliderInput(inputId = "save_height_boxplot",
                                           label = "Height (inches)",
                                           min = 1,
                                           max = 20,
                                           value = 10,
                                           step = 1,
                                           ticks = FALSE,
                                           width = "200px"
                               )
                           )
                         )
                       ),
                       mainPanel(
                         width = 9,
                         plotOutput("boxplot", height = 800)
                         )
)

######### ORDINATION UI #########
ui_ordination <- tabPanel("Ordination", 
                       sidebarPanel(
                         width = 3,
                         position = "left",
                         h4("Ordination"),
                         tags$hr(),
                         uiOutput("ord_UI_group"),
                         a(id = "toggleOrdinationAdvanced", "Toggle advanced options", href = "#"),
                         shinyjs::useShinyjs(),
                         shinyjs::hidden(
                           div(id = "ordination_advanced",
                               tags$hr(),
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
                               tags$hr(),
                               radioButtons(inputId = "ord_plotlytype",
                                            "Plotly layer",
                                            choices = c("Samples", "Species"),
                                            selected = "Samples")
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
                         ),
                         tags$hr(),
                         downloadButton("saveOrdination", "Save plot"),
                         tags$br(),
                         tags$br(),
                         a(id = "toggleDimOrdination", "Dimensions", href = "#"),
                         p(),
                         shinyjs::useShinyjs(),
                         shinyjs::hidden(
                           div(id = "save_dimensions_ordination",
                               tags$hr(),
                               sliderInput(inputId = "save_width_ordination",
                                           label = "Width (inches)",
                                           min = 1,
                                           max = 20,
                                           value = 10,
                                           step = 1,
                                           ticks = FALSE,
                                           width = "200px"
                               ),
                               sliderInput(inputId = "save_height_ordination",
                                           label = "Height (inches)",
                                           min = 1,
                                           max = 20,
                                           value = 10,
                                           step = 1,
                                           ticks = FALSE,
                                           width = "200px"
                               )
                           )
                         )
                       ),
                       mainPanel(
                         width = 9,
                         plotlyOutput("ord", height = 800, width = 800)
                       )
)