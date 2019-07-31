######### for UI #########

#sidebarpanel
choosedata_sbp <- sidebarPanel(
  position = "left",
  width = 3,
  h4("Choose data"),
  radioButtons(inputId = "chosendata",
               label = "",
               choices = c("MiDAS example data", "Upload data"),
               selected = "MiDAS example data"
  ),
  conditionalPanel(
    condition = "input.chosendata == 'MiDAS example data'",
    helpText("The MiDAS example data contains 16S rRNA amplicon sequencing data from 23 Danish wastewater treatment plants sampled a few times a year in 2014-2015. See ", a("http://midasfieldguide.org", href = "http://midasfieldguide.org"))
  ),
  conditionalPanel(
    condition = "input.chosendata == 'Upload data'",
    tags$hr(),
    fileInput(inputId = "upl_metadata",
              label = "Sample metadata (.xls(x)/.csv/.tsv/.txt)",
              multiple = FALSE,
              accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
    ),
    helpText("The metadata must be a table where sample ID's matching the sample ID's in the OTU table are in the first column. The metadata can contain any number of variables."),
    tags$b("Minimal example is available here:"),
    tags$br(),
    downloadLink(
      outputId = "dlexamplemetadata",
      label = "metadata.xlsx"
    ),
    tags$hr(),
    h5(tags$b("OTU table and taxonomy")),
    fileInput(inputId = "upl_otutable",
              label = "",
              multiple = FALSE,
              accept = c(".txt", ".csv", ".biom", ".tsv")
    ),
    radioButtons(inputId = "upl_data_type",
                 label = "Format",
                 choices = c("BIOM format" = "biom",
                             "usearch",
                             "Text file" = "text"),
                 selected = "biom"),
    conditionalPanel(
      condition = "input.upl_data_type == 'usearch'",
      helpText("With usearch the OTU table and taxonomy are in two separate files; OTU table is a CSV file and the taxonomy is in a .sintax file."),
      fileInput(inputId = "upl_sintax",
                label = "Sintax file",
                multiple = FALSE,
                accept = c(".sintax"))
    ),
    conditionalPanel(
      condition = "input.upl_data_type == 'text'",
      helpText("If the OTU table is a text or CSV file, the last 7 columns of the OTU table must be the taxonomy matching the OTU's (Kingdom -> Species)."),
      tags$b("Minimal example is available here:"),
      tags$br(),
      downloadLink(
        outputId = "dlexampleotutable",
        label = "otutable.csv"
      )
    )
  )
)

#mainpanel
choosedata_mp <- mainPanel(
  width = 9,
  tabsetPanel(
    tabPanel("Filter data",
             conditionalPanel(
               condition = "$('html').hasClass('shiny-busy')",
               p("Loading...")
             ),
             tags$hr(),
             checkboxInput(
               inputId = "filterdata_inv",
               label = "Inverse filtering",
               value = FALSE
             ),
             DT::dataTableOutput("filtermetadata", width = "100%")
             ),
    tabPanel("Filter taxa",
             conditionalPanel(
               condition = "$('html').hasClass('shiny-busy')",
               p("Loading...")
             ),
             tags$hr(),
             checkboxInput(
               inputId = "filtertaxa_inv",
               label = "Inverse filtering",
               value = FALSE
             ),
             DT::dataTableOutput("filtertaxa", width = "100%")
             ),
    tabPanel("About filtering",
             h4("Filtering"),
             p("You can filter the data by searching a column or multiple columns in either the metadata (filter data) or the taxonomy table (filter taxa) and the resulting rows will be used for plotting. For example you can search for 'Aalborg West' in the Plant column and 'Tetrasphaera' in the Genus column to only use data from Aalborg West containing Tetrasphaera. If the metadata is large you can choose which columns to show and drag-and-drop columns to reorder them. "), 
             p("Furthermore, you can choose to inverse the filtering so that the shown rows are those NOT used for plotting. This is useful for when you want to filter only a few samples out of many, without having to select all but a few."),
             p("Because this app is running on a free shinyapps.io account, the instances are limited to 1GB of working memory and cannot handle large datasets (~50MB or more) at the moment, unless the data is reduced by filtering. If you want to analyse larger data sets you can run the app on your local computer as described", a("here", href = "https://github.com/KasperSkytte/Amplicon-visualiser"))
             )
  )
)