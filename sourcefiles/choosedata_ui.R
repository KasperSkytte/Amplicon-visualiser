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
    condition = "input.chosendata == 'Upload data'",
    tags$hr(),
    h4("Upload data"),
    radioButtons(inputId = "upl_type",
                 label = "",
                 choices = c("Two files (metadata and OTU table)", "One file (.RData)")
                 ),
    conditionalPanel(
      condition = "input.upl_type == 'Two files (metadata and OTU table)'",
      #Upload metadata and otutable separately
      fileInput(inputId = "upl_metadata",
                label = "Metadata (.xlsx/.csv)",
                multiple = FALSE,
                accept = c(".xlsx", ".xls", ".csv")
                ),
      fileInput(inputId = "upl_otutable",
                label = "OTU table  (.txt/.csv)",
                multiple = FALSE,
                accept = c(".txt", ".csv")
                ),
      tags$hr(),
      selectInput(inputId = "csvsep",
                  label = "CSV separator",
                  multiple = FALSE,
                  choices = c("Tabular", "Comma ','", "Semicolon ';'"),
                  selected = "Tabular"
                  ),
      selectInput(inputId = "csvdec",
                  label = "CSV decimal",
                  multiple = FALSE,
                  choices = c("Dot '.'", "Comma ','"),
                  selected = "Dot"
                  ),
      tags$hr(),
      helpText("If you get an error it either means you have chosen the wrong separator or the metadata does not match the OTU table. Uploading metadata is not required but recommended."),
      helpText("The metadata must contain sample ID's in the first column matching the sample ID's in the first row of the OTU table. The last 7 columns of the OTU table must be the taxonomy matching the OTU's (Kingdom -> Species)."),
      tags$b("Minimal examples of both files are available here:"),
      tags$br(),
      downloadLink(
        outputId = "dlexamplemetadata",
        label = "metadata.xlsx"
      ),
      tags$br(),
      downloadLink(
        outputId = "dlexampleotutable",
        label = "otutable.csv"
      )
      ),
    #Upload metadata and otutable in one .RData file
    conditionalPanel(
      condition = "input.upl_type == 'One file (.RData)'",
      fileInput(inputId = "upl_rdata",
                label = "Choose file (.RData)",
                multiple = FALSE,
                accept = c(".RData")
                ),
      helpText("Don't use the one file upload yet.")
      #,uiOutput("loadedobjectslist")
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