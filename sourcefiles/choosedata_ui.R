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
                 choices = c("Two files (metadata and otutable)", "One file (.RData)")
                 ),
    conditionalPanel(
      condition = "input.upl_type == 'Two files (metadata and otutable)'",
      #Upload metadata and otutable separately
      fileInput(inputId = "upl_metadata",
                label = "Choose metadata file (.xlsx/.csv)",
                multiple = FALSE,
                accept = c(".xlsx", ".xls", ".csv")
                ),
      fileInput(inputId = "upl_otutable",
                label = "Choose otutable file (.txt/.csv)",
                multiple = FALSE,
                accept = c(".txt", ".csv")
                )
      ),
    #Upload metadata and otutable in one .RData file
    conditionalPanel(
      condition = "input.upl_type == 'One file (.RData)'",
      fileInput(inputId = "upl_rdata",
                label = "Choose file (.RData)",
                multiple = FALSE,
                accept = c(".RData"))
    )
  )
)

#mainpanel
choosedata_mp <- mainPanel(
  width = 9,
  tabsetPanel(
    tabPanel("Filter data",
             tags$hr(),
             DT::dataTableOutput("filtermetadata")
             ),
    tabPanel("Filter taxa",
             p("Filter taxa is disabled at the moment."),
             tags$hr(),
             DT::dataTableOutput("filtertaxa")
             ),
    tabPanel("Info",
             h4("Filtering"),
             p("You can filter the data by searching a column or multiple columns in either the metadata or the taxonomy table and the resulting rows will be used for plotting. For example you can search for 'Aalborg West' in the Plant column and 'Tetrasphaera' in the Genus column to only show data from Aalborg West containing Tetrasphaera. The metadata must contain variables in columns and sample ID's in rows. If the dataset is large you can choose which columns to show and drag-and-drop columns to reorder them. The MiDAS example data is a large dataset with many samples, thus some filtering is neccessary or the plots will be slow.")
             )
  )
)