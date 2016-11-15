######### for UI #########

#sidebarpanel
choosedata_sbp <- sidebarPanel(
  position = "left",
  width = 3,
  h4("Choose data"),
  radioButtons(inputId = "chosendata",
               label = "",
               choices = c("MiDAS example data", "Upload your own"),
               selected = "MiDAS example data"
  ),
  conditionalPanel(
    condition = "input.chosendata == 'Upload your own'",
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
             DT::dataTableOutput("datatable")
             ),
    tabPanel("Info",
             h4("Filtering"),
             p("You can filter your data by searching a column or multiple columns in your metadata and the resulting rows will be used for plotting. Your metadata must contain variables in columns and sample ID's in rows. If your dataset is large you can choose which columns to show and drag-and-drop columns to reorder them. The MiDAS example data is a large dataset with many samples, some filtering is neccessary or the plots will be slow.")
             )
  )
)