################## Load data ##################
loaded_data <- reactive({
  if (input$chosendata == "MiDAS example data") {
    load("MiDAS_1.21.RData")
    loadedObjects <- MiDAS_1.21
    } else if(input$chosendata == "Upload data") {
    file_otutable <- input$upl_otutable
    file_metadata <- input$upl_metadata
    
    #stop if no otutable uploaded!
    if (is.null(file_otutable))
      return(NULL)
    
    #load otutable
    otutable <- data.table::fread(file_otutable$datapath,
                                  header = TRUE, 
                                  check.names = FALSE,
                                  fill = TRUE)
    
    #load metadata
    #if no metadata is uploaded, create dummy metadata
    if(is.null(file_metadata)) {
      metadata <- data.frame(SampleID = colnames(otutable)[1:(ncol(otutable)-7)], Dummycolumn = "No metadata provided")
      rownames(metadata) <- metadata$SampleID
      warning("No metadata provided, dummy metadata created.")
    } else if(!is.null(file_metadata)) {
      #check file extension
      ext <- tools::file_ext(file_metadata$name)
      if(ext %in% c("xlsx", "xls")) {
        file.rename(file_metadata$datapath,
                    paste(file_metadata$datapath, ext, sep="."))
        metadata <- as.data.frame(read_excel(paste(file_metadata$datapath, ext, sep=".")), na = "")
      } else if (ext %in% c("txt", "csv")) {
        metadata <- data.table::fread(file_metadata$datapath, 
                                      header = TRUE,
                                      check.names = FALSE,  
                                      fill = TRUE)
      } else
        stop("Only supports metadata from Microsoft Excel files or flat delimited text files")
    }
    metadata <- mutate_all(metadata, as.factor) #all columns to factors so it is possible to select more than one when searching in columns with DT
    loadedObjects <- amp_load(otutable, metadata)
  } else 
    return(NULL)
  
  #return the loaded data list
  return(loadedObjects)
})

## Minimal example download handlers
output$dlexamplemetadata <- downloadHandler(
  filename = "metadata.xlsx",
  content <- function(file) {
    file.copy("minimal example data/metadata.xlsx", file)
  }
)
output$dlexampleotutable <- downloadHandler(
  filename = "otutable.csv",
  content <- function(file) {
    file.copy("minimal example data/otutable.csv", file)
  }
)


################## Data tables and subsetting ##################
#metadata table
output$filtermetadata <- DT::renderDataTable(
  loaded_data()$metadata,
  server = TRUE,
  filter = "top",
  extensions = c("ColReorder", "Buttons"),
  rownames = FALSE,
  selection = "none", 
  options = list(pageLength = 10,
                 dom = "Bfrtip",
                 buttons = I("colvis"),
                 autoWidth = FALSE,
                 scrollX = TRUE,
                 lengthMenu = c(5, 10, 25, 50, 100),
                 colReorder = TRUE,
                 columnDefs = list(list(width = '125px', targets = "_all"))
                 )
  )

#taxonomy table
output$filtertaxa <- DT::renderDataTable(
  #rename $tax with amp_rename to remove prefixes, fx g__, 
  #and transform to factors so the user can select multiple taxa from a dropdown menu in each filter column 
  as.data.frame(lapply(amp_rename(loaded_data())[["tax"]], as.factor)),
  server = TRUE,
  filter = "top",
  rownames = FALSE,
  selection = "none", 
  options = list(pageLength = 10,
                 autoWidth = FALSE,
                 scrollX = TRUE,
                 lengthMenu = c(5, 10, 25, 50, 100),
                 columnDefs = list(list(width = '125px', targets = "_all"))
  )
)

#subset the data based on user input from the 2 above data tables
loaded_data_subset <- reactive({
  d <- loaded_data()
  # Subset data based on input from the datatable, inverse if selected
  if(input$filterdata_inv) {
    newmetadata <- d$metadata[-c(input$filtermetadata_rows_all), , drop = FALSE]
  } else if(!input$filterdata_inv) {
    newmetadata <- d$metadata[c(input$filtermetadata_rows_all), , drop = FALSE]
  }
  
  #subset taxa based on input from the datatable, inverse if selected
  if(input$filtertaxa_inv & !is.null(c(input$filtertaxa_rows_all))) {
    newabund <- d$abund[-c(input$filtertaxa_rows_all), rownames(newmetadata), drop=FALSE]
    newtax <- d$tax[-c(input$filtertaxa_rows_all), , drop = FALSE]
  } else if(!input$filtertaxa_inv & !is.null(c(input$filtertaxa_rows_all))) {
    newabund <- d$abund[c(input$filtertaxa_rows_all), rownames(newmetadata), drop=FALSE]
    newtax <- d$tax[c(input$filtertaxa_rows_all), , drop = FALSE]
  }
  
  #return a new list, the user has to click the 'filter taxa' pane to actually render the tax table and not return NULL, therefore the if():
  if(is.null(c(input$filtertaxa_rows_all))) {
    newabund <- d$abund[, rownames(newmetadata), drop=FALSE]
    
    newlist <- list(abund = newabund, tax = d$tax, metadata = newmetadata)
  } else if(!is.null(c(input$filtertaxa_rows_all))) {
    newlist <- list(abund = newabund, tax = newtax, metadata = newmetadata)
  }
  return(newlist)
})
