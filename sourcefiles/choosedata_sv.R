################## Load data ##################
loaded_data <- reactive({
  if (input$chosendata == "MiDAS example data") {
    objectNames <- load("MiDAS_1.20.RData")
    loadedObjects <- mget(objectNames)
    loadedObjects <- amp_load(loadedObjects$otutable, loadedObjects$metadata)
    } else if(input$chosendata == "Upload data" & input$upl_type == "One file (.RData)") {
    file <- input$upl_rdata
    
    #HALT if the file is not uploaded!
    if (is.null(file))
      return(NULL)
    
    objectNames <- load(file = file$datapath)
    loadedObjects <- mget(objectNames)
    if(!any(names(loadedObjects) == "metadata") | 
       !any(names(loadedObjects) == "otutable") | 
       !is.data.frame(loadedObjects[["metadata"]]) | 
       !is.data.frame(loadedObjects[["otutable"]])) {
      stop("The .RData file must contain two dataframes with the exact names: 'otutable' and 'metadata'")
    }
    loadedObjects <- amp_load(loadedObjects$otutable, loadedObjects$metadata)
  } else if(input$chosendata == "Upload data" & input$upl_type == "Two files (metadata and otutable)") {
    file_otutable <- input$upl_otutable
    file_metadata <- input$upl_metadata
    
    #HALT if the 2 files are not uploaded!
    if (is.null(file_otutable) | is.null(file_metadata))
      return(NULL)
    
    #load otutable
    otutable <- read.delim(file_otutable$datapath, sep = "\t", header = TRUE, check.names = FALSE, row.names = 1) #OTU table
    
    #load metadata
    ext <- tools::file_ext(file_metadata$name)
    if(ext == "xlsx" | ext == "xls") {
    file.rename(file_metadata$datapath,
                paste(file_metadata$datapath, ext, sep="."))
    metadata <- as.data.frame(read_excel(paste(file_metadata$datapath, ext, sep=".")))
    } else stop("Currently only supports metadata from Excel files")
    
    loadedObjects <- amp_load(otutable, metadata)
  } else return(NULL)
  
  #return the loaded data list
  return(loadedObjects)
})

################## Data table and subsetting ##################
output$datatable <- DT::renderDataTable(
  loaded_data()$metadata,
  server = TRUE,
  filter = "top",
  extensions = c("ColReorder", "Buttons"),
  rownames = FALSE,
  #selection = list(target = 'row+column'),
  selection = "none", 
  options = list(pageLength = 10,
                 dom = "Bfrtip",
                 buttons = I("colvis"),
                 autoWidth = TRUE,
                 scrollX = TRUE,
                 lengthMenu = c(5, 10, 25, 50, 100),
                 colReorder = TRUE,
                 columnDefs = list(list(width = '125px', targets = "_all"))
                 )
  )

loaded_data_subset <- reactive({
  d <- loaded_data()
  # Subset data based on input from the datatable
  newmetadata <- d$metadata[c(input$datatable_rows_all), , drop = FALSE]
  newabund <- d$abund[, rownames(newmetadata), drop=FALSE]
  #return a new list
  newlist <- list(abund = newabund, tax = d$tax, metadata = newmetadata)
  return(newlist)
})
