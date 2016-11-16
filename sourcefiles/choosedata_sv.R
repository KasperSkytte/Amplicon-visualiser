################## Load data ##################
loaded_data <- reactive({
  if (input$chosendata == "MiDAS example data") {
    objectNames <- load("MiDAS_1.20.RData")
    loadedObjects <- mget(objectNames)
    loadedObjects <- amp_load(loadedObjects$otutable, loadedObjects$metadata)
    } else if(input$chosendata == "Upload data" & input$upl_type == "One file (.RData)") {
    file <- input$upl_rdata
    
    #HALT if the 3 files are not uploaded!
    if (is.null(file))
      return(NULL)
    
    objectNames <- load(file = file$datapath)
    loadedObjects <- mget(objectNames)
    loadedObjects <- amp_load(loadedObjects$otutable, loadedObjects$metadata)
  } else if(input$chosendata == "Upload data" & input$upl_type == "Two files (metadata and otutable)") {
    file_otutable <- input$upl_otutable
    file_metadata <- input$upl_metadata
    
    #HALT if the 3 files are not uploaded!
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
  
  #Check if the sample ID's match eachother in otutable and metadata, else return an error
  if (all(rownames(loadedObjects$metadata) == colnames(loadedObjects$otutable)[1:nrow(loadedObjects$metadata)])) {
    return(loadedObjects)
  } else stop("The sample names in metadata do not match those in otutable")
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
  newotutable <- cbind(d$otutable[, rownames(newmetadata), drop=FALSE], d$otutable[, (ncol(d$otutable) - 6):ncol(d$otutable)])
  #return a new list
  newlist <- list(otutable = newotutable, metadata = newmetadata)
  return(newlist)
})
