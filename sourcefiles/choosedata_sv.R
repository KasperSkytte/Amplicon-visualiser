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
    #check the data to contain otutable+metadata
    if(!any(names(loadedObjects) == "metadata") | 
       !any(names(loadedObjects) == "otutable") | 
       !is.data.frame(loadedObjects[["metadata"]]) | 
       !is.data.frame(loadedObjects[["otutable"]])) {
      stop("The .RData file must contain two dataframes with the exact names: 'otutable' and 'metadata'")
    }
    loadedObjects <- amp_load(loadedObjects$otutable, loadedObjects$metadata, percent = TRUE)
  } else if(input$chosendata == "Upload data" & input$upl_type == "Two files (metadata and otutable)") {
    file_otutable <- input$upl_otutable
    file_metadata <- input$upl_metadata
    
    #HALT if the 2 files are not uploaded!
    if (is.null(file_otutable) | is.null(file_metadata))
      return(NULL)
    
    #load otutable
    otutable <- read.delim(file_otutable$datapath, sep = "\t", header = TRUE, check.names = FALSE, row.names = 1)
    
    #load metadata, check file extension
    ext <- tools::file_ext(file_metadata$name)
    if(ext == "xlsx" | ext == "xls") {
      file.rename(file_metadata$datapath,
                  paste(file_metadata$datapath, ext, sep="."))
      metadata <- as.data.frame(read_excel(paste(file_metadata$datapath, ext, sep=".")), na = "")
    } else if(ext == "csv" | ext == "txt") {
      metadata <- read.delim(file_metadata$datapath, sep = "\t", header = TRUE, check.names = TRUE)
    } else stop("Currently only supports metadata from Microsoft Excel files and .csv files")
    
    loadedObjects <- amp_load(otutable, metadata, percent = TRUE)
  } else return(NULL)
  
  #return the loaded data list
  return(loadedObjects)
})

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
                 autoWidth = TRUE,
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
                 autoWidth = TRUE,
                 scrollX = FALSE,
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
