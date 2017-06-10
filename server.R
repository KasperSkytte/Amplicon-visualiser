#max 50MB uploads if needed
#options(shiny.maxRequestSize=50*1024^2) 
######### Server #########
shinyServer(function(input, output, session) {
  source("sourcefiles/choosedata_sv.R", local = TRUE)
  source("sourcefiles/plot_sv.R", local = TRUE)
})
