#max 512MB uploads
options(shiny.maxRequestSize=512*1024^2) 
######### Server #########
shinyServer(function(input, output, session) {
  source("sourcefiles/choosedata_sv.R", local = TRUE)
  source("sourcefiles/plot_sv.R", local = TRUE)
})
