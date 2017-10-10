#max 50MB uploads
options(shiny.maxRequestSize=50*1024^2) 
######### Server #########
shinyServer(function(input, output, session) {
  source("sourcefiles/choosedata_sv.R", local = TRUE)
  source("sourcefiles/plot_sv.R", local = TRUE)
  shinyjs::onclick("toggleDimHeatmap", shinyjs::toggle(id = "save_dimensions_heatmap", anim = FALSE))
  shinyjs::onclick("toggleDimBoxplot", shinyjs::toggle(id = "save_dimensions_boxplot", anim = FALSE))
  shinyjs::onclick("toggleDimOrdination", shinyjs::toggle(id = "save_dimensions_ordination", anim = FALSE))
  shinyjs::onclick("toggleHeatmapAdvanced", shinyjs::toggle(id = "heatmap_advanced", anim = TRUE)) 
  shinyjs::onclick("toggleBoxplotAdvanced", shinyjs::toggle(id = "boxplot_advanced", anim = TRUE)) 
  shinyjs::onclick("toggleOrdinationAdvanced", shinyjs::toggle(id = "ordination_advanced", anim = TRUE)) 
})