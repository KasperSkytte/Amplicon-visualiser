data_variables <- reactive({
  colnames(loaded_data_subset()[["metadata"]])
})

################## Heatmap ##################
output$heatmap_UI_tax.group <- renderUI({
  selectizeInput(inputId = "heatmap_tax.group",
                 label = "Choose grouping: ",
                 choices = data_variables(),
                 selected = data_variables()[5],
                 multiple = TRUE
                 #,options = list(maxItems = 2)
  )
})

plot_heatmap <- eventReactive(input$goplot, {
  if(is.null(input$heatmap_tax.group)) return(NULL)
  amp_heatmap(data = loaded_data_subset(),
              group = input$heatmap_tax.group, 
              #                #tax.class = "p__Proteobacteria",
              tax.add = input$heatmap_tax.add,
              tax.aggregate = "Genus",
              #                #tax.empty = "OTU", #PISS SLOW
              #                min.abundance = 0.1,
              tax.show = input$heatmap_tax.show,
              plot.numbers = input$heatmap_plot.numbers,
              plot.colorscale = "log10"
) + theme_light() +
  theme(axis.text.x = element_text(angle = 45, size=12, hjust = 1), 
        axis.text.y=element_text(size=10)) 
})

output$heatmap <- renderPlot({
  plot_heatmap()
})

################## PCA ##################

output$PCA_UI_group <- renderUI({
  selectInput(
    inputId = "pca_group",
    label = "Select group variable: ",
    choices = data_variables(),
    selected = data_variables()[3]
  )
})

output$PCA_UI_shape <- renderUI({
  selectInput(
    inputId = "pca_shape",
    label = "Select point shapes: ",
    choices = data_variables(),
    selected = data_variables()[3]
  )
})

plot_PCA <- eventReactive(input$goplot, {
  if(is.null(input$pca_group) | is.null(input$pca_shape)) return(NULL)
  plot <- amp_ordinate(loaded_data_subset(),
               output = "complete", 
               envfit.significant = 0.05,
               envfit.factor = input$pca_group,
               plot.color = input$pca_group, 
               plot.group = input$pca_plot_group,
               plot.shape = input$pca_shape,
               constrain = input$pca_group,
               plot.group.label = input$pca_group,
               plot.species = TRUE,
               plot.nspecies = input$pca_plot_nspecies,
               scale.species = TRUE
  ) 
  #pca_effmodel <<- plot$eff.model
  output$pca_stats <- renderPrint({plot$eff.model})
  plot$plot + theme_light() 
})
  
output$PCA <- renderPlot({
  plot_PCA()
})

################## Rank Abundance ##################

output$RA_UI_group <- renderUI({
  selectInput(inputId = "RA_group",
              label = "Choose grouping: ",
              choices = data_variables(),
              selected = data_variables()[5]
  )
})


plot_RA <- eventReactive(input$goplot ,{
  if(is.null(input$RA_group)) return(NULL)
    amp_rabund(loaded_data_subset(),
               tax.aggregate = "Genus",
               tax.show = input$RA_tax.show,
               tax.add = input$RA_tax.add,
               plot.flip = input$RA_flip,
               group = input$RA_group
    ) + theme_light() +
      theme(axis.text.x = element_text(angle = 45, size=12, hjust = 1), 
            axis.text.y=element_text(size=10)) 
})

output$RA <- renderPlot({
  plot_RA()
})

################## Download feature ##################

output$saveplot <- downloadHandler(
  filename = function() {paste(input$plot_type, '.png', sep='') },
  content = function(file) {
    if(input$plot_type == "Heatmap") {
    ggsave(file, plot = plot_heatmap(), device = "png")
    } else if (input$plot_type == "PCA") {
      ggsave(file, plot = plot_PCA(), device = "png")
    } else if (input$plot_type == "RA") {
      ggsave(file, plot = plot_RA(), device = "png")
      }
  }
)