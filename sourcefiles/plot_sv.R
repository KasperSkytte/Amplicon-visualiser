data_variables <- reactive({
  colnames(loaded_data_subset()[["metadata"]])
})

################## Heatmap ##################
output$heatmap_UI_tax.group <- renderUI({
  selectizeInput(inputId = "heatmap_tax.group",
                 label = "Choose grouping: ",
                 choices = data_variables(),
                 selected = data_variables()[1],
                 multiple = TRUE
                 #,options = list(maxItems = 2)
  )
})

plot_heatmap <- eventReactive(input$goplot, {
  if(is.null(input$heatmap_tax.group)) return(NULL)
  order_x <- if(input$heatmap_cluster_x) {"cluster"}
  order_y <- if(input$heatmap_cluster_y) {"cluster"}
  amp_heatmap(data = loaded_data_subset(),
              group = input$heatmap_tax.group, 
              #tax.class = "p__Proteobacteria",
              tax.add = input$heatmap_tax.add,
              tax.aggregate = input$heatmap_tax.aggregate,
              order.x = order_x, 
              order.y = order_y,
              #tax.empty = "OTU", #PISS SLOW
              #min.abundance = 0.1,
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


################## Rank Abundance ##################
output$RA_UI_group <- renderUI({
  selectInput(inputId = "RA_group",
              label = "Select group variable: ",
              choices = data_variables()
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

################## PCA ##################
output$PCA_UI_group <- renderUI({
  selectInput(
    inputId = "pca_group",
    label = "Select group variable: ",
    choices = data_variables(),
    selected = data_variables()[1]
  )
})

plot_PCA <- eventReactive(input$goplot, {
  #A group is a must
  if(is.null(input$pca_group)) return(NULL)
  
  #argument plot.nspecies
  if(input$pca_plot_nspecies != 0) {
    plot_species <- TRUE
    plot_nspecies <- input$pca_plot_nspecies
  } else if(input$pca_plot_nspecies == 0) {
    plot_species <- FALSE
    plot_nspecies <- NULL
  }

  plot <- amp_ordinate(loaded_data_subset(),
               output = "complete", 
               #envfit.significant = 0.05,
               #envfit.factor = input$pca_group,
               plot.color = input$pca_group, 
               plot.group = input$pca_plot_group,
               #constrain = input$pca_group,
               #plot.group.label = input$pca_group,
               plot.species = plot_species,
               plot.nspecies = plot_nspecies
               #scale.species = TRUE
  ) 
  
  #output$pca_stats <- renderPrint({plot$eff.model})
  plot$plot + theme_light() 
})
  
output$PCA <- renderPlot({
  plot_PCA()
})

################## Download feature ##################
output$saveplot <- downloadHandler(
  filename = function() {paste(input$plot_type, '.png', sep='') },
  content = function(file) {
    if(input$plot_type == "Heatmap" & !is.null(plot_heatmap())) {
    ggsave(file, plot = plot_heatmap(), device = "png")
    } else if (input$plot_type == "Principal Component Analysis (PCA)" & !is.null(plot_PCA())) {
      ggsave(file, plot = plot_PCA(), device = "png")
    } else if (input$plot_type == "Rank Abundance" & !is.null(plot_RA())) {
      ggsave(file, plot = plot_RA(), device = "png")
      } else return(NULL)
  }
)