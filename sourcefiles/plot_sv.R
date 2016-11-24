data_variables <- reactive({
  colnames(loaded_data_subset()[["metadata"]])
})

data_variables_selected <- reactive({
  if(input$chosendata == "MiDAS example data") {
    data_variables()[2]
  } else {
    data_variables()[1]
  }
})


################## Heatmap ##################
output$heatmap_UI_tax.group <- renderUI({
  selectizeInput(
    inputId = "heatmap_tax.group",
    label = "Choose grouping: ",
    choices = data_variables(),
    selected = data_variables_selected(),
    multiple = TRUE
    #,options = list(maxItems = 2)
  )
})

#subtract the chosen tax.aggregate input from tax.add input, because an error occurs if the same is chosen 
output$heatmap_UI_tax.add <- renderUI({
  taxvector <- c(Phylum = "Phylum", Class = "Class", Order = "Order", Family = "Family", Genus = "Genus", Species = "Species")
  
  checkboxGroupInput(
    inputId = "heatmap_tax.add",
    label = "Extra taxonomic information:",
    choices = taxvector[!taxvector %in% c(input$heatmap_tax.aggregate)],
    selected = NULL
 )
})

plot_heatmap <- eventReactive(input$renderplot_heatmap, {
  #A group is a must
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
  selectInput(
    inputId = "RA_group",
    label = "Select group variable: ",
    choices = data_variables(),
    selected = data_variables_selected()
  )
})


plot_RA <- eventReactive(input$renderplot_RA ,{
  #A group is a must
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
    inputId = "PCA_group",
    label = "Select group variable: ",
    choices = data_variables(),
    selected = data_variables_selected()
  )
})

output$PCA_UI_trajectory <- renderUI({
  selectInput(
    inputId = "PCA_trajectory",
    label = "Select trajectory variable: ",
    choices = data_variables(),
    selected = data_variables_selected()
  )
})

plot_PCA <- eventReactive(input$renderplot_PCA, {
  #A group is a must
  if(is.null(input$PCA_group)) return(NULL)
  
  #arguments 
  if(input$PCA_plot_nspecies != 0) {
    plot_species <- TRUE
    plot_nspecies <- input$PCA_plot_nspecies
  } else if(input$PCA_plot_nspecies == 0) {
    plot_species <- FALSE
    plot_nspecies <- NULL
  }
  
  if(input$PCA_constrain) {
    plot <- amp_ordinate(loaded_data_subset(),
                         output = "complete", 
                         envfit.significant = input$PCA_envfitslvl,
                         envfit.factor = input$PCA_group,
                         plot.color = input$PCA_group, 
                         plot.group = input$PCA_plot_group,
                         constrain = input$PCA_group,
                         #plot.group.label = input$PCA_group,
                         plot.species = plot_species,
                         plot.nspecies = plot_nspecies
                         #scale.species = TRUE
    ) 
  } else {
    plot <- amp_ordinate(loaded_data_subset(),
                         output = "complete", 
                         trajectory = input$PCA_trajectory,
                         plot.color = input$PCA_group, 
                         plot.group = input$PCA_plot_group,
                         #plot.group.label = input$PCA_group,
                         plot.species = plot_species,
                         plot.nspecies = plot_nspecies
                         #scale.species = TRUE
    ) 
  } 
  output$PCA_stats <- renderPrint({plot$eff.model})
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