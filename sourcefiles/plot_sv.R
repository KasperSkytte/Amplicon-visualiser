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
output$heatmap_UI_tax_group <- renderUI({
  selectizeInput(
    inputId = "heatmap_tax_group",
    label = "Choose grouping",
    choices = data_variables(),
    selected = data_variables_selected(),
    multiple = TRUE
    #,options = list(maxItems = 2)
  )
})

#subtract the chosen tax_aggregate input from tax_add input, because an error occurs if the same is chosen 
output$heatmap_UI_tax_add <- renderUI({
  taxvector <- c(Phylum = "Phylum", Class = "Class", Order = "Order", Family = "Family", Genus = "Genus", Species = "Species")
  
  checkboxGroupInput(
    inputId = "heatmap_tax_add",
    label = "Extra taxonomic information",
    choices = taxvector[!taxvector %in% c(input$heatmap_tax_aggregate)],
    selected = NULL
 )
})

plot_heatmap <- eventReactive(input$renderplot_heatmap, {
  #A group is a must
  if(is.null(input$heatmap_tax_group)) return(NULL)
  order_x <- if(input$heatmap_cluster_x) {"cluster"}
  order_y <- if(input$heatmap_cluster_y) {"cluster"}
  amp_heatmap(data = loaded_data_subset(),
              group_by = input$heatmap_tax_group, 
              tax_add = input$heatmap_tax_add,
              tax_aggregate = input$heatmap_tax_aggregate,
              order_x = order_x, 
              order_y = order_y,
              tax_show = input$heatmap_tax_show,
              plot_values = input$heatmap_plot_values,
              plot_colorscale = "log10",
              color_vector = unlist(strsplit(input$heatmap_colorvector, ","))
  )
})

output$heatmap <- renderPlot({
  plot_heatmap()
})

################## Boxplot ##################
output$boxplot_UI_group <- renderUI({
  selectInput(
    inputId = "boxplot_group",
    label = "Select group variable",
    choices = data_variables(),
    selected = data_variables_selected()
  )
})

#subtract the chosen tax_aggregate input from tax_add input, because an error occurs if the same is chosen 
output$boxplot_UI_tax_add <- renderUI({
  taxvector <- c(Phylum = "Phylum", Class = "Class", Order = "Order", Family = "Family", Genus = "Genus", Species = "Species")
  
  checkboxGroupInput(
    inputId = "boxplot_tax_add",
    label = "Extra taxonomic information",
    choices = taxvector[!taxvector %in% c(input$boxplot_tax_aggregate)],
    selected = NULL
  )
})

plot_boxplot <- eventReactive(input$renderplot_boxplot ,{
  #A group is a must
  if(is.null(input$boxplot_group)) return(NULL)
  
  if(input$boxplot_flip) {
    amp_boxplot(loaded_data_subset(),
               tax_aggregate = input$boxplot_tax_aggregate,
               tax_show = input$boxplot_tax_show,
               tax_add = input$boxplot_tax_add,
               plot_flip = input$boxplot_flip,
               group = input$boxplot_group
    ) + theme_light() +
      theme(axis.text.x = element_text(angle = 45, size=12, hjust = 1), 
            axis.text.y = element_text(size=10),
            axis.line = element_line(colour = "black", size = 0.5)
      ) 
  } else {
    amp_boxplot(loaded_data_subset(),
               tax_aggregate = input$boxplot_tax_aggregate,
               tax_show = input$boxplot_tax_show,
               tax_add = input$boxplot_tax_add,
               plot_flip = input$boxplot_flip,
               group = input$boxplot_group
    ) + theme_light() +
      theme(axis.text.x = element_text(size=12), 
            axis.text.y = element_text(size=10),
            axis.line = element_line(colour = "black", size = 0.5)
      ) 
  }
})

output$boxplot <- renderPlot({
  plot_boxplot()
})

################## Ordination ##################
output$ord_UI_group <- renderUI({
  selectInput(
    inputId = "ord_group",
    label = "Select group variable",
    choices = data_variables(),
    selected = data_variables_selected()
  )
})

output$ord_UI_constrain <- renderUI({
  selectInput(
    inputId = "ord_constrain",
    label = "Select constrain variable",
    choices = data_variables(),
    selected = data_variables_selected()
  )
})

plot_ord <- eventReactive(input$renderplot_ord, {
  #A group is a must
  if(is.null(input$ord_group)) return(NULL)
  
  amp_ordinate(loaded_data_subset(),
               type = input$ord_type,
               transform = input$ord_transform,
               distmeasure = input$ord_distmeasure,
               constrain = input$ord_constrain,
               sample_color_by = input$ord_group,
               sample_colorframe = TRUE,
               sample_colorframe_label = input$ord_group,
               species_nlabels = ifelse(any(input$ord_type == c("pcoa", "nmds", "ca", "cca", "dca")), 0, input$ord_nspecies)
               )
})
  
output$ord <- renderPlot({
  plot_ord()
})

################## Download feature ##################
output$saveplot <- downloadHandler(
  filename = function() {paste(input$plot_type, '.png', sep='') },
  content = function(file) {
    if(input$plot_type == "Heatmap" & !is.null(plot_heatmap())) {
    ggsave(file, plot = plot_heatmap(), device = "png")
    } else if (input$plot_type == "Ordination" & !is.null(plot_ord())) {
      ggsave(file, plot = plot_ord(), device = "png")
    } else if (input$plot_type == "Boxplot" & !is.null(plot_boxplot())) {
      ggsave(file, plot = plot_boxplot(), device = "png")
      } else return(NULL)
  }
)