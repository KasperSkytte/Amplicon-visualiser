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
output$heatmap_UI_group_by <- renderUI({
  selectizeInput(
    inputId = "heatmap_group_by",
    label = "Group by",
    choices = data_variables(),
    selected = data_variables_selected(),
    multiple = TRUE
    #,options = list(maxItems = 2)
  )
})

output$heatmap_UI_facet_by <- renderUI({
  selectizeInput(
    inputId = "heatmap_facet_by",
    label = "Facet by",
    choices = c("none", data_variables()),
    selected = "none",
    multiple = FALSE
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
  if(is.null(input$heatmap_group_by)) return(NULL)
  order_x <- if(any(input$heatmap_cluster == "Cluster x-axis")) {"cluster"}
  order_y <- if(any(input$heatmap_cluster == "Cluster y-axis")) {"cluster"}
  facet_by <- if(!input$heatmap_facet_by == "none") {input$heatmap_facet_by}
  amp_heatmap(data = loaded_data_subset(),
              group_by = input$heatmap_group_by, 
              facet_by = facet_by,
              tax_add = input$heatmap_tax_add,
              tax_aggregate = input$heatmap_tax_aggregate,
              order_x_by = order_x, 
              order_y_by = order_y,
              tax_show = input$heatmap_tax_show,
              plot_values = input$heatmap_plot_values,
              plot_colorscale = "log10",
              color_vector = unlist(strsplit(input$heatmap_colorvector, ","))
  )
})

output$heatmap <- renderPlot({
  plot_heatmap()
})

################## Alpha-diversity ##################
output$alphadiv_options <- renderUI({
  #find char and factor columns, remove sampleID column
  cols2keep <- unlist(
    lapply(loaded_data_subset()[["metadata"]][-1], 
           function(x) {
             is.character(x) | is.factor(x)
           }), 
    use.names = FALSE)
  choices <- c("All samples",
               "Individual samples (scatter plot)",
               colnames(loaded_data_subset()[["metadata"]])[c(FALSE, cols2keep)])
  selectInput(
    inputId = "alphadiv_group_by",
    label = "Group by",
    choices = choices,
    selected = "Individual samples"
  )
})

output$alphadiv_rarefy <- renderUI({
  reads <- colSums(loaded_data_subset()[["abund"]])
  numericInput(
    inputId = "alphadiv_rarefy",
    label = "Rarefy samples to even depth",
    value = max(reads),
    min = 1L,
    max = max(reads),
    step = 1L
  )
})

calcAlphadiv <- reactive({
  shiny::req(input$alphadiv_rarefy)
  res <- amp_alphadiv(
    loaded_data_subset(), 
    rarefy = input$alphadiv_rarefy,
    richness = TRUE)
  return(res)
})

output$alphadiv_plot <- renderPlot({
  shiny::req(input$alphadiv_measure)
  calcAlphadiv() %>%
    tidyr::gather(key = "index", 
                  value = "value",
                  Reads, 
                  ObservedOTUs,
                  Shannon, 
                  Simpson,
                  invSimpson,
                  Chao1,
                  ACE) %>% 
    dplyr::filter(index %in% input$alphadiv_measure) %>% {
      .$index <- factor(.$index, levels = c("Shannon", "Simpson", "invSimpson", "Chao1", "ACE", "ObservedOTUs", "Reads"))
      return(.)
    } %>% 
    mutate("All samples" = "All samples") %>% 
    ggplot(aes_q(x = switch((input$alphadiv_group_by == "Individual samples (scatter plot)") + 1L,
                            sym(input$alphadiv_group_by),
                            sym(names(.)[1])),
                 y = .$value,
                 group = switch((input$alphadiv_group_by == "Individual samples (scatter plot)") + 1L, 
                                sym(input$alphadiv_group_by),
                                NULL))) + {
                                  if(input$alphadiv_group_by != "Individual samples (scatter plot)")
                                    geom_boxplot()
                                  else if(input$alphadiv_group_by == "Individual samples (scatter plot)")
                                    geom_point()
                                } +
    facet_wrap(~index, nrow = 1, scales = "free_y") +
    theme_minimal() +
    theme(strip.background = element_rect(colour = NA, fill = "grey95"), 
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = 10),
          axis.text.x = element_text(angle = 90),
          axis.title = element_blank())
})

################## Boxplot ##################
output$boxplot_UI_group_by <- renderUI({
  selectInput(
    inputId = "boxplot_group_by",
    label = "Group by",
    choices = data_variables()
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
  if(input$boxplot_flip) {
    plot <- amp_boxplot(loaded_data_subset(),
               tax_aggregate = input$boxplot_tax_aggregate,
               tax_show = input$boxplot_tax_show,
               tax_add = input$boxplot_tax_add,
               plot_flip = input$boxplot_flip,
               group_by = input$boxplot_group_by
               )
    plot <- plot + 
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, size=12, hjust = 1), 
            axis.text.y = element_text(size=10),
            axis.line = element_line(colour = "black", size = 0.5)
            ) 
  } else {
    plot <- amp_boxplot(loaded_data_subset(),
               tax_aggregate = input$boxplot_tax_aggregate,
               tax_show = input$boxplot_tax_show,
               tax_add = input$boxplot_tax_add,
               plot_flip = input$boxplot_flip,
               group_by = input$boxplot_group_by
               )
    plot <- plot +
      theme_light() +
      theme(axis.text.x = element_text(size=12), 
            axis.text.y = element_text(size=10),
            axis.line = element_line(colour = "black", size = 0.5)
            ) 
  }
  return(plot)
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
  
  plotly <- amp_ordinate(loaded_data_subset(),
               type = input$ord_type,
               transform = input$ord_transform,
               distmeasure = input$ord_distmeasure,
               constrain = input$ord_constrain,
               sample_color_by = input$ord_group,
               sample_colorframe = TRUE,
               sample_colorframe_label = input$ord_group,
               sample_plotly = ifelse(input$ord_plotlytype == "Samples", TRUE, FALSE),
               species_plotly = ifelse(input$ord_plotlytype == "Species", TRUE, FALSE),
               species_plot = ifelse(input$ord_plotlytype == "Species", TRUE, FALSE),
               species_point_size = 2
               )
  plotly$elementId <- NULL
  ggplot <- amp_ordinate(loaded_data_subset(),
                       type = input$ord_type,
                       transform = input$ord_transform,
                       distmeasure = input$ord_distmeasure,
                       constrain = input$ord_constrain,
                       sample_color_by = input$ord_group,
                       sample_colorframe = TRUE,
                       sample_colorframe_label = input$ord_group
  )
  ggplot <- ggplot + theme(legend.position = "none")
  return(list(plotly = plotly, ggplot = ggplot))
})
  
output$ord <- renderPlotly({
  plot_ord()[["plotly"]]
})

################## Download feature ##################
#heatmap
output$saveHeatmap <- downloadHandler(
  filename = function() {paste(input$currentTab, '.pdf', sep='')},
  content = function(file) {
    ggsave(file, plot = plot_heatmap(), device = "pdf", width = input$save_width_heatmap, height = input$save_height_heatmap)
  }
)

#boxplot
output$saveBoxplot <- downloadHandler(
  filename = function() {paste(input$currentTab, '.pdf', sep='')},
  content = function(file) {
    ggsave(file, plot = plot_boxplot(), device = "pdf", width = input$save_width_boxplot, height = input$save_height_boxplot)
  }
)

#ordination
output$saveOrdination <- downloadHandler(
  filename = function() {paste(input$currentTab, '.pdf', sep='')},
  content = function(file) {
    ggsave(file, plot = plot_ord()$ggplot, device = "pdf", width = input$save_width_ordination, height = input$save_height_ordination)
  }
)