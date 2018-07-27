##### amp_load #####
amp_load <- function (otutable, metadata, fasta = NULL) {
  otutable <- as.data.frame(otutable)
  metadata <- as.data.frame(metadata)
  rownames(metadata) <- as.character(metadata[, 1])
  if (any(tolower(colnames(otutable)) == "otu")) {
    otucolid <- which(tolower(colnames(otutable)) == "otu")
    rownames(otutable) <- as.character(otutable[, otucolid])
    otutable <- otutable[, -otucolid]
  }
  else if (all(rownames(otutable) %in% c(1:nrow(otutable))) & 
           !any(tolower(colnames(otutable)) == "otu")) {
    stop("Cannot find OTU ID's. Make sure they are provided as rownames or in a column called \"OTU\".")
  }
  colnames(metadata) <- stringr::str_replace_all(colnames(metadata), 
                                                 "[^[:alnum:]]", "_")
  tax.names <- colnames(otutable[, (ncol(otutable) - 6):ncol(otutable)])
  expected.tax <- c("Kingdom", "Phylum", "Class", "Order", 
                    "Family", "Genus", "Species")
  if (!all(tax.names %in% expected.tax)) {
    stop(paste("The last 7 columns in the OTU-table must be the taxonomy (Kingdom->Species) and named accordingly\nCurrent:", 
               paste(tax.names, collapse = ", "), "\nExpected:", 
               paste(expected.tax, collapse = ", ")))
  }
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)
  otutable$Kingdom <- trim(as.character(otutable$Kingdom))
  otutable$Phylum <- trim(as.character(otutable$Phylum))
  otutable$Class <- trim(as.character(otutable$Class))
  otutable$Order <- trim(as.character(otutable$Order))
  otutable$Family <- trim(as.character(otutable$Family))
  otutable$Genus <- trim(as.character(otutable$Genus))
  otutable$Species <- trim(as.character(otutable$Species))
  abund <- lapply(otutable[, 1:(ncol(otutable) - 7), drop = FALSE], 
                  as.numeric) %>% as.data.frame(check.names = FALSE, row.names = rownames(otutable))
  abund0 <- abund
  metadata0 <- metadata
  if (!all(rownames(metadata) %in% colnames(abund)) | !all(colnames(abund) %in% 
                                                           rownames(metadata))) {
    if (!any(colnames(abund) %in% rownames(metadata))) {
      stop("No sample names match between metadata and otutable. Check that you have loaded matching files and that they meet the requirements described in ?amp_load(). Remember to use check.names = FALSE when loading the files.")
    }
    else {
      sharedSamples <- dplyr::intersect(colnames(abund), 
                                        rownames(metadata))
      abund0 <- abund[, match(sharedSamples, colnames(abund)), 
                      drop = FALSE]
      abund0 <- abund0[rowSums(abund0) > 0, ]
      metadata0 <- metadata[match(sharedSamples, rownames(metadata)), 
                            , drop = FALSE]
      metadataUniques <- metadata[-which(rownames(metadata) %in% 
                                           rownames(metadata0)), , drop = FALSE] %>% rownames()
      abundUniques <- abund[, -which(colnames(abund) %in% 
                                       colnames(abund0)), drop = FALSE] %>% colnames()
      warning("Only ", ncol(abund0), " of ", length(unique(c(rownames(metadata), 
                                                             colnames(abund)))), " unique sample names match between metadata and otutable. The following unmatched samples have been removed:", 
              ifelse(length(metadataUniques) > 0, paste0("\nmetadata (", 
                                                         length(metadataUniques), "): \n\t\"", paste(metadataUniques, 
                                                                                                     collapse = "\", \""), "\""), ""), ifelse(length(abundUniques) > 
                                                                                                                                                0, paste0("\notutable (", length(abundUniques), 
                                                                                                                                                          "): \n\t\"", paste(abundUniques, collapse = "\", \""), 
                                                                                                                                                          "\""), ""))
    }
  }
  tax <- data.frame(otutable[, (ncol(otutable) - 6):ncol(otutable)], 
                    OTU = rownames(otutable))
  tax <- tax[which(rownames(abund) %in% rownames(abund0)), 
             c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", 
               "Species", "OTU")]
  tax[is.na(tax)] <- ""
  if (!is.null(fasta)) {
    data <- list(abund = abund0, tax = tax, metadata = metadata0, 
                 refseq = ape::read.dna(file = fasta, format = "fasta"))
  }
  else {
    data <- list(abund = abund0, tax = tax, metadata = metadata0)
  }
  class(data) <- "ampvis2"
  return(data)
}

##### amp_rename #####
amp_rename <- function(data, tax_class = NULL, tax_empty = "best", tax_level = "Genus") {
  
  tax = data[["tax"]]
  
  ## First make sure that all entries are strings
  for ( i in 1:ncol(tax) ){
    tax[,i] <- as.character(tax[,i])  
  }
  
  ## Change a specific phylum to class level
  if(!is.null(tax_class)){
    for (i in 1:nrow(tax)){
      if (!is.na(tax$Phylum[i]) & tax$Phylum[i] %in% tax_class){
        tax$Phylum[i] <- tax$Class[i]   
      }
    }
  }
  
  ## Remove the underscore classifier from the data  
  tax$Kingdom <- gsub("^k_*", "", tax$Kingdom)
  tax$Phylum <- gsub("^p_*", "", tax$Phylum)
  tax$Phylum <- gsub("^c_*", "", tax$Phylum)
  tax$Class <- gsub("^c_*", "", tax$Class)
  tax$Order <- gsub("^o_*", "", tax$Order)
  tax$Family <- gsub("^f_*", "", tax$Family)
  tax$Genus <- gsub("^g_*", "", tax$Genus)
  tax$Kingdom <- gsub("uncultured", "", tax$Kingdom)
  tax$Phylum <- gsub("uncultured", "", tax$Phylum)
  tax$Class <- gsub("uncultured", "", tax$Class)
  tax$Order <- gsub("uncultured", "", tax$Order)
  tax$Family <- gsub("uncultured", "", tax$Family)
  tax$Genus <- gsub("uncultured", "", tax$Genus)
  
  ## Check if there is a species level otherwise add it for consistency
  if (!is.null(tax$Species)){
    tax$Species <- gsub("^s_*", "", tax$Species)
  } else {
    tax$Species <- ""
  }
  
  tax[is.na(tax)] <- ""
  
  ## How to handle empty taxonomic assignments
  if (tax_empty == "OTU"){
    for (i in 1:nrow(tax)) {
      if (tax[i,"Species"] == "") {tax[i,"Species"] <- rownames(tax)[i]}
      if (tax[i,"Genus"] == "") {tax[i,"Genus"] <- rownames(tax)[i]}
      if (tax[i,"Family"] == "") {tax[i,"Family"] <- rownames(tax)[i]}
      if (tax[i,"Order"] == "") {tax[i,"Order"] <- rownames(tax)[i]}
      if (tax[i,"Class"] == "") {tax[i,"Class"] <- rownames(tax)[i]}
      if (tax[i,"Phylum"] == "") {tax[i,"Phylum"] <- rownames(tax)[i]}
    }
  }
  
  ## Handle empty taxonomic strings
  rn <- rownames(tax) #damn rownames are silently dropped by mutate()
  if(tax_empty == "best"){
    tax <- mutate(tax, Kingdom, Kingdom = ifelse(Kingdom == "", "Unclassified", Kingdom)) %>%
      mutate(Phylum, Phylum = ifelse(Phylum == "", paste("k__", Kingdom, "_", rownames(tax), sep = ""), Phylum)) %>%
      mutate(Class, Class = ifelse(Class == "", ifelse(grepl("__", Phylum), Phylum, paste("p__", Phylum, "_", rownames(tax), sep = "")), Class)) %>%
      mutate(Order, Order = ifelse(Order == "", ifelse(grepl("__", Class), Class, paste("c__", Class, "_", rownames(tax), sep = "")), Order)) %>%
      mutate(Family, Family = ifelse(Family == "", ifelse(grepl("__", Order), Order, paste("o__", Order, "_", rownames(tax), sep = "")), Family)) %>%
      mutate(Genus, Genus = ifelse(Genus == "", ifelse(grepl("__", Family), Family, paste("f__", Family, "_", rownames(tax), sep = "")), Genus)) %>%
      mutate(Species, Species = ifelse(Species == "", ifelse(grepl("__", Genus), Genus, paste("g__", Genus, "_", rownames(tax), sep = "")), Species))
  }
  rownames(tax) <- rn
  
  if(tax_empty == "remove"){
    abund <- data[["abund"]]
    tax <- subset(tax, tax[,tax_level] != "")
    abund <- subset(abund, rownames(abund) %in% rownames(tax))
    data[["abund"]] <- abund
  }
  data[["tax"]] <- tax
  rownames(data[["tax"]]) <- rownames(tax)
  
  return(data)
}


##### amp_heatmap #####
amp_heatmap <- function (data, group_by = NULL, facet_by = NULL, normalise_by = NULL, 
                         scale_by = NULL, tax_aggregate = "Phylum", tax_add = NULL, 
                         tax_show = 10, tax_class = NULL, tax_empty = "best", order_x_by = NULL, 
                         order_y_by = NULL, plot_values = TRUE, plot_legendbreaks = NULL, 
                         plot_colorscale = "log10", plot_na = TRUE, plot_values_size = 4, 
                         plot_theme = "normal", measure = "mean", min_abundance = 0.1, 
                         max_abundance = NULL, sort_by = NULL, color_vector = NULL, 
                         round = 1, raw = FALSE, textmap = FALSE) {
  
  data <- amp_rename(data = data, tax_class = tax_class, tax_empty = tax_empty, 
                     tax_level = tax_aggregate)
  if (!is.null(tax_aggregate) & !is.null(tax_add)) {
    if (tax_aggregate == tax_add) {
      stop("tax_aggregate and tax_add cannot be the same")
    }
  }
  abund <- data[["abund"]]
  tax <- data[["tax"]]
  metadata <- data[["metadata"]]
  if (!is.null(group_by)) {
    metadata[group_by] <- lapply(metadata[group_by], factor)
  }
  if (!is.null(facet_by)) {
    if (is.null(group_by)) {
      group_by <- facet_by
    }
    metadata[facet_by] <- lapply(metadata[facet_by], factor)
  }
  if (!is.null(scale_by)) {
    variable <- as.numeric(metadata[, scale_by])
    abund <- t(t(abund) * variable)
  }
  if (raw == FALSE) {
    abund <- as.data.frame(sapply(abund, function(x) x/sum(x) * 
                                    100))
  }
  suppressWarnings(if (!is.null(tax_add)) {
    if (tax_add != tax_aggregate) {
      tax <- data.frame(tax, Display = apply(tax[, c(tax_add, 
                                                     tax_aggregate)], 1, paste, collapse = "; "))
    }
  }
  else {
    tax <- data.frame(tax, Display = tax[, tax_aggregate])
  })
  abund3 <- cbind.data.frame(Display = tax[, "Display"], abund) %>% 
    tidyr::gather(key = Sample, value = Abundance, -Display) %>% 
    as.data.table()
  abund3 <- abund3[, `:=`("sum", sum(Abundance)), by = list(Display, 
                                                            Sample)] %>% setkey(Display, Sample) %>% as.data.frame()
  if (!is.null(facet_by)) {
    ogroup <- group_by
    group_by <- c(group_by, facet_by)
  }
  suppressWarnings(if (!is.null(group_by)) {
    if (length(group_by) > 1) {
      grp <- data.frame(Sample = metadata[, 1], Group = apply(metadata[, 
                                                                       group_by], 1, paste, collapse = " "))
      oldGroup <- unique(cbind.data.frame(metadata[, group_by], 
                                          Group = grp$Group))
    }
    else {
      grp <- data.frame(Sample = metadata[, 1], Group = metadata[, 
                                                                 group_by])
    }
    abund3$Group <- grp$Group[match(abund3$Sample, grp$Sample)]
    abund5 <- abund3
  }
  else {
    abund5 <- data.frame(abund3, Group = abund3$Sample)
  })
  if (measure == "mean") {
    abund6 <- data.table(abund5)[, `:=`(Abundance, mean(sum)), 
                                 by = list(Display, Group)] %>% setkey(Display, Group) %>% 
      unique() %>% as.data.frame()
  }
  if (measure == "max") {
    abund6 <- data.table(abund5)[, `:=`(Abundance, max(sum)), 
                                 by = list(Display, Group)] %>% setkey(Display, Group) %>% 
      unique() %>% as.data.frame()
  }
  if (measure == "median") {
    abund6 <- data.table(abund5)[, `:=`(Abundance, median(sum)), 
                                 by = list(Display, Group)] %>% setkey(Display, Group) %>% 
      unique() %>% as.data.frame()
  }
  if (measure == "mean") {
    TotalCounts <- group_by(abund6, Display) %>% summarise(Abundance = sum(Abundance)) %>% 
      arrange(desc(Abundance))
  }
  if (measure == "max") {
    TotalCounts <- group_by(abund6, Display) %>% summarise(Abundance = max(Abundance)) %>% 
      arrange(desc(Abundance))
  }
  if (measure == "median") {
    TotalCounts <- group_by(abund6, Display) %>% summarise(Abundance = median(Abundance)) %>% 
      arrange(desc(Abundance))
  }
  if (!is.null(sort_by)) {
    TotalCounts <- filter(abund6, Group == sort_by) %>% 
      arrange(desc(Abundance))
  }
  if (is.numeric(tax_show)) {
    if (tax_show > nrow(TotalCounts)) {
      tax_show <- nrow(TotalCounts)
    }
    abund7 <- filter(abund6, Display %in% TotalCounts$Display[1:tax_show])
  }
  if (!is.numeric(tax_show)) {
    if (tax_show != "all") {
      abund7 <- filter(abund6, Display %in% tax_show)
    }
    if (tax_show == "all") {
      tax_show <- nrow(TotalCounts)
      abund7 <- filter(abund6, Display %in% TotalCounts$Display[1:tax_show])
    }
  }
  abund7 <- as.data.frame(abund7)
  if (!is.null(normalise_by)) {
    temp <- tidyr::spread(abund7, key = Group, value = Abundance)
    temp1 <- cbind.data.frame(Display = temp$Display, temp[, 
                                                           -1]/temp[, normalise_by])
    abund7 <- tidyr::gather(temp1, key = Group, value = Abundance, 
                            -Display)
  }
  if (is.null(order_y_by)) {
    abund7$Display <- factor(abund7$Display, levels = rev(TotalCounts$Display))
  }
  if (!is.null(order_y_by)) {
    if ((length(order_y_by) == 1) && (order_y_by != "cluster")) {
      temp1 <- filter(abund7, Group == order_y_by) %>% 
        group_by(Display) %>% summarise(Mean = mean(Abundance)) %>% 
        arrange(desc(Mean))
      abund7$Display <- factor(abund7$Display, levels = rev(temp1$Display))
    }
    if (length(order_y_by) > 1) {
      abund7$Display <- factor(abund7$Display, levels = order_y_by)
    }
    if ((length(order_y_by) == 1) && (order_y_by == "cluster")) {
      if (is.null(max_abundance)) {
        max_abundance <- max(abund7$Abundance)
      }
      tdata <- mutate(abund7, Abundance = ifelse(Abundance < 
                                                   min_abundance, min_abundance, Abundance), Abundance = ifelse(Abundance > 
                                                                                                                  max_abundance, max_abundance, Abundance))
      tdata <- dcast(tdata, Display ~ Group, value.var = "Abundance")
      rownames(tdata) <- tdata$Display
      tdata2 <- tdata[, -1]
      tclust <- hclust(dist(tdata2))
      tnames <- levels(droplevels(tdata$Display))[tclust$order]
      abund7$Display <- factor(abund7$Display, levels = tnames)
    }
  }
  if (!is.null(order_x_by)) {
    if ((length(order_x_by) == 1) && (order_x_by != "cluster")) {
      temp1 <- filter(abund7, Display == order_x_by) %>% 
        group_by(Group) %>% summarise(Mean = mean(Abundance)) %>% 
        arrange(desc(Mean))
      abund7$Group <- factor(abund7$Group, levels = as.character(temp1$Group))
    }
    if (length(order_x_by) > 1) {
      abund7$Group <- factor(abund7$Group, levels = order_x_by)
    }
    if ((length(order_x_by) == 1) && (order_x_by == "cluster")) {
      if (is.null(max_abundance)) {
        max_abundance <- max(abund7$Abundance)
      }
      tdata <- mutate(abund7, Abundance = ifelse(Abundance < 
                                                   min_abundance, min_abundance, Abundance), Abundance = ifelse(Abundance > 
                                                                                                                  max_abundance, max_abundance, Abundance))
      tdata <- dcast(tdata, Display ~ Group, value.var = "Abundance")
      rownames(tdata) <- tdata$Display
      tdata2 <- tdata[, -1]
      tclust <- hclust(dist(t(tdata2)))
      tnames <- tclust$labels[tclust$order]
      abund7$Group <- factor(abund7$Group, levels = tnames)
    }
  }
  if (plot_na == FALSE) {
    plot_na <- "grey50"
  }
  else {
    if (!is.null(color_vector)) {
      plot_na <- color_vector[1]
    }
    else {
      plot_na <- "#67A9CF"
    }
  }
  if (length(group_by) > 1) {
    abund7 <- merge(abund7, oldGroup)
  }
  if (is.null(min_abundance)) {
    min_abundance <- ifelse(min(abund7$Abundance) > 0.001, 
                            min(abund7$Abundance), 0.001)
  }
  if (is.null(max_abundance)) {
    max_abundance <- max(abund7$Abundance)
  }
  if (!textmap) {
    p <- ggplot(abund7, aes_string(x = "Group", y = "Display", 
                                   label = formatC("Abundance", format = "f", digits = 1))) + 
      geom_tile(aes(fill = Abundance), colour = "white", 
                size = 0.5) + theme(axis.text.y = element_text(size = 12, 
                                                               color = "black", vjust = 0.4), axis.text.x = element_text(size = 10, 
                                                                                                                         color = "black", vjust = 0.5, angle = 90, hjust = 1), 
                                    axis.title = element_blank(), text = element_text(size = 8, 
                                                                                      color = "black"), plot.margin = unit(c(1, 1, 
                                                                                                                             1, 1), "mm"), title = element_text(size = 8), 
                                    panel.background = element_blank())
    if (!is.null(color_vector)) {
      color.pal = color_vector
    }
    else {
      color.pal = rev(brewer.pal(3, "RdBu"))
    }
    if (plot_values == TRUE) {
      abund8 <- abund7
      abund8$Abundance <- round(abund8$Abundance, round)
      p <- p + geom_text(data = abund8, size = plot_values_size, 
                         colour = "grey10", check_overlap = TRUE) + theme(legend.position = "none")
    }
    if (is.null(plot_legendbreaks)) {
      p <- p + scale_fill_gradientn(colours = color.pal, 
                                    trans = plot_colorscale, na.value = plot_na, 
                                    oob = squish, limits = c(min_abundance, max_abundance))
    }
    if (!is.null(plot_legendbreaks)) {
      p <- p + scale_fill_gradientn(colours = color.pal, 
                                    trans = plot_colorscale, breaks = plot_legendbreaks, 
                                    na.value = plot_na, oob = squish, limits = c(min_abundance, 
                                                                                 max_abundance))
    }
    if (is.null(normalise_by)) {
      p <- p + labs(x = "", y = "", fill = "% Read\nAbundance")
    }
    if (!is.null(normalise_by)) {
      p <- p + labs(x = "", y = "", fill = "Relative")
    }
    if (!is.null(facet_by)) {
      if (length(ogroup) > 1) {
        p$data$Group <- apply(p$data[, ogroup], 1, paste, 
                              collapse = " ")
      }
      else {
        p$data$Group <- p$data[, ogroup]
      }
      if (plot_values == TRUE) {
        if (length(ogroup) > 1) {
          p$layers[[2]]$data$Group <- apply(p$layers[[2]]$data[, 
                                                               ogroup], 1, paste, collapse = " ")
        }
        else {
          p$layers[[2]]$data$Group <- p$layers[[2]]$data[, 
                                                         ogroup]
        }
      }
      p <- p + facet_grid(reformulate(facet_by),
                          scales = "free_x", 
                          space = "free")
      p <- p + theme(strip.text = element_text(size = 12, face = "bold"))
    }
    return(p)
  }
  else if (textmap) {
    textmap <- abund7[, c("Display", "Abundance", "Group")] %>% 
      group_by(Group) %>% filter(!duplicated(Abundance, 
                                             Group)) %>% spread(Group, Abundance) %>% arrange(desc(droplevels(Display)))
    textmap <- data.frame(textmap[, -1], row.names = textmap$Display, 
                          check.names = FALSE)
    return(textmap)
  }
}

##### amp_ordinate #####
amp_ordinate <- function(data,
                         filter_species = 0.1, 
                         type = "PCA",
                         distmeasure = "none",
                         transform = "hellinger",
                         constrain = NULL,
                         x_axis = 1,
                         y_axis = 2, 
                         sample_color_by = NULL,
                         sample_color_order = NULL, 
                         sample_shape_by = NULL,
                         sample_colorframe = FALSE,
                         sample_colorframe_label = NULL, 
                         sample_label_by = NULL,
                         sample_label_size = 4,
                         sample_label_segment_color = "black",
                         sample_point_size = 2,
                         sample_trajectory = NULL,
                         sample_trajectory_group = sample_trajectory,
                         sample_plotly = FALSE,
                         species_plot = FALSE, 
                         species_nlabels = 0, 
                         species_label_taxonomy = "Genus",
                         species_label_size = 3, 
                         species_label_color = "grey10", 
                         species_rescale = FALSE, 
                         species_point_size = 2,
                         species_shape = 20, 
                         species_plotly = FALSE,
                         envfit_factor = NULL,
                         envfit_numeric = NULL,
                         envfit_signif_level = 0.001, 
                         envfit_textsize = 3,
                         envfit_color = "darkred", 
                         envfit_numeric_arrows_scale = 1, 
                         envfit_show = TRUE, 
                         repel_labels = TRUE, 
                         opacity = 0.8, 
                         tax_empty = "best", 
                         detailed_output = FALSE, 
                         ...) {
  if(species_plotly == T | sample_plotly == T){
    repel_labels <- F
  }
  
  #Impossible to do ordination with 1 or 2 samples
  if(length(unique(data$metadata[,1])) <= 2)
    stop("Ordination cannot be performed on 2 or fewer samples (the number of resulting axes will always be n-1, where n is the number of samples).")
  
  #Check the data
  data <- amp_rename(data = data, tax_empty = tax_empty)
  
  #First transform to percentages
  abund_pct <- as.data.frame(sapply(data$abund, function(x) x/sum(x) * 100))
  rownames(abund_pct) <- rownames(data$abund) #keep rownames
  
  #Then filter low abundant OTU's where ALL samples have below the threshold set with filter_species in percent
  abund_subset <- abund_pct[!apply(abund_pct, 1, function(row) all(row <= filter_species)),,drop = FALSE] #remove low abundant OTU's 
  data$abund <- data$abund[which(rownames(data$abund) %in% rownames(abund_subset)),,drop = FALSE]
  rownames(data$tax) <- data$tax$OTU
  data$tax <- data$tax[which(rownames(data$tax) %in% rownames(abund_subset)),,drop = FALSE] #same with taxonomy
  
  #to fix user argument characters, so fx PCoA/PCOA/pcoa are all valid
  type <- tolower(type)
  
  #data transformation with decostand()
  if(!transform == "none" & transform != "sqrt") {
    transform <- tolower(transform)
    data$abund <- t(vegan::decostand(t(data$abund), method = transform))
  } else if (transform == "sqrt") {
    data$abund <- t(sqrt(t(data$abund)))
  } 
  
  #Generate inputmatrix AFTER transformation
  if (any(type == c("nmds", "mmds", "pcoa", "dca"))) {
    if (!distmeasure == "none") {
      #Calculate distance matrix with vegdist()
      distmeasure <- tolower(distmeasure)
      if(distmeasure == "jsd") {
        #This is based on http://enterotype.embl.de/enterotypes.html
        #Abundances of 0 will be set to the pseudocount value to avoid 0-value denominators
        #Unfortunately this code is SLOOOOOOOOW
        dist.JSD <- function(inMatrix, pseudocount=0.000001) {
          KLD <- function(x,y) sum(x *log(x/y))
          JSD <- function(x,y) sqrt(0.5 * KLD(x, (x+y)/2) + 0.5 * KLD(y, (x+y)/2))
          matrixColSize <- length(colnames(inMatrix))
          matrixRowSize <- length(rownames(inMatrix))
          colnames <- colnames(inMatrix)
          resultsMatrix <- matrix(0, matrixColSize, matrixColSize)
          
          inMatrix = apply(inMatrix,1:2,function(x) ifelse (x==0,pseudocount,x))
          
          for(i in 1:matrixColSize) {
            for(j in 1:matrixColSize) { 
              resultsMatrix[i,j]=JSD(as.vector(inMatrix[,i]),
                                     as.vector(inMatrix[,j]))
            }
          }
          colnames -> colnames(resultsMatrix) -> rownames(resultsMatrix)
          as.dist(resultsMatrix)->resultsMatrix
          attr(resultsMatrix, "method") <- "dist"
          return(resultsMatrix) 
        }
        cat("Calculating the Jensen-Shannon Divergence (JSD) distance matrix may take a long time.")
        inputmatrix <- dist.JSD(data$abund)
      } else if(any(distmeasure == c("manhattan", "euclidean", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao", "mahalanobis"))) {
        inputmatrix <- vegan::vegdist(t(data$abund), method = distmeasure)
      }
    } else if (distmeasure == "none") {
      warning("No distance measure selected, using raw data. If this is not deliberate, please provide one with the argument: distmeasure.")
      inputmatrix <- t(data$abund)
    }
    
    if (transform != "none" & distmeasure != "none") {
      warning("Using both transformation AND a distance measure is not recommended for distance-based ordination (nMDS/PCoA/DCA). If this is not deliberate, consider transform = \"none\".")
    }
  } else if(any(type == c("pca", "rda", "ca", "cca"))) {
    inputmatrix <- t(data$abund)
  }
  
  #################################### end of block ####################################
  
  #Generate data depending on the chosen ordination type
  if(type == "pca") {
    #make the model
    model <- vegan::rda(inputmatrix, ...)
    
    #axis (and data column) names
    x_axis_name <- paste0("PC", x_axis)
    y_axis_name <- paste0("PC", y_axis)
    
    #Calculate the amount of inertia explained by each axis
    totalvar <- round(model$CA$eig/model$tot.chi * 100, 1)
    
    #Calculate species- and site scores
    sitescores <- vegan::scores(model, display = "sites", choices = c(x_axis, y_axis))
    speciesscores <- vegan::scores(model, display = "species", choices = c(x_axis, y_axis))
    
  } else if(type == "rda") {
    if(is.null(constrain)) 
      stop("Argument constrain must be provided when performing constrained/canonical analysis.")
    #make the model
    codestring <- paste0("rda(inputmatrix~", paste(constrain, collapse = "+"), ", data$metadata, ...)") #function arguments written in the format "rda(x ~ y + z)" cannot be directly passed to rda(), now user just provides a vector
    model <-  eval(parse(text = codestring))
    
    #axes depend on the results
    x_axis_name <- paste0("RDA", x_axis)
    if (model$CCA$rank <= 1){
      y_axis_name <- "PC1"
      
      #Calculate the amount of inertia explained by each axis
      totalvar <- c(round(model$CCA$eig/model$tot.chi * 100, 1), #constrained of total space
                    round(model$CA$eig/model$tot.chi * 100, 1)  #UNconstrained of total space
      )
      constrainedvar <- c(round(model$CA$eig/model$CA$tot.chi * 100, 1)) #UNconstrained of total UNconstrained space
    } else if (model$CCA$rank > 1) {
      y_axis_name <- paste0("RDA", y_axis)
      
      #Calculate the amount of inertia explained by each axis
      totalvar <- c(round(model$CCA$eig/model$tot.chi * 100, 1), #constrained of total space
                    round(model$CA$eig/model$tot.chi * 100, 1)  #UNconstrained of total space
      )
      constrainedvar <- c(round(model$CCA$eig/model$CCA$tot.chi * 100, 1)) #constrained of total constrained space
    }
    
    #Calculate species- and site scores
    sitescores <- vegan::scores(model, display = "sites", choices = c(x_axis, y_axis))
    speciesscores <- vegan::scores(model, display = "species", choices = c(x_axis, y_axis))
  } else if(type == "nmds") {
    #make the model
    model <- vegan::metaMDS(inputmatrix, trace = FALSE, ...)
    
    #axis (and data column) names
    x_axis_name <- paste0("NMDS", x_axis)
    y_axis_name <- paste0("NMDS", y_axis)
    
    #Calculate species- and site scores
    #Speciesscores may not be available with MDS
    sitescores <- vegan::scores(model, display = "sites")
    if(!length(model$species) > 1) {
      speciesscores <- warning("Speciesscores are not available.")
    } else {
      speciesscores <- vegan::scores(model, display = "species", choices = c(x_axis, y_axis))
    }
  } else if(type == "mmds" | type == "pcoa") {
    #make the model
    model <- ape::pcoa(inputmatrix, ...)
    
    #axis (and data column) names
    x_axis_name <- paste0("PCo", x_axis)
    y_axis_name <- paste0("PCo", y_axis)
    
    #Calculate the percentage of eigenvalues explained by the axes
    totalvar <- round(model$values$Relative_eig * 100, 1)
    names(totalvar) <- c(paste0("PCo", seq(1:length(totalvar))))
    
    #Calculate species- and site scores
    #Speciesscores are not available with pcoa
    sitescores <- as.data.frame(model$vectors)
    colnames(sitescores) <- c(paste0("PCo", seq(1:length(sitescores))))
    speciesscores <- warning("Speciesscores are not available.")
  } else if(type == "ca") {
    #make the model
    model <- vegan::cca(inputmatrix, ...)
    
    #axis (and data column) names
    x_axis_name <- paste0("CA", x_axis)
    y_axis_name <- paste0("CA", y_axis)
    
    #Calculate the percentage of eigenvalues explained by the axes
    totalvar <- round(model$CA$eig/model$tot.chi * 100, 1)
    
    #Calculate species- and site scores
    sitescores <- vegan::scores(model, display = "sites", choices = c(x_axis, y_axis))
    speciesscores <- vegan::scores(model, display = "species", choices = c(x_axis, y_axis))
  } else if(type == "cca") {
    if(is.null(constrain)) 
      stop("Argument constrain must be provided when performing constrained/canonical analysis.")
    #make the model
    codestring <- paste0("cca(inputmatrix~", paste(constrain, collapse = "+"), ", data$metadata, ...)") #function arguments written in the format "rda(x ~ y + z)" cannot be directly passed to rda(), now user just provides a vector
    model <-  eval(parse(text = codestring))
    
    #axes depend on the results
    x_axis_name <- paste0("CCA", x_axis)
    if (model$CCA$rank <= 1){
      y_axis_name <- "CA1"
      
      #Calculate the amount of inertia explained by each axis
      totalvar <- c(round(model$CCA$eig/model$tot.chi * 100, 1), #constrained of total space
                    round(model$CA$eig/model$tot.chi * 100, 1)  #UNconstrained of total space
      )
      constrainedvar <- c(round(model$CA$eig/model$CA$tot.chi * 100, 1)) #UNconstrained of total UNconstrained space
    } else if (model$CCA$rank > 1) {
      y_axis_name <- paste0("CCA", y_axis)
      
      #Calculate the amount of inertia explained by each axis
      totalvar <- c(round(model$CCA$eig/model$tot.chi * 100, 1), #constrained of total space
                    round(model$CA$eig/model$tot.chi * 100, 1)  #UNconstrained of total space
      )
      constrainedvar <- c(round(model$CCA$eig/model$CCA$tot.chi * 100, 1)) #constrained of total constrained space
    }
    
    #Calculate species- and site scores
    sitescores <- vegan::scores(model, display = "sites", choices = c(x_axis, y_axis))
    speciesscores <- vegan::scores(model, display = "species", choices = c(x_axis, y_axis))
  } else if(type == "dca") {
    #make the model
    model <- vegan::decorana(inputmatrix, ...)
    
    #axis (and data column) names
    x_axis_name <- paste0("DCA", x_axis)
    y_axis_name <- paste0("DCA", y_axis)
    
    #Calculate the percentage of eigenvalues explained by the axes
    #totalvar <- round(model$CA$eig/model$CA$tot.chi * 100, 1)
    
    #Calculate species- and site scores
    sitescores <- vegan::scores(model, display = "sites", choices = c(x_axis, y_axis))
    speciesscores <- vegan::scores(model, display = "species", choices = c(x_axis, y_axis))
  }
  #################################### end of block ####################################
  
  #Make data frames for ggplot
  dsites <- cbind.data.frame(data$metadata, sitescores)
  
  if (!is.null(sample_color_order)) {
    dsites[, sample_color_by] <- factor(dsites[, sample_color_by], levels = sample_color_order)
  }
  
  if(length(speciesscores) > 1) {
    dspecies <- merge(data.frame(speciesscores, OTU = rownames(speciesscores)), data$tax, by.x = "OTU")
    dspecies$dist <- dspecies[, x_axis_name]^2 + dspecies[, y_axis_name]^2
    dspecies <- dplyr::arrange(dspecies, desc(dist))
    rownames(dspecies) <- dspecies$OTU
    if (species_rescale == TRUE) {
      maxx <- max(abs(dsites[, x_axis_name]))/max(abs(dspecies[,x_axis_name]))
      dspecies[, x_axis_name] <- dspecies[, x_axis_name] * maxx * 0.8
      maxy <- max(abs(dsites[, y_axis_name]))/max(abs(dspecies[,y_axis_name]))
      dspecies[, y_axis_name] <- dspecies[, y_axis_name] * maxy * 0.8
    }
  } else {
    dspecies = NULL
  }
  
  #Generate a nice ggplot with the coordinates from scores
  plot <- ggplot(dsites,
                 aes_string(x = x_axis_name,
                            y = y_axis_name,
                            color = sample_color_by,
                            shape = sample_shape_by))
  
  #Generate a color frame around the chosen color group
  
  if(sample_colorframe == TRUE) {
    if(is.null(sample_color_by)) stop("Please provide the argument sample_color_by.")
    splitData <- split(dsites, dsites[, sample_color_by]) %>% 
      lapply(function(df) {
        df[chull(df[, x_axis_name], df[, y_axis_name]), ]
      })
    hulls <- do.call(rbind, splitData)
    plot <- plot + geom_polygon(data = hulls, aes_string(fill = sample_color_by, group = sample_color_by), alpha = 0.2*opacity)
  }
  
  # Add points and plotly functionality for samples
  if (!sample_plotly == FALSE){
    if(length(sample_plotly) > 1){
      data_plotly <- apply(data$metadata[,sample_plotly], 1, paste, collapse = "<br>")  
    } else if(sample_plotly == "all" | sample_plotly == TRUE){
      data_plotly <- apply(data$metadata[,], 1, paste, collapse = "<br>")  
    } else{
      data_plotly <- paste0(sample_plotly,": ",data$metadata[,sample_plotly])
    }
    plot <- plot +
      suppressWarnings(geom_point(size = 2, alpha = opacity,
                                  aes(text = data_plotly))) + #HER
      theme_minimal() +
      theme(axis.line = element_line(colour = "black", size = 0.5))
  } else{
    plot <- plot +
      geom_point(size = sample_point_size, alpha = opacity) +
      theme_minimal() +
      theme(axis.line = element_line(colour = "black", size = 0.5))
  }
  
  #Only eigenvalue-based ordination methods can be displayed with % on axes
  if(type == "pca" | type == "ca" | type == "pcoa" | type == "mmds") {
    plot <- plot +
      xlab(paste(x_axis_name, " [", totalvar[x_axis_name], "%]", sep = "")) + 
      ylab(paste(y_axis_name, " [", totalvar[y_axis_name], "%]", sep = ""))
  } else if(type == "rda" | type == "cca") {
    if(model$CCA$rank > 1) {
      plot <- plot + 
        xlab(paste(x_axis_name, " [", totalvar[x_axis_name], "% / ", constrainedvar[x_axis_name], "%]", sep = "")) +
        ylab(paste(y_axis_name, " [", totalvar[y_axis_name], "% / ", constrainedvar[y_axis_name], "%]", sep = ""))
    } else if(model$CCA$rank <= 1) {
      plot <- plot + 
        xlab(paste(x_axis_name, " [", totalvar[x_axis_name], "%]", sep = "")) +
        ylab(paste(y_axis_name, " [", totalvar[y_axis_name], "% / ", constrainedvar[y_axis_name], "%]", sep = ""))
    }
  } else if (type == "nmds") {
    plot <- plot +
      annotate("text", size = 3, x = Inf, y = Inf, hjust = 1, vjust = 1, label = paste0("Stress value = ", round(model$stress, 3)))
  }
  
  #Plot species points
  if (species_plot == TRUE) {
    if(species_plotly == T){
      data_plotly <- paste("Kingdom: ", data$tax[,1],"<br>",
                           "Phylum: ", data$tax[,2],"<br>",
                           "Class: ", data$tax[,3],"<br>",
                           "Order: ", data$tax[,4],"<br>",
                           "Family: ", data$tax[,5],"<br>",
                           "Genus: ", data$tax[,6],"<br>",
                           "Species: ", data$tax[,7],"<br>",
                           "OTU: ", data$tax[,8],sep = "")
      plot <- plot + 
        geom_point(data = dspecies,
                   color = "darkgrey",
                   shape = species_shape,
                   size = species_point_size-1,
                   alpha = opacity,
                   aes(text = data_plotly))
    } else{
      plot <- plot + 
        geom_point(data = dspecies,
                   color = "darkgrey",
                   shape = species_shape,
                   size = species_point_size,
                   alpha = opacity) 
    }
    
  }
  
  #Plot text labels
  if (!is.null(sample_colorframe_label)) {
    temp <- data.frame(group = dsites[, sample_colorframe_label], 
                       x = dsites[, x_axis_name],
                       y = dsites[, y_axis_name]) %>% 
      group_by(group) %>%
      summarise(cx = mean(x), cy = mean(y)) %>% 
      as.data.frame()
    temp2 <- merge(dsites, temp,
                   by.x = sample_colorframe_label, 
                   by.y = "group")
    temp3 <- temp2[!duplicated(temp2[, sample_colorframe_label]), ]
    if (repel_labels == T){plot <- plot + ggrepel::geom_text_repel(data = temp3, aes_string(x = "cx", y = "cy", label = sample_colorframe_label), size = 3,color = "black",fontface = 2)}
    else{plot <- plot +geom_text(data = temp3, aes_string(x = "cx", y = "cy", label = sample_colorframe_label), size = 3,color = "black",fontface = 2)}
  }
  
  #sample_trajectory
  if (!is.null(sample_trajectory)) {
    traj <- dsites[order(dsites[, sample_trajectory]), ]
    plot <- plot + geom_path(data = traj, aes_string(group = sample_trajectory_group))
  }
  
  #Sample point labels
  if(!is.null(sample_label_by)) {
    
    if (repel_labels == T){plot <- plot + ggrepel::geom_text_repel(aes_string(label = sample_label_by),size = sample_label_size, color = "grey40", segment.color = sample_label_segment_color)}
    else{plot <- plot + geom_text(aes_string(label = sample_label_by),size = sample_label_size, color = "grey40", segment.color = sample_label_segment_color)}
  }
  
  #Plot species labels
  if (species_nlabels > 0) {
    if (repel_labels == T){plot <- plot + ggrepel::geom_text_repel(data = dspecies[1:species_nlabels,], aes_string(x = x_axis_name, y = y_axis_name, label = species_label_taxonomy),colour = species_label_color, size = species_label_size,fontface = 4,inherit.aes = FALSE)}
    else{plot <- plot +geom_text(data = dspecies[1:species_nlabels,], aes_string(x = x_axis_name, y = y_axis_name, label = species_label_taxonomy),colour = species_label_color, size = species_label_size,fontface = 4,inherit.aes = FALSE)}
  }
  
  ######## Fit environmental variables ########
  # Categorical fitting
  if(!is.null(envfit_factor)) {
    evf_factor_model <- envfit(model,
                               data$metadata[,envfit_factor, drop = FALSE],
                               permutations = 999,
                               choices = c(x_axis_name, y_axis_name)
    )
    evf_factor_data <- data.frame(Name = rownames(evf_factor_model$factors$centroids),
                                  Variable = evf_factor_model$factors$var.id,
                                  evf_factor_model$factors$centroids,
                                  pval = evf_factor_model$factors$pvals
    ) %>% subset(pval <= envfit_signif_level)
    if (nrow(evf_factor_data) > 0 & envfit_show == TRUE) {
      if (repel_labels == T){plot <- plot + ggrepel::geom_text_repel(data = evf_factor_data,aes_string(x = x_axis_name, y = y_axis_name, label = "Name"), colour = envfit_color, inherit.aes = FALSE, size = envfit_textsize, fontface = "bold")}
      else{plot <- plot + geom_text(data = evf_factor_data,aes_string(x = x_axis_name, y = y_axis_name, label = "Name"), colour = envfit_color, inherit.aes = FALSE, size = envfit_textsize, fontface = "bold")}
    }
    if (nrow(evf_factor_data) == 0) {
      warning("No environmental variables fit below the chosen significant level.")
    }
  } else {
    evf_factor_model <- NULL
  }
  
  # Numerical fitting
  if (!is.null(envfit_numeric)) {
    evf_numeric_model <- envfit(model,
                                data$metadata[,envfit_numeric, drop = FALSE],
                                permutations = 999,
                                choices = c(x_axis_name, y_axis_name)
    )
    evf_numeric_data <- data.frame(Name = rownames(evf_numeric_model$vectors$arrows),
                                   evf_numeric_model$vectors$arrows * sqrt(evf_numeric_model$vectors$r) * envfit_numeric_arrows_scale,
                                   pval = evf_numeric_model$vectors$pvals
    ) %>% subset(pval <= envfit_signif_level)
    if (nrow(evf_numeric_data) > 0 & envfit_show == TRUE) {
      plot <- plot + geom_segment(data = evf_numeric_data,
                                  aes_string(x = 0,
                                             xend = x_axis_name,
                                             y = 0,
                                             yend = y_axis_name
                                  ),
                                  arrow = arrow(length = unit(3, "mm")),
                                  colour = "darkred",
                                  size = 1,
                                  inherit.aes = FALSE) + 
        geom_text(data = evf_numeric_data,
                  aes_string(x = x_axis_name,
                             y = y_axis_name,
                             label = "Name"),
                  colour = envfit_color,
                  inherit.aes = FALSE,
                  size = envfit_textsize,
                  hjust = 1.2,
                  vjust = 1.2,
                  fontface = "bold"
        )
    } 
    if (nrow(evf_numeric_data) == 0) {
      warning("No environmental variables fit below the chosen significant level.")
    }
  } else {
    evf_numeric_model <- NULL
  }
  
  #################################### end of block ####################################
  
  #return plot or additional details
  if(!sample_plotly == FALSE){
    plotly::ggplotly(plot, tooltip = "text") %>% 
      layout(showlegend = FALSE)
  } 
  else if(species_plotly == T){
    plotly::ggplotly(plot, tooltip = "text") %>% 
      layout(showlegend = FALSE)
  }
  else if(!detailed_output){
    return(plot)
  }
  else if(detailed_output){
    if (type == "nmds") {
      screeplot <- NULL
    } else {
      ### screeplot ###
      #the data for it
      if (type == "mmds" | type == "pcoa") {
        if (length(model$values$Relative_eig) > 10) {
          unconstrained_eig <- model$values$Relative_eig[1:10]*100
        } else {
          unconstrained_eig <- model$values$Relative_eig*100
        }
        #the scree plot
        screeplot <- ggplot(data.frame(axis = factor(as.character(c(1:length(unconstrained_eig))), levels = c(1:length(unconstrained_eig))), eigenvalues = unconstrained_eig), aes(x = axis, y = eigenvalues)) +
          geom_col() +
          #geom_text(label = round(eigenvalues, 2), vjust = -1, size = 3)  + #Can't get it to work
          theme_minimal() +
          xlab("Axis (max. 10 axes will be shown)") +
          ylab("Eigenvalue in percent of total inertia")
      } else {
        unconstrained_eig <- model$CA$eig/model$tot.chi*100
        constrained_eig <- model$CCA$eig/model$tot.chi*100
        if (length(constrained_eig) > 10) {
          constrained_eig <- constrained_eig[1:10]
        }
        if (length(unconstrained_eig) > 10) {
          unconstrained_eig <- unconstrained_eig[1:10]
        }
        eigenvalues <- c(constrained_eig, unconstrained_eig) #constrained combined with unconstrained
        #the scree plot
        screeplot <- ggplot(data.frame(axis = factor(names(eigenvalues), levels = names(eigenvalues)), eigenvalues = eigenvalues), aes(x = axis, y = eigenvalues)) +
          geom_col() +
          geom_text(label = round(eigenvalues, 2), vjust = -1, size = 3)  +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          xlab("Axis (max. 10 axes will be shown)") +
          ylab("Eigenvalue in percent of total inertia")
      }
    }
    
    return(list(plot = plot,
                screeplot = screeplot,
                model = model,
                dsites = dsites,
                dspecies = dspecies,
                evf_factor_model = evf_factor_model,
                evf_numeric_model = evf_numeric_model)
    )
  }
}

##### amp_boxplot #####
amp_boxplot <- function (data, group_by = "Sample", sort_by = "median", plot_type = "boxplot", 
                         point_size = 1, tax_aggregate = "Genus", tax_add = NULL, 
                         tax_show = 20, tax_empty = "best", tax_class = NULL, order_group = NULL, 
                         order_y = NULL, plot_flip = FALSE, plot_log = FALSE, adjust_zero = NULL, 
                         raw = FALSE, detailed_output = FALSE) {
  data <- amp_rename(data = data, tax_class = tax_class, tax_empty = tax_empty, 
                     tax_level = tax_aggregate)
  if (!is.null(tax_aggregate) & !is.null(tax_add)) {
    if (tax_aggregate == tax_add) {
      stop("tax_aggregate and tax_add cannot be the same")
    }
  }
  abund <- data[["abund"]]
  tax <- data[["tax"]]
  metadata <- data[["metadata"]]
  if (raw == FALSE) {
    abund <- as.data.frame(sapply(abund, function(x) x/sum(x) * 
                                    100))
  }
  suppressWarnings(if (!is.null(tax_add)) {
    if (tax_add != tax_aggregate) {
      tax <- data.frame(tax, Display = apply(tax[, c(tax_add, 
                                                     tax_aggregate)], 1, paste, collapse = "; "))
    }
  }
  else {
    tax <- data.frame(tax, Display = tax[, tax_aggregate])
  })
  abund3 <- cbind.data.frame(Display = tax[, "Display"], abund) %>% 
    tidyr::gather(key = Sample, value = Abundance, -Display) %>% 
    as.data.table()
  abund3 <- abund3[, `:=`("Abundance", sum(Abundance)), by = list(Display, 
                                                                  Sample)] %>% setkey(Display, Sample) %>% unique()
  suppressWarnings(if (group_by != "Sample") {
    if (length(group_by) > 1) {
      grp <- data.frame(Sample = rownames(metadata), Group = apply(metadata[, 
                                                                            group_by], 1, paste, collapse = " "))
    }
    else {
      grp <- data.frame(Sample = rownames(metadata), Group = metadata[, 
                                                                      group_by])
    }
    abund3$Group <- grp$Group[match(abund3$Sample, grp$Sample)]
    abund5 <- abund3
  }
  else {
    abund5 <- data.frame(abund3, Group = abund3$Sample)
  })
  TotalCounts <- group_by(abund5, Display) %>% summarise(Median = median(Abundance), 
                                                         Total = sum(Abundance), Mean = mean(Abundance))
  if (sort_by == "median") {
    TotalCounts %<>% arrange(desc(Median)) %>% as.data.frame()
  }
  if (sort_by == "mean") {
    TotalCounts %<>% arrange(desc(Mean)) %>% as.data.frame()
  }
  if (sort_by == "total") {
    TotalCounts %<>% arrange(desc(Total)) %>% as.data.frame()
  }
  abund5$Display <- factor(abund5$Display, levels = rev(TotalCounts$Display))
  if (is.numeric(tax_show)) {
    if (tax_show > nrow(TotalCounts)) {
      tax_show <- nrow(TotalCounts)
    }
    abund7 <- subset(abund5, abund5$Display %in% TotalCounts[1:tax_show, 
                                                             "Display"])
  }
  if (!is.numeric(tax_show)) {
    if (length(tax_show) > 1) {
      abund7 <- subset(abund5, as.character(abund5$Display) %in% 
                         tax_show)
    }
    if ((length(tax_show) == 1) && (tax_show != "all")) {
      abund7 <- subset(abund5, as.character(abund5$Display) %in% 
                         tax_show)
    }
    if ((length(tax_show) == 1) && (tax_show == "all")) {
      tax_show <- nrow(TotalCounts)
      abund7 <- subset(abund5, abund5$Display %in% TotalCounts[1:tax_show, 
                                                               "Display"])
    }
  }
  if (!is.null(adjust_zero)) {
    abund7$Abundance[abund7$Abundance == 0] <- adjust_zero
  }
  if (length(order_y) > 1) {
    abund7$Display <- factor(abund7$Display, levels = order_y)
    abund7 <- subset(abund7, !is.na(Display))
  }
  if (group_by == "Sample") {
    p <- ggplot(abund7, aes(x = Display, y = Abundance))
  }
  if (group_by != "Sample") {
    if (!is.null(order_group)) {
      abund7$Group <- factor(abund7$Group, levels = rev(order_group))
    }
    p <- ggplot(abund7, aes(x = Display, y = Abundance, 
                            color = Group))
  }
  p <- p + ylab("Read Abundance (%)") + guides(col = guide_legend(reverse = TRUE)) + 
    xlab("") + theme_classic() + theme(panel.grid.major.x = element_line(color = "grey90"), 
                                       panel.grid.major.y = element_line(color = "grey90"))
  if (plot_flip == F) {
    p <- p + coord_flip()
  }
  else {
    p <- p + theme(axis.text.x = element_text(angle = 90, 
                                              hjust = 1, vjust = 0.5))
  }
  if (plot_type == "point") {
    p <- p + geom_point(size = point_size)
  }
  if (plot_type == "boxplot") {
    p <- p + geom_boxplot(outlier.size = point_size)
  }
  if (plot_log == TRUE) {
    p <- p + scale_y_log10()
  }
  if (detailed_output) {
    return(list(plot = p, data = abund7))
  }
  else return(p)
}
