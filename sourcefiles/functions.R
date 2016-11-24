amp_subset <- function(list, ...) {
  #Check the data first
  if(!is.list(list) | 
     !any(names(list) == "abund") | 
     !any(names(list) == "tax") | 
     !any(names(list) == "metadata") | 
     !is.data.frame(list[["abund"]]) |
     !is.data.frame(list[["tax"]]) |
     !is.data.frame(list[["metadata"]])
     ) {
    stop("The data must be a list with three dataframes named abund, tax and metadata")
  }
  
  #extract data from the list
  metadata <- list$metadata
  abund <- list$abund
  tax <- list$tax
  
  #subset metadata based on ... and only keep columns in otutable matching the rows in the subsetted metadata
  newmetadata <- subset(metadata, ...)
  newabund <- abund[, rownames(newmetadata), drop=FALSE]
  
  #return a new list
  newlist <- list(abund = newabund, tax = tax, metadata = newmetadata)
  return(newlist)
}

amp_subset_taxa <- function(list, ...) {
  #Check the data first
  if(!is.list(list) | 
     !any(names(list) == "abund") | 
     !any(names(list) == "tax") | 
     !any(names(list) == "metadata") | 
     !is.data.frame(list[["abund"]]) |
     !is.data.frame(list[["tax"]]) |
     !is.data.frame(list[["metadata"]])
  ) {
    stop("The data must be a list with three dataframes named abund, tax and metadata")
  }
  
  #extract data from the list
  metadata <- list$metadata
  abund <- list$abund
  tax <- list$tax
  
  #subset tax table based on ... and only keep rows in abund and metadata matching the rows in the subsetted tax table
  newtax <- subset(tax, ...)
  newabund <- abund[rownames(newtax), , drop=FALSE]
  newmetadata <- metadata[colnames(newabund), , drop=FALSE]
  
  #return a new list
  newlist <- list(abund = newabund, tax = newtax, metadata = newmetadata)
  return(newlist)
}

amp_load <- function(otutable, metadata, rarefy = NULL, percent = FALSE){
  # Remove whitespace from the otutable as this will break the structure of the taxonomy
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  otutable$Kingdom<-trim(as.character(otutable$Kingdom))
  otutable$Phylum<-trim(as.character(otutable$Phylum))
  otutable$Class<-trim(as.character(otutable$Class))
  otutable$Order<-trim(as.character(otutable$Order))
  otutable$Family<-trim(as.character(otutable$Family))
  otutable$Genus<-trim(as.character(otutable$Genus))
  otutable$Species<-trim(as.character(otutable$Species))
  
  #metadata: order rows by rownames
  rownames(metadata) <- metadata[,1]
  metadata = suppressWarnings(as.data.frame(as.matrix(metadata)))
  metadata <- metadata[order(rownames(metadata)), ]
  
  #abund: all columns from otutable except the last 7 to numeric and order rows by rownames:
  abund <- as.data.frame(otutable[,1:(ncol(otutable) - 7)])/1
  abund <- abund[order(rownames(abund)),order(colnames(abund))]
  #abundances to percent
  if(percent == TRUE) {
    abund <- as.data.frame(sapply(abund, function(x) x/sum(x)*100))
  }
  
  #tax: the last 7 columns from otutable to factor, order rows by rownames and order columns by taxonomic rank(not alphabetically)
  tax <- data.frame(otutable[, (ncol(otutable) - 6):ncol(otutable)] %>% transform(as.factor) 
                    ,OTU = rownames(otutable))
  tax <- tax[order(rownames(tax)), c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "OTU")]
  
  #data: return the data in a combined list
  data <- list(abund = abund, tax = tax, metadata = metadata)
  
  #rarefy function
  #  if(!is.null(rarefy)){data <- rarefy_even_depth(data, sample.size = rarefy, rngseed = 712)}metadata <- metadata[order(rownames(metadata)), ]
  
  #check if metadata and otutable match
  if(!all(rownames(data$metadata) %in% colnames(data$abund))) {
    stop("The sample names in metadata do not match those in otutable")
  }
  return(data)
}

amp_heatmap <- function(data, group = "Sample", normalise = NULL, scale = NULL, tax.aggregate = "Phylum", tax.add = NULL, tax.show = 10, tax.class = NULL, tax.empty = "best", order.x = NULL, order.y = NULL, plot.numbers = T, plot.breaks = NULL, plot.colorscale = "log10", plot.na = T, scale.seq = 100, output = "plot", plot.text.size = 4, plot.theme = "normal", calc = "mean", min.abundance = 0.1, max.abundance = NULL, sort.by = NULL, color.vector = NULL){
  
  ## Clean up the taxonomy
  data <- amp_rename(data = data, tax.class = tax.class, tax.empty = tax.empty, tax.level = tax.aggregate)
  
  ## Extract the data into separate objects for readability
  abund <- data[["abund"]]
  tax <- data[["tax"]]
  sample <- data[["metadata"]]
  
  ## Scale the data by a selected metadata sample variable
  if (!is.null(scale)){
    variable <- as.numeric(sample[,scale])
    abund <- t(t(abund)*variable)
  }
  
  ## Make a name variable that can be used instead of tax.aggregate to display multiple levels 
  suppressWarnings(
    if (!is.null(tax.add)){
      if (tax.add != tax.aggregate) {
        tax <- data.frame(tax, Display = apply(tax[,c(tax.add,tax.aggregate)], 1, paste, collapse="; "))
      }
    } else {
      tax <- data.frame(tax, Display = tax[,tax.aggregate])
    }
  )  
  
  # Aggregate to a specific taxonomic level
  abund3 <- cbind.data.frame(Display = tax[,"Display"], abund) %>%
    melt(id.var = "Display", value.name= "Abundance", variable.name = "Sample")
  
  abund3 <- data.table(abund3)[, sum:=sum(Abundance), by=list(Display, Sample)] %>%
    setkey(Display, Sample) %>%
    unique() %>% 
    as.data.frame()
  
  ## Add group information
  suppressWarnings(
    if (group != "Sample"){
      if (length(group) > 1){
        grp <- data.frame(Sample = rownames(sample), Group = apply(sample[,group], 1, paste, collapse = " ")) 
        oldGroup <- unique(cbind.data.frame(sample[,group], Group = grp$Group))
      } else{
        grp <- data.frame(Sample = rownames(sample), Group = sample[,group]) 
      }
      abund3$Group <- grp$Group[match(abund3$Sample, grp$Sample)]
      abund5 <- abund3
    } else{ abund5 <- data.frame(abund3, Group = abund3$Sample)}
  )
  
  ## Take the average to group level
  
  if (calc == "mean"){
    abund6 <- data.table(abund5)[, Abundance:=mean(sum), by=list(Display, Group)] %>%
      setkey(Display, Group) %>%
      unique() %>% 
      as.data.frame()
  }
  
  if (calc == "max"){
    abund6 <- data.table(abund5)[, Abundance:=max(sum), by=list(Display, Group)] %>%
      setkey(Display, Group) %>%
      unique() %>% 
      as.data.frame()
  }  
  
  if (calc == "median"){
    abund6 <- data.table(abund5)[, Abundance:=median(sum), by=list(Display, Group)] %>%
      setkey(Display, Group) %>%
      unique() %>% 
      as.data.frame()
  }
  
  
  ## Find the X most abundant levels
  if (calc == "mean"){
    TotalCounts <- group_by(abund6, Display) %>%
      summarise(Abundance = sum(Abundance)) %>%
      arrange(desc(Abundance))
  }
  
  if (calc == "max"){
    TotalCounts <- group_by(abund6, Display) %>%
      summarise(Abundance = max(Abundance)) %>%
      arrange(desc(Abundance))
  }
  
  if (calc == "median"){
    TotalCounts <- group_by(abund6, Display) %>%
      summarise(Abundance = median(Abundance)) %>%
      arrange(desc(Abundance))
  }
  
  if (!is.null(sort.by)){
    TotalCounts <- filter(abund6, Group == sort.by) %>%
      arrange(desc(Abundance))
  }
  
  ## Subset to X most abundant levels
  if (is.numeric(tax.show)){
    if (tax.show > nrow(TotalCounts)){  
      tax.show <- nrow(TotalCounts)
    }
    abund7 <- filter(abund6, Display %in% TotalCounts$Display[1:tax.show])
  }
  
  ## Subset to a list of level names
  if (!is.numeric(tax.show)){
    if (tax.show != "all"){
      abund7 <- filter(abund6, Display %in% tax.show)    
    }
    ### Or just show all  
    if (tax.show == "all"){
      tax.show <- nrow(TotalCounts)  
      abund7 <- filter(abund6, Display %in% TotalCounts$Display[1:tax.show]) 
    }
  }
  abund7 <- as.data.frame(abund7)
  
  ## Normalise to a specific group (The Abundance of the group is set as 1)  
  
  if(!is.null(normalise)){
    if (normalise != "relative"){
      temp <- dcast(abund7, Display~Group, value.var = "Abundance")
      temp1 <- cbind.data.frame(Display = temp$Display, temp[,-1]/temp[,normalise])   
      abund7 <- melt(temp1, id.var = "Display", value.name="Abundance", variable.name="Group")
    }
  } 
  if(!is.null(normalise)){
    if (normalise == "relative"){
      temp <- dcast(abund7, Display~Group, value.var = "Abundance")
      temp1 <- cbind.data.frame(Display = temp[,1], temp[,-1]/apply(as.matrix(temp[,-1]), 1, mean))    
      abund7 <- melt(temp1, id.var = "Display" , value.name="Abundance", variable.name="Group")
    }
  }
  
  ## Order.y
  if (is.null(order.y)){
    abund7$Display <- factor(abund7$Display, levels = rev(TotalCounts$Display))
  }
  if (!is.null(order.y)){
    if ((length(order.y) == 1) && (order.y != "cluster")){       
      temp1 <- filter(abund7, Group == order.y) %>%
        group_by(Display) %>%
        summarise(Mean = mean(Abundance)) %>%
        arrange(desc(Mean))
      
      abund7$Display <- factor(abund7$Display, levels = rev(temp1$Display))
    }
    if (length(order.y) > 1){
      abund7$Display <- factor(abund7$Display, levels = order.y)
    }
    if ((length(order.y) == 1) && (order.y == "cluster")){
      if (is.null(max.abundance)){max.abundance <- max(abund7$Abundance)}
      tdata <- mutate(abund7, 
                      Abundance = ifelse(Abundance < min.abundance, min.abundance, Abundance),
                      Abundance = ifelse(Abundance > max.abundance, max.abundance, Abundance))
      tdata <- dcast(tdata, Display~Group, value.var = "Abundance")
      rownames(tdata) <- tdata$Display
      tdata2 <- tdata[,-1]
      tclust <- hclust(dist(tdata2))
      tnames <- levels(droplevels(tdata$Display))[tclust$order]
      abund7$Display <- factor(abund7$Display, levels = tnames)
    }
  }
  
  ## Order.x
  if (!is.null(order.x)){
    if ((length(order.x) == 1) && (order.x != "cluster")){
      temp1 <- filter(abund7, Display == order.x) %>%
        group_by(Group) %>%
        summarise(Mean = mean(Abundance)) %>%
        arrange(desc(Mean))
      abund7$Group <- factor(abund7$Group, levels = as.character(temp1$Group))
    }    
    if (length(order.x) > 1){
      abund7$Group <- factor(abund7$Group, levels = order.x)
    }
    if ((length(order.x) == 1) && (order.x == "cluster")){
      if (is.null(max.abundance)){max.abundance <- max(abund7$Abundance)}
      tdata <- mutate(abund7, 
                      Abundance = ifelse(Abundance < min.abundance, min.abundance, Abundance),
                      Abundance = ifelse(Abundance > max.abundance, max.abundance, Abundance))
      tdata <- dcast(tdata, Display~Group, value.var = "Abundance")
      rownames(tdata) <- tdata$Display
      tdata2 <- tdata[,-1]
      tclust <- hclust(dist(t(tdata2)))
      tnames <- tclust$labels[tclust$order]
      abund7$Group <- factor(abund7$Group, levels = tnames) 
    }
  }
  
  ## Handle NA values
  if(plot.na == F){ plot.na <- "grey50" }else{ if(!is.null(color.vector)) {plot.na <-color.vector[1]} else {plot.na <-"#67A9CF"}}  
  
  ## Scale to percentages if not normalised and scaled
  
  if (is.null(scale) & is.null(normalise)){
    abund7[,3] <- abund7[,3]/scale.seq*100
  }
  
  if (length(group) > 1 ){ abund7 <- merge(abund7, oldGroup)}
  
  if (is.null(min.abundance)){
    min.abundance <- ifelse(min(abund7$Abundance) > 0.001, min(abund7$Abundance), 0.001)
  }
  if (is.null(max.abundance)){
    max.abundance <- max(abund7$Abundance)
  }
  
  ## Make a heatmap style plot
  p <- ggplot(abund7, aes_string(x = "Group", y = "Display", label = formatC("Abundance", format = "f", digits = 1))) +     
    geom_tile(aes(fill = Abundance), colour = "white", size = 0.5) +
    theme(axis.text.x = element_text(size = 10, hjust = 1, angle = 90)) + 
    theme(axis.text.y = element_text(size = 12)) + 
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
  
  ## Get colorpalette for colorscale or set default
  if (!is.null(color.vector)){
    color.pal = color.vector
  } else {
    color.pal = rev(brewer.pal(3, "RdBu"))
  }
  
  if (plot.numbers == T){
    abund8 <- abund7
    abund8$Abundance <- round(abund8$Abundance, 1)
    p <- p + geom_text(data = abund8, size = plot.text.size, colour = "grey10")  
  }
  if (is.null(plot.breaks)){
    p <- p +scale_fill_gradientn(colours = color.pal, trans = plot.colorscale, na.value=plot.na, oob = squish, limits = c(min.abundance, max.abundance))
  }
  if (!is.null(plot.breaks)){
    p <- p +scale_fill_gradientn(colours = color.pal, trans = plot.colorscale, breaks=plot.breaks, na.value=plot.na , oob = squish, limits = c(min.abundance, max.abundance))
  }
  
  
  if (is.null(normalise)){
    p <- p + labs(x = "", y = "", fill = "% Read\nAbundance")  
  }
  if (!is.null(normalise)){
    p <- p + labs(x = "", y = "", fill = "Relative")  
  }
  
  if(plot.theme == "clean"){
    p <- p + theme(legend.position = "none",
                   axis.text.y = element_text(size = 8, color = "black"),
                   axis.text.x = element_text(size = 8, color = "black", vjust = 0.5),
                   axis.title = element_blank(),
                   text = element_text(size = 8, color = "black"),
                   axis.ticks.length = unit(1, "mm"),
                   plot.margin = unit(c(0,0,0,0), "mm"),
                   title = element_text(size = 8)
    )
  }
  
  ## Define the output 
  if (output == "complete"){
    outlist <- list(heatmap = p, data = abund7)
    return(outlist)  
  }
  if (output == "plot"){
    return(p)
  }
}

amp_rabund <- function(data, group = "Sample", order.group = NULL, tax.show = 50, scale.seq = 100, tax.clean = T, plot.type = "boxplot", plot.log = F, output = "plot", tax.add = NULL, tax.aggregate = "Genus", tax.empty = "best", tax.class = NULL, point.size = 2, plot.flip = F, sort.by = "median", adjust.zero = NULL, plot.theme = "normal", order.y = NULL){
  
  ## Clean up the taxonomy
  data <- amp_rename(data = data, tax.class = tax.class, tax.empty = tax.empty, tax.level = tax.aggregate)
  
  ## Extract the data into separate objects for readability
  abund <- data[["abund"]]
  tax <- data[["tax"]]
  sample <- data[["metadata"]]
  
  ## Make a name variable that can be used instead of tax.aggregate to display multiple levels 
  suppressWarnings(
    if (!is.null(tax.add)){
      if (tax.add != tax.aggregate) {
        tax <- data.frame(tax, Display = apply(tax[,c(tax.add,tax.aggregate)], 1, paste, collapse="; "))
      }
    } else {
      tax <- data.frame(tax, Display = tax[,tax.aggregate])
    }
  )  
  
  # Aggregate to a specific taxonomic level
  abund3 <- cbind.data.frame(Display = tax[,"Display"], abund) %>%
    melt(id.var = "Display", value.name= "Abundance", variable.name = "Sample")
  
  abund3 <- data.table(abund3)[, Abundance:=sum(Abundance), by=list(Display, Sample)] %>%
    setkey(Display, Sample) %>%
    unique() %>% 
    as.data.frame()
  
  ## Add group information
  suppressWarnings(
    if (group != "Sample"){
      if (length(group) > 1){
        grp <- data.frame(Sample = rownames(sample), Group = apply(sample[,group], 1, paste, collapse = " ")) 
      } else{
        grp <- data.frame(Sample = rownames(sample), Group = sample[,group]) 
      }
      abund3$Group <- grp$Group[match(abund3$Sample, grp$Sample)]
      abund5 <- abund3
    } else{ abund5 <- data.frame(abund3, Group = abund3$Sample)}
  ) 
  
  if (plot.type != "curve"){
    ## Find the x most abundant levels and sort
    TotalCounts <- group_by(abund5, Display) %>%
      summarise(Median = median(Abundance), Total = sum(Abundance), Mean = mean(Abundance))
    if(sort.by == "median"){TotalCounts %<>% arrange(desc(Median)) %>% as.data.frame()}
    if(sort.by == "mean"){TotalCounts %<>% arrange(desc(Mean)) %>% as.data.frame()}
    if(sort.by == "total"){TotalCounts %<>% arrange(desc(Total)) %>% as.data.frame()}
    
    abund5$Display <- factor(abund5$Display, levels = rev(TotalCounts$Display))
    
    ## Subset to the x most abundant levels
    if (is.numeric(tax.show)){
      message(tax.show)
      if (tax.show > nrow(TotalCounts)){  
        tax.show <- nrow(TotalCounts)
      }
      abund7 <- subset(abund5, abund5$Display %in% TotalCounts[1:tax.show,"Display"])  
    }
    ## Subset to a list of level names
    if (!is.numeric(tax.show)){
      if (length(tax.show) > 1){
        abund7 <- subset(abund5, as.character(abund5$Display) %in% tax.show)
      }
      if ((length(tax.show) == 1) && (tax.show != "all")){
        abund7 <- subset(abund5, as.character(abund5$Display) %in% tax.show)
      }
      ### Or just show all  
      if ((length(tax.show) == 1) && (tax.show == "all")){
        tax.show <- nrow(TotalCounts)  
        abund7 <- subset(abund5, abund5$Display %in% TotalCounts[1:tax.show,"Display"])  
      }
    }
    
    ## Scale to a specific abundance
    abund7$Abundance <- abund7$Abundance/scale.seq*100
    
    ## Add a small constant to handle ggplot2 removal of 0 values in log scaled plots
    if(!is.null(adjust.zero)){
      abund7$Abundance[abund7$Abundance==0] <- adjust.zero
    }
    
    ## Order y based on a vector
    if (length(order.y) > 1){
      abund7$Display <- factor(abund7$Display, levels = order.y)
      abund7 <- subset(abund7, !is.na(Display))
    }
    
    ## plot the data
    if (group == "Sample"){
      p <-ggplot(abund7, aes(x = Display, y = Abundance))   
    }
    if (group != "Sample"){
      if(!is.null(order.group)){
        abund7$Group <- factor(abund7$Group, levels = rev(order.group))
      }
      p <-ggplot(abund7, aes(x = Display, y = Abundance, color = Group))   
    }
    
    p <- p +  ylab("Read Abundance (%)") + guides(col = guide_legend(reverse = TRUE)) + xlab("")
    
    if (plot.flip == F){ p <- p + coord_flip() } else{
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    }
    
    if (plot.type == "point"){ p <- p + geom_point(size = point.size) }
    if (plot.type == "boxplot"){p <- p + geom_boxplot(outlier.size = point.size)}
    if (plot.log ==T){ p <- p + scale_y_log10()}
    
    outlist <- list(plot = p, data = abund7)
  }
  
  ## If type = curve then generate a second dataframe
  
  if (plot.type == "curve"){
    temp3 <- group_by(abund5, Display, Group) %>%
      summarise(Mean = mean(Abundance))
    
    TotalCounts <- temp3[with(temp3, order(-Mean)),] %>%
      group_by(Group) %>%
      mutate(dummy = 1) %>%
      mutate(Cumsum = cumsum(Mean), Rank = cumsum(dummy)) %>%
      as.data.frame()
    
    if(!is.null(order.group)){
      TotalCounts$Group <- factor(TotalCounts$Group, levels = rev(order.group))
    }
    TotalCounts$Cumsum <- TotalCounts$Cumsum/scale.seq * 100
    
    p <- ggplot(data = TotalCounts, aes(x = Rank, y = Cumsum, color = Group)) +
      geom_line(size = 2) +
      ylim(0,100) +
      xlab("Rank abundance") +
      ylab("Cummulative read abundance (%)")  
    if (plot.log ==T){
      p <- p + scale_x_log10() 
    } 
    
    outlist <- list(plot = p, data = TotalCounts)
  }
  
  if(plot.theme == "clean"){
    p <- p + theme(axis.ticks.length = unit(1, "mm"),
                   axis.ticks = element_line(color = "black"),
                   text = element_text(size = 10, color = "black"),
                   axis.text = element_text(size = 8, color = "black"),
                   plot.margin = unit(c(0,0,0,0), "mm"),
                   panel.grid.major = element_line(color = "grey90"),
                   panel.grid.minor = element_blank(),
                   legend.key = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(color = "black")
    )
  }
  
  if(output == "complete"){ return(outlist) }
  if(output == "plot"){ return(p) }
}

amp_ordinate <- function(data, scale = NULL, trans = "sqrt", ordinate.type = "PCA", ncomp = 5, plot.x = "PC1", plot.y = "PC2", plot.color = NULL, plot.color.order = NULL, plot.point.size = 3, plot.shape = NULL, plot.species = F, plot.nspecies = NULL, plot.nspecies.tax = "Genus", plot.label = NULL, plot.group = NULL, plot.group.label = NULL, envfit.factor = NULL, envfit.numeric = NULL, envfit.significant = 0.001, envfit.resize = 1, envfit.color = "darkred", envfit.textsize = 3, envfit.show = T, tax.empty ="best", output = "plot", constrain = NULL, scale.species = F, trajectory = NULL, trajectory.group = trajectory, plot.group.label.size = 4, plot.theme = "normal", plot.group.manual = NULL, plot.label.size = 3, plot.label.repel = F, plot.label.seqment.color = "black", plot.nspecies.repel = F, plot.species.size = 2, plot.nspecies.size = 4){
  
  ## Clean up the taxonomy
  data <- amp_rename(data = data, tax.empty = tax.empty)
  
  ## Extract the data into separate objects for readability
  abund <- data[["abund"]]
  tax <- data[["tax"]]
  sample <- data[["metadata"]]
  
  outlist <- list(abundance = abund, taxonomy = tax, sampledata = sample)
  
  if (length(trajectory.group) > 1){
    sample$TrajGroup <- do.call(paste, c(as.list(sample[,trajectory.group]), sep=" "))
    trajectory.group <- "TrajGroup"
  }
  
  if (length(plot.color) > 1){
    sample$ColorGroup <- do.call(paste, c(as.list(sample[,plot.color]), sep=" "))
    plot.color <- "ColorGroup"
  }
  
  ## Scale the data
  if (!is.null(scale)){
    variable <- unlist(sample[,scale])
    abund <- t(t(abund)*variable)
  }
  
  ## Transform the data
  abund1 <- abund
  if (trans == "sqrt"){ abund1 <- sqrt(abund)}
  
  ## Calculate NMDS
  
  if (ordinate.type == "NMDS"){
    plot.x = "MDS1"
    plot.y = "MDS2"
    
    model <- metaMDS(t(abund1))
    combined <- cbind.data.frame(model$points, sample) 
    
    loadings <- cbind.data.frame(model$species, tax)
    loadings$MDS1 <- loadings$MDS1*attributes(model$species)$shrinkage[1]
    loadings$MDS2 <- loadings$MDS2*attributes(model$species)$shrinkage[2]
    OTU <- gsub("denovo", "", rownames(loadings))
    loadings <- cbind.data.frame(loadings, OTU)
    
    if(scale.species == T){
      maxx <- max(abs(scores[,plot.x]))/max(abs(loadings[,plot.x]))
      loadings[, plot.x] <- loadings[, plot.x] * maxx * 0.8
      
      maxy <- max(abs(scores[,plot.y]))/max(abs(loadings[,plot.y]))
      loadings[, plot.y] <- loadings[, plot.y] * maxy * 0.8 
    }
    
    species <- cbind(loadings, loadings[,plot.x]^2 + loadings[,plot.y]^2)
    colnames(species)[ncol(species)] <- "dist"
    species <- species[with(species, order(-dist)), ]
    
    outlist <- append(outlist, list(nmds.model = model, nmds.scores = combined, nmds.loadings = species))
  }
  
  ## Calculate PCA
  
  if (ordinate.type == "PCA"){
    
    if(is.null(constrain)){
      model <- rda(t(abund1))  
      exp <- round(model$CA$eig/model$CA$tot.chi*100,1)
    }
    
    if(!is.null(constrain)){
      constrain1 <- as.data.frame(sample[, constrain])
      colnames(constrain1) <- "constrain"
      model <- rda(t(abund1) ~ constrain1$constrain)  
      plot.x <- "RDA1"
      if (model$CCA$rank > 1){
        plot.y <- "RDA2"
      }
      exp <- round(model$CA$eig/model$tot.chi*100,1)
      expCCA <- round(model$CCA$eig/model$CA$tot.chi*100,1)
      exp <- c(exp, expCCA)
    }
    
    scores <- scores(model, choices = 1:ncomp)$sites
    combined <- cbind.data.frame(sample, scores)
    
    loadings <- cbind.data.frame(scores(model, choices = 1:ncomp)$species, tax)
    OTU <- gsub("denovo", "", rownames(loadings))
    loadings <- cbind.data.frame(loadings, OTU)
    
    if(scale.species == T){
      maxx <- max(abs(scores[,plot.x]))/max(abs(loadings[,plot.x]))
      loadings[, plot.x] <- loadings[, plot.x] * maxx * 0.8
      
      maxy <- max(abs(scores[,plot.y]))/max(abs(loadings[,plot.y]))
      loadings[, plot.y] <- loadings[, plot.y] * maxy * 0.8
      
    }
    
    species <- cbind(loadings, loadings[,plot.x]^2 + loadings[,plot.y]^2)
    colnames(species)[ncol(species)] <- "dist"
    species <- species[with(species, order(-dist)), ]
    
    outlist <- append(outlist, list(pca.model = model, pca.scores = combined, loadings = species))
  }
  
  ## Fit environmental factors using vegans "envfit" function
  
  if(!is.null(envfit.factor)){   
    fit  <- suppressWarnings(as.data.frame(as.matrix(sample[, envfit.factor])))
    if (ordinate.type == "PCA"){ef.f <- envfit(model, fit, permutations = 999, choices=c(plot.x, plot.y))}
    if (ordinate.type == "NMDS"){ef.f <- envfit(model, fit, permutations = 999)}
    temp <- cbind.data.frame(rownames(ef.f$factors$centroids),ef.f$factors$var.id, ef.f$factors$centroids)
    colnames(temp)[1:2] <- c("Name","Variable")
    temp1 <- cbind.data.frame(names(ef.f$factors$pvals), ef.f$factors$pvals)
    colnames(temp1) <- c("Variable", "pval")
    temp2 <- merge(temp, temp1)
    f.sig <- subset(temp2, pval <= envfit.significant)   
    if (ordinate.type == "NMDS"){colnames(f.sig)[3:4] <- c("MDS1", "MDS2")}
    outlist <- append(outlist, list(eff.model = ef.f))
  }
  
  ## Fit environmental numeric data using vegans "envfit" function
  
  if (!is.null(envfit.numeric)){
    fit <- suppressWarnings(as.data.frame(as.matrix(sample[, envfit.numeric])))
    if (ordinate.type == "PCA") {suppressWarnings(ef.n <- envfit(model, fit, permutations = 999, choices=c(plot.x, plot.y)))}
    if (ordinate.type == "NMDS") {ef.n <- envfit(model, fit, permutations = 999)}
    temp <- cbind.data.frame(rownames(ef.n$vectors$arrows), ef.n$vectors$arrows*sqrt(ef.n$vectors$r), ef.n$vectors$pvals)
    colnames(temp)[c(1,4)] <- c("Name","pval")      
    n.sig <- subset(temp, pval <= envfit.significant)          
    if (ordinate.type == "NMDS"){colnames(n.sig)[2:3] <- c("MDS1", "MDS2")}
    outlist <- append(outlist, list(efn.model = ef.n))
  }
  
  ## Order the colors
  
  if (!is.null(plot.color.order)) {
    combined[,plot.color] <- factor(combined[,plot.color], levels = plot.color.order)
  }
  
  ## Plot  
  
  ### Plot: Basic plot either with or without colors
  p <- ggplot(combined, aes_string(x = plot.x, y = plot.y, color = plot.color, shape = plot.shape))
  
  ### Plot: Add loadings as points
  if(plot.species == T){
    p <- p + geom_point(data = species, color = "grey", shape = 20, size = plot.species.size)        
  }
  
  ###Plot: Add trajectory based on e.g. data
  if (!is.null(trajectory)){
    traj <- combined[order(combined[,trajectory]),]
    p <- p + geom_path(data = traj, aes_string(group = trajectory.group))
  }
  
  ### Plot: Add samples as points
  p <- p + geom_point(size = plot.point.size)
  
  ### Plot: If PCA add explained variance to the axis
  if(ordinate.type == "PCA"){
    p <- p + xlab(paste(plot.x, " [",exp[plot.x],"%]", sep = "")) +
      ylab(paste(plot.y, " [",exp[plot.y],"%]", sep = ""))  
  }  
  
  ###Plot: Group the data either by centroid or chull
  if (is.null(plot.group.manual)){
    plot.group.manual <- plot.color
  }
  
  if (!is.null(plot.group)& !is.null(plot.color)){    
    os <- data.frame(group = combined[,plot.group.manual], x = combined[,plot.x], y = combined[,plot.y]) %>%
      group_by(group) %>%
      summarise(cx = mean(x), cy = mean(y)) %>%
      as.data.frame()
    os2 <- merge(combined, os, by.x=plot.group.manual, by.y = "group")
    
    
    if (plot.group == "centroid"){
      p <- p + geom_segment(data=os2, aes_string(x = plot.x, xend = "cx", y = plot.y, yend = "cy", color = plot.color), size = 1)
    }
    
    if (plot.group == "chull"){
      splitData <- split(combined, combined[,plot.group.manual]) %>%
        lapply(function(df){df[chull(df[,plot.x], df[,plot.y]), ]})
      hulls <- do.call(rbind, splitData)
      
      p <- p + geom_polygon(data=hulls, aes_string(fill = plot.color, group = plot.group.manual), alpha = 0.2)
    }
  }
  
  if (!is.null(plot.group.label)){
    os <- data.frame(group = combined[,plot.group.label], x = combined[,plot.x], y = combined[,plot.y]) %>%
      group_by(group) %>%
      summarise(cx = mean(x), cy = mean(y)) %>%
      as.data.frame()
    
    os2 <- merge(combined, os, by.x=plot.group.label, by.y = "group")
    os3<- os2[!duplicated(os2[,plot.group.label]),]
    p <- p + geom_text(data=os3, aes_string(x = "cx", y = "cy", label = plot.group.label), size = plot.group.label.size , color = "black", fontface = 2) 
  }
  
  ### Plot: Plot the names of the n most extreme species
  if(!is.null(plot.nspecies)){
    if(plot.nspecies.repel == F){
      p <- p + geom_text(data = species[1:plot.nspecies,], aes_string(x = plot.x, y = plot.y, label = plot.nspecies.tax), colour = "black", size = plot.nspecies.size, inherit.aes = F)  
    } else {
      p <- p + geom_text_repel(data = species[1:plot.nspecies,], aes_string(x = plot.x, y = plot.y, label = plot.nspecies.tax), colour = "black", size = plot.nspecies.size, inherit.aes = F)  
    }
  }
  
  ### Plot: Environmental factors
  if(!is.null(envfit.factor) & envfit.show == T){
    if (nrow(f.sig) != 0){
      p <- p + geom_text(data = f.sig, aes_string(x = plot.x, y = plot.y, label = "Name"), colour = envfit.color, size = 4, hjust = -0.05, vjust = 1, inherit.aes = F, size = envfit.textsize)
    }
  }
  
  ### Plot: Environmental numeric data
  if(!is.null(envfit.numeric) & envfit.show == T){
    if (nrow(n.sig) != 0){
      n.sig[, plot.x] <- n.sig[, plot.x]*envfit.resize
      n.sig[, plot.y] <- n.sig[, plot.y]*envfit.resize
      n.sig2 <- n.sig
      n.sig2[, plot.x] <- n.sig[, plot.x]*1.2
      n.sig2[, plot.y] <- n.sig[, plot.y]*1.2
      
      p <- p + geom_segment(data=n.sig, aes_string(x = 0, xend = plot.x, y = 0, yend = plot.y),arrow = arrow(length = unit(3, "mm")), colour = "darkred", size = 1 , inherit.aes = F) +
        geom_text(data=n.sig2, aes_string(x = plot.x, y = plot.y, label = "Name"), colour = envfit.color, inherit.aes = F, size = envfit.textsize)
    }
  }  
  
  ### Plot: Label all samples using sample data
  
  if (!is.null(plot.label)){
    if(plot.label.repel == F){
      p <- p + geom_text(aes_string(label=plot.label), size = plot.label.size, color = "grey40", vjust = 1.5)
    } else{
      p <- p + geom_text_repel(aes_string(label=plot.label), size = plot.label.size, color = "grey40", segment.color = plot.label.seqment.color)
    }
  }  
  
  if(plot.theme == "clean"){
    p <- p + theme(axis.ticks.length = unit(1, "mm"),
                   axis.ticks = element_line(color = "black"),
                   text = element_text(size = 10, color = "black"),
                   axis.text = element_text(size = 8, color = "black"),
                   plot.margin = unit(c(0,0,0,0), "mm"),
                   panel.grid = element_blank(),
                   legend.key = element_blank(),
                   panel.background = element_blank(),
                   axis.line.x = element_line(color = "black"),
                   axis.line.y = element_line(color = "black"))
  }
  
  outlist <- append(outlist, list(plot = p))
  
  ## Export the data
  
  if(output == "plot"){
    return(outlist$plot)
  }
  if(output == "complete"){
    return(outlist)
  }
  
}

amp_rename <- function(data, tax.class = NULL, tax.empty = "best", tax.level = "Genus"){
  
  tax = data[["tax"]]
  
  ## First make sure that all entires are strings
  for ( i in 1:ncol(tax) ){
    tax[,i] <- as.character(tax[,i])  
  }
  
  ## Change a specific phylum to class level
  if(!is.null(tax.class)){
    for (i in 1:nrow(tax)){
      if (!is.na(tax$Phylum[i]) & tax$Phylum[i] %in% tax.class){
        tax$Phylum[i] <- tax$Class[i]   
      }
    }
  }
  
  ## Remove the underscore classifier from the data  
  tax$Kingdom <- gsub("k__", "", tax$Kingdom)
  tax$Phylum <- gsub("p__", "", tax$Phylum)
  tax$Phylum <- gsub("c__", "", tax$Phylum)
  tax$Class <- gsub("c__", "", tax$Class)
  tax$Order <- gsub("o__", "", tax$Order)
  tax$Family <- gsub("f__", "", tax$Family)
  tax$Genus <- gsub("g__", "", tax$Genus)
  tax$Kingdom <- gsub("uncultured", "", tax$Kingdom)
  tax$Phylum <- gsub("uncultured", "", tax$Phylum)
  tax$Phylum <- gsub("uncultured", "", tax$Phylum)
  tax$Class <- gsub("uncultured", "", tax$Class)
  tax$Order <- gsub("uncultured", "", tax$Order)
  tax$Family <- gsub("uncultured", "", tax$Family)
  tax$Genus <- gsub("uncultured", "", tax$Genus)
  
  ## Check if there is a species level otherwise add it for consistency
  if (!is.null(tax$Species)){
    tax$Species <- gsub("s__", "", tax$Species)
  } else {
    tax$Species <- ""
  }
  
  tax[is.na(tax)] <- ""
  
  ## How to handle empty taxonomic assignments
  if (tax.empty == "OTU"){
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
  if(tax.empty == "best"){
    tax <- mutate(tax, Kingdom, Kingdom = ifelse(Kingdom == "", "Unclassified", Kingdom)) %>%
      mutate(Phylum, Phylum = ifelse(Phylum == "", paste("k__", Kingdom, "_", rownames(tax), sep = ""), Phylum)) %>%
      mutate(Class, Class = ifelse(Class == "", ifelse(grepl("__", Phylum), Phylum, paste("c__", Phylum, "_", rownames(tax), sep = "")), Class)) %>%
      mutate(Order, Order = ifelse(Order == "", ifelse(grepl("__", Class), Class, paste("c__", Class, "_", rownames(tax), sep = "")), Order)) %>%
      mutate(Family, Family = ifelse(Family == "", ifelse(grepl("__", Order), Order, paste("o__", Order, "_", rownames(tax), sep = "")), Family)) %>%
      mutate(Genus, Genus = ifelse(Genus == "", ifelse(grepl("__", Family), Family, paste("f__", Family, "_", rownames(tax), sep = "")), Genus)) %>%
      mutate(Species, Species = ifelse(Species == "", ifelse(grepl("__", Genus), Genus, paste("g__", Genus, "_", rownames(tax), sep = "")), Species))
  }
  
  if(tax.empty == "remove"){
    abund <- data[["abund"]]
    tax <- subset(tax, tax[,tax.level] != "")
    abund <- subset(abund, rownames(abund) %in% rownames(tax))
    data[["abund"]] <- abund
  }
  data[["tax"]] <- tax
  
  return(data)
}
