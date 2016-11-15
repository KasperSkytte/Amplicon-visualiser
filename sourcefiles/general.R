
######### Functions #########
load_data <- function (otutable, metadata, refseq = NULL, rarefy = NULL, to_percentages = FALSE) {
  otu_counts <- otutable[, 1:(ncol(otutable) - 7)]
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)
  otutable$Kingdom <- trim(as.character(otutable$Kingdom))
  otutable$Phylum <- trim(as.character(otutable$Phylum))
  otutable$Class <- trim(as.character(otutable$Class))
  otutable$Order <- trim(as.character(otutable$Order))
  otutable$Family <- trim(as.character(otutable$Family))
  otutable$Genus <- trim(as.character(otutable$Genus))
  otutable$Species <- trim(as.character(otutable$Species))
  taxonomy <- otutable[, (ncol(otutable) - 6):ncol(otutable)] %>% 
    as.matrix()
  rownames(metadata) <- metadata[, 1]
  if (!is.null(refseq)) {
    data <- phyloseq(otu_table(otu_counts, taxa_are_rows = T), 
                     tax_table(taxonomy), sample_data(metadata), refseq)
  }
  else {
    data <- phyloseq(otu_table(otu_counts, taxa_are_rows = T), 
                     tax_table(taxonomy), sample_data(metadata))
  }
  data <- transform_sample_counts(data, function(x) x/1) #Hvorfor det?
  if (!is.null(rarefy)) {
    data <- rarefy_even_depth(data, sample.size = rarefy, 
                              rngseed = 712)
  }
  #added to_percentages argument
  if (to_percentages == TRUE) {
    data <- transform_sample_counts(data, function(x) x/sum(x)*100) %>% 
      filter_taxa(function(x) max(x) >= 0.1, TRUE) %>%
      subset_samples(sample_sums(d) > 400)
  }
  return(data)
}
