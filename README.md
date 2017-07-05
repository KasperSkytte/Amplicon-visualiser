# Amplicon-visualiser
A shiny app to visualise amplicon data based on the ampvis package (https://github.com/KasperSkytte/ampvis)

Live at https://kasperskytte.shinyapps.io/shinyampvis/

Run in RStudio (as administrator) on your local computer for larger data sets:
```
install.packages("shiny")
install.packages("devtools")
install.packages("Cairo") (For prettier plots)
install.packages("tidyverse")

source("http://bioconductor.org/biocLite.R")
biocLite("Biostrings")
biocLite("DESeq2")
biocLite("munsell")
biocLite("stringi")
biocLite("chron")
biocLite("permute")
biocLite("assertthat")
biocLite("tibble")
biocLite("rmarkdown")
biocLite("readxl")

shiny::runGitHub("amplicon-visualiser","kasperskytte")
```

If you get the error "there is no package 'packagename'" simply use install.packages("packagename") to install the required package(s). 

Currently internal project on Aalborg University
