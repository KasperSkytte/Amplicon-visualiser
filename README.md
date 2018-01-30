# Amplicon-visualiser
A shiny app to visualise amplicon data based on the ampvis2 package (https://github.com/MadsAlbertsen/ampvis2)

Live at https://kasperskytte.shinyapps.io/shinyampvis/

Run in RStudio (as administrator) on your local computer for larger data sets:
```
install.packages("shiny")
install.packages("devtools")
install.packages("Cairo") (For prettier plots on Linux/Mac)
install.packages("tidyverse")

shiny::runGitHub("amplicon-visualiser","kasperskytte")
```

If you get the error "there is no package 'packagename'" use `install.packages("packagename")` to install the required package(s). 

Currently internal project on Aalborg University.