# Amplicon-visualiser
A shiny app to visualise amplicon data based on the ampvis2 package (https://github.com/MadsAlbertsen/ampvis2)

Live at https://kasperskytte.shinyapps.io/shinyampvis/

Run in RStudio (as administrator) on your local computer for larger data sets:
```
if(!require("shiny"))
  install.packages("shiny")
shiny::runGitHub("amplicon-visualiser","kasperskytte")
```
