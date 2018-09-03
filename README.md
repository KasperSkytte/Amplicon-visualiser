# Amplicon-visualiser
A shiny app to visualise amplicon data based on the ampvis2 package (https://github.com/MadsAlbertsen/ampvis2).

Live at https://kasperskytte.shinyapps.io/shinyampvis/

Run in RStudio (as administrator) on your local computer for larger data sets:
```
install.packages("shiny")
shiny::runGitHub("amplicon-visualiser","kasperskytte")
```

This will install all the required packages as well as launch the app. If you still get the error "there is no package 'packagename'" then just use `install.packages("packagename")` to install the required package(s). 