requiredPackages = c('reticulate', 'Metrics', 'ggplot2', 'epiR', 'lme4', 'irr', 'sjPlot', 'plotly',
                     'shiny', 'shinythemes', 'shinydashboard')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

runApp('ShinyDashboard')