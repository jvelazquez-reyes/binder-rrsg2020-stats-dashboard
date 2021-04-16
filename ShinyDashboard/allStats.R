##Call Python script from R##

#Disable prompt to install miniconda
Sys.setenv(RETICULATE_MINICONDA_ENABLED = "FALSE")
reticulate::source_python('nist.py')

data <- read.csv("3T_NIST_T1maps_database.csv")
data[] <- gsub("[][]", "", as.matrix(data))

submission <- 1:40
listSpheres = list()
list2append = list()
for (i in submission){
  for (j in seq(1,14)){
    dataSphere = gsub("\\. ","",data[i,j+grep("^T1...NIST.sphere.1$", colnames(data))-1])
    dataSphere = as.matrix(as.numeric(unlist(strsplit(dataSphere," "))))
    dataSphere = dataSphere[!is.na(dataSphere)]
    
    list2append[[j]] = dataSphere
  }
  listSpheres[[i]] = list2append
}

##COMPARE MAGNITUDE VS COMPLEX##
source("comparison_magnitude_complex.R")

cases <- c(1,seq(11,25,2),34,36)

#p-value > 0.5, there's no statistical difference between magnitude and complex
magVScomp <- comparison_magnitude_complex(cases,listSpheres)

##ANALYSIS WITHIN GROUPS ACROSS SITES
source("comparison_across_sites.R")

#Germany <- 13:26
#Montreal <- c(4,11,12,27:30,40)
#US <- c(3,5:10,32:39)
#London <- 1:2
#Australia <- 31
#id = data[scans[j],"id"]
London <- c(1.001,1.002)
US <- c(2.001,4.001,4.002,4.003,4.004,4.005,4.006,10.001,10.002,11.001,11.002,11.003,11.004,12.001,12.002)
Montreal <- c(3.001,5.001,5.002,7.001,7.002,8.001,8.002,13.001)
Germany <- c(6.001,6.002,6.003,6.004,6.005,6.006,6.007,6.008,6.009,6.010,6.011,6.012,6.013,6.014)
Australia <- c(9.001)

SiteGermany <- comparison_across_sites(Germany)
SiteMontreal <- comparison_across_sites(Montreal)
SiteUS <- comparison_across_sites(US)
SiteLondon <- comparison_across_sites(London)
SiteAustralia <- comparison_across_sites(Australia)

##LOADING NIST_whitelists##
whitelist <- fromJSON(file = "NIST_whitelists.json")

filteredSites <- whitelist$whitelists$`one measurement per scanner`$whitelist
labelSidSite <- matrix(0L, nrow = length(filteredSites), ncol = 1)
for (ii in seq(1,length(filteredSites))){
  if(filteredSites[ii] %in% London){labelSidSite[ii] = paste(filteredSites[ii],"London")}
  if(filteredSites[ii] %in% US){labelSidSite[ii] = paste(filteredSites[ii],"US")}
  if(filteredSites[ii] %in% Montreal){labelSidSite[ii] = paste(filteredSites[ii],"Montreal")}
  if(filteredSites[ii] %in% Germany){labelSidSite[ii] = paste(filteredSites[ii],"Germany")}
  if(filteredSites[ii] %in% Australia){labelSidSite[ii] = paste(filteredSites[ii],"Australia")}
}

##COMPARISON BETWEEN MEASURED AND REFERENCE T1 VALUES##
source("measuredT1_against_referenceT1.R")

scans <- 1:4
RefVSMeas <- measuredT1_against_referenceT1(scans)
sdMontreal <- measuredT1_against_referenceT1(Montreal)
sdGermany <- measuredT1_against_referenceT1(Germany)

##LINEAR MIXED EFFECTS MODEL##
source("linear_mixed_effects_model.R")

sites <- 1:6
sitesLMEM <- linear_mixed_effects_model(sites)
