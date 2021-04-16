comparison_across_sites <- function(site){
  meanSite = data.frame()
  for (j in seq(1,length(site))){
    for (k in seq(1,14)){
      rowIndex = match(site[j],as.numeric(data[,"id"]))
      siteData = as.numeric(unlist(listSpheres[[rowIndex]][k]))
      
      phantomTemperature = as.numeric(data[rowIndex,"phantom.temperature"])
      phantomVersion = as.numeric(data[rowIndex,"phantom.version"])
      if (phantomVersion<42){
        refT1 = temperature_correction(phantomTemperature,phantomVersion)
      } else {
        refT1 = temperature_correction(phantomTemperature,phantomVersion)
      }
      
      meanSite[k,j] = mean(siteData)
      
      ##DIFERENCE BETWEEN MAGNITUDE AND COMPLEX
      #diff_Mag_Comp[k,j] = mean(magData) - mean(compData)
      #diff_Perc_Mag_Comp[k,j] = 100*abs(mean(magData) - mean(compData))/mean(magData)
      
      ##STATISTICAL TESTS (COMPARE MEANS)
      
    }
    
    id = site[j]
    sid <- as.matrix(rep(id,14))
    sph <- as.matrix(1:14)
    t1 <- as.matrix(refT1)
    ID_Site <- as.matrix(rep(labelSidSite[j],14))
    
    data_Site <- data.frame(sid, ID_Site, sph, t1, meanSite[,j])
    
    if (j==1){
      dataTmp = rbind(data.frame(), data_Site)
      if (length(site)==1){
        dataSite2plot = data_Site
      }
    }
    else{
      dataSite2plot = rbind(dataTmp, data_Site)
      dataTmp <- dataSite2plot
    }
  }
  
  ##ONE-WAY ANOVA##
  multComparisons <- list()
  if (length(site)>2){
    for (j in seq(1,14)){
      flag = 1
      anovaGer <- data.frame(T1=numeric(),group=numeric())
      
      for (k in site){
        rowIndex = match(k,as.numeric(data[,"id"]))
        if (flag==1){
          firstIndex = 0
          lastIndex = 0
        }
        sample = as.numeric(unlist(listSpheres[[rowIndex]][j]))
        lastIndex = length(sample)
        anovaGer[(1+firstIndex):(firstIndex+lastIndex),1] = sample
        anovaGer[(1+firstIndex):(firstIndex+lastIndex),2] = rep(k,length(sample))
        
        firstIndex = firstIndex + length(sample)
        flag = 0
      }
      anovaGer$group <- as.factor(anovaGer$group)
      res.aov <- aov(T1 ~ group, data = anovaGer)
      multComparisons[j] = TukeyHSD(res.aov)
    }
  }
  
  colnames(dataSite2plot) <- c('Site', 'ID_Site', 'Sphere', 'refT1', 'Mean')
  
  returnComparison <- list("dataSite" = dataSite2plot,
                           "ANOVA" = multComparisons)
  
  return(returnComparison)
}
