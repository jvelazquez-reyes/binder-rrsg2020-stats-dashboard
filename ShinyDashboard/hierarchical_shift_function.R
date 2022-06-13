hierarchical_shift_function <- function(dataSites){
  #e <- environment()
  listHSFDiff <- list()
  listHSFBootstrapDiff <- list()
  tempTitle = temperature_correction(20,42)
  for (sph in seq(14,1)){
    uniqueSites = unique(dataSites$ID_Site_long)
    listdat1 = list()
    listdat2 = list()
    lengthDAT = array()
    
    # analysis parameters
    np = length(uniqueSites) # Number of submissions of interest
    qseq <- seq(0.1,0.9,0.1) # quantiles
    alpha <- 0.05
    nboot <- 750 # bootstrap
    tr <- 0.2 # group trimmed mean for each quantile
    nq <- length(qseq) #quantile length
    icrit <- round((1-alpha)*nboot) # 95th quantile
    ilo <- round((alpha/2)*nboot)
    iup <- nboot - ilo
    ilo <- ilo + 1
    
    for (x in seq(1,length(uniqueSites))) {
      subdata = subset(dataSites, ID_Site_long == uniqueSites[x] & sph_long == sph)
      listdat1[[x]] = subdata$siteData
      listdat2[[x]] = subdata$t1_long
      lengthDAT[x] = nrow(subdata)
    }
    dat1 = stri_list2matrix(listdat1, byrow=TRUE, fill=NA)
    dat2 = stri_list2matrix(listdat2, byrow=TRUE, fill=NA)
    dat1 = `dim<-`(as.numeric(dat1), dim(dat1))
    dat2 = `dim<-`(as.numeric(dat2), dim(dat2))
    nt = max(lengthDAT)
    
    qdiff <- t(apply(dat2, 1, quantile, probs = qseq, type = 8, names = FALSE, na.rm = TRUE) - 
                 apply(dat1, 1, quantile, probs = qseq, type = 8, names = FALSE, na.rm = TRUE))
    
    gptm <- apply(qdiff, 2, mean, trim = tr)
    
    df1 <- tibble(difference = as.vector(qdiff),
                  quantile = rep(qseq, each = np),
                  site = factor(rep(unique(dataSites$ID_Site_long), nq)))
    
    df1.md <- tibble(difference = gptm,
                     quantile = qseq)
    
    hsf_colors <- setNames(viridis(length(uniqueSites)), unique(df1$site))
    p <- plotly_plot <- plot_ly(df1) %>%
      add_trace(df1, x = ~quantile, y = ~difference, color = ~site, colors = hsf_colors,
                                type = 'scatter', mode = 'markers+lines', marker = list(size = 5),
                                hoverinfo = 'text',
                                text = ~paste('<br> Decile: ', quantile,
                                              '<br> Difference (ms): ', signif(difference,4),
                                              '<br> ID: ', site)) %>%
      layout(xaxis = list(title=list(text="Deciles", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                            range=list(0,1), tickvals=seq(0.1,0.9,0.1)),
             yaxis = list(title=list(text="Reference - Measured T1 (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                            range=list(-300,300)),
             legend = list(title=list(text="<b>Site ID</b>")),
             annotations = list(x=0.5, y=250, text=paste("Reference T1: ", signif(tempTitle[sph],5), " ms"),
                                showarrow = FALSE, font = list(size=20, color="black"))) %>%
      add_trace(x = c(0, 1), y = c(0, 1),
                  type = "scatter", mode = "lines", line = list(color = 'black', width = 2), showlegend = FALSE) %>%
      add_trace(data=df1.md, x = ~quantile, y = ~difference,
                type = "scatter", mode = "markers+lines", marker = list(color = 'black', size = 10),
                line = list(color = 'black', width = 4), showlegend = FALSE)
    
    listHSFDiff[[sph]] <- p
    
    set.seed(8899)
    
    boot_data1 <- array(data = 0, dim = c(np, nt))
    boot_data2 <- array(data = 0, dim = c(np, nt))
    boot_qdiff <- array(data = 0, dim = c(nboot, nq, np))
    
    for(B in 1:nboot){
      
      # bootstrap participants
      boot_id <- sample(np, np, replace = TRUE)
      
      for(CP in 1:np){ # bootstrap trials
        boot_data1[CP,] <- sample(dat1[boot_id[CP],], nt, replace = TRUE)
        boot_data2[CP,] <- sample(dat2[boot_id[CP],], nt, replace = TRUE)
      }
      
      boot_qdiff[B,,] <- apply(boot_data2, 1, quantile, probs = qseq, type = 8, names = FALSE, na.rm = TRUE) - 
        apply(boot_data1, 1, quantile, probs = qseq, type = 8, names = FALSE, na.rm = TRUE)
      
    }
    
    boot_tm <- apply(boot_qdiff, c(1,2), mean, trim = tr)
    sort_boot_tm <- apply(boot_tm, 2, sort)
    boot_ci <- matrix(data = 0, nrow = 2, ncol = nq)
    boot_ci[1,] <- sort_boot_tm[ilo,]
    boot_ci[2,] <- sort_boot_tm[iup,]
    
    boot_hdi <- apply(boot_tm, 2, HDInterval::hdi, credMass = 1-alpha)
    
    int_to_plot <- boot_ci 
    #int_to_plot <- boot_hdi
    
    df2 <- tibble(difference = as.vector(qdiff),
                  quantile = rep(qseq, each = np),
                  site = factor(rep(unique(dataSites$ID_Site_long), nq)))
    
    df2.md <- tibble(difference = gptm,
                     quantile = qseq,
                     ymin = int_to_plot[1,],
                     ymax = int_to_plot[2,])
    
    p <- plotly_plot <- plot_ly(df2) %>%
      add_trace(df2, x = ~quantile, y = ~difference, color = ~site, colors = hsf_colors,
                type = 'scatter', mode = 'markers+lines', marker = list(size = 5),
                hoverinfo = 'text',
                text = ~paste('<br> Decile: ', quantile,
                              '<br> Difference (ms): ', signif(difference,4),
                              '<br> ID: ', site)) %>%
      layout(xaxis = list(title=list(text="Deciles", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,1), tickvals=seq(0.1,0.9,0.1)),
             yaxis = list(title=list(text="Reference - Measured T1 (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(-300,300)),
             legend = list(title=list(text="<b>Site ID</b>")),
             annotations = list(x=0.5, y=250, text=paste("Reference T1: ", signif(tempTitle[sph],5), " ms"),
                                showarrow = FALSE, font = list(size=20, color="black"))) %>%
      add_trace(x = c(0, 1), y = c(0, 1),
                type = "scatter", mode = "lines", line = list(color = 'black', width = 2), showlegend = FALSE) %>%
      add_trace(data=df2.md, x = ~quantile, y = ~difference,
                error_y = ~list(symmetric = FALSE, color = 'black', array = ymax - difference,
                                arrayminus = difference - ymin),
                type = "scatter", mode = "markers+lines", marker = list(color = 'black', size = 10),
                line = list(color = 'black', width = 4), showlegend = FALSE)
    
    listHSFBootstrapDiff[[sph]] <- p
    #p.id <- p
    
    df3 <- tibble(boot_samp = as.vector(boot_tm),
                  quantile = rep(qseq, each = nboot))

    #returnHSF <- list("diffDeciles" = df1,
    #                  "diffDecilesMD" = df1.md,
    #                  "diffBootstrap" = df2,
    #                  "diffBootstrapMD" = df2.md,
    #                  "densitiesBootstrap" = df3)
    
  }
  
  returnHSF <- list("diffDeciles" = listHSFDiff,
                    "diffBootstrapDiff" = listHSFBootstrapDiff,
                    "a"=df1,
                    "b"=df2)
  
  return(returnHSF)
}