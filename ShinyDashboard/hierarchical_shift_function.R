hierarchical_shift_function <- function(dataSites){
  #e <- environment()
  listHSFDiff <- list()
  listHSFBootstrapDiff <- list()
  tempTitle = temperature_correction(20,42)
  for (sph in seq(14,1)){
    a = unique(dataSites$sid_long)
    listdat1 = list()
    listdat2 = list()
    lengthDAT = array()
    
    # analysis parameters
    np = 20
    qseq <- seq(0.1,0.9,0.1) # quantiles
    alpha <- 0.05
    nboot <- 1000 # bootstrap
    tr <- 0.2 # group trimmed mean for each quantile
    nq <- length(qseq)
    icrit <- round((1-alpha)*nboot) # 95th quantile
    ilo <- round((alpha/2)*nboot)
    iup <- nboot - ilo
    ilo <- ilo + 1
    
    for (x in seq(1,length(a))) {
      subdata = subset(dataSites, sid_long == a[x] & sph_long == sph)
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
                  participants = factor(rep(seq(1:np), nq)))
    
    df1.md <- tibble(difference = gptm,
                     quantile = qseq)
    
    p <- ggplot(df1, aes(x = quantile, 
                         y = difference, 
                         colour = participants)) + 
      theme_classic() +
      geom_line(alpha = 0.5) +
      geom_abline(slope = 0, intercept = 0) +
      geom_line(data = df1.md, colour = "black", size = 1) +
      geom_point(data = df1.md, colour = "black") +
      scale_colour_viridis_d(option = "B") +
      scale_x_continuous(breaks = seq(0.1,0.9,0.1)) +
      scale_y_continuous(breaks = seq(-300,300,50)) +
      ggtitle(paste("Reference Temperature (ms): ", signif(tempTitle[sph],5))) +
      theme(legend.position = "none",
            plot.title = element_text(size=18),
            axis.title.x = element_text(size = 18),
            axis.text = element_text(size = 16, colour = "black"),
            axis.title.y = element_text(size = 18)) + 
      labs(x = "Deciles", y = "Difference")
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
                  participants = factor(rep(seq(1:np), nq)))
    
    df2.md <- tibble(difference = gptm,
                     quantile = qseq,
                     ymin = int_to_plot[1,],
                     ymax = int_to_plot[2,])
    
    p <- ggplot(df2, aes(x = quantile, 
                                           y = difference, 
                                           colour = participants)) + 
      theme_classic() +
      geom_line(alpha = 0.5) +
      geom_abline(slope = 0, intercept = 0) +
      geom_line(data = df2.md, colour = "black", size = 1) +
      # geom_point(data = df2.md, colour = "black", size = 2) +
      geom_pointrange(data = df2.md, aes(ymin = ymin, ymax = ymax), 
                      colour = "black", size = 0.75) +
      scale_colour_viridis_d(option = "B") +
      scale_x_continuous(breaks = seq(0.1,0.9,0.1)) +
      scale_y_continuous(breaks = seq(-300,300,50)) +
      # coord_cartesian(ylim = c(-500, 700)) +
      ggtitle(paste("Reference Temperature (ms): ", signif(tempTitle[sph],5))) +
      theme(legend.position = "none",
            plot.title = element_text(size=18),
            axis.title.x = element_text(size = 18),
            axis.text = element_text(size = 16, colour = "black"),
            axis.title.y = element_text(size = 18)) + 
      labs(x = "Deciles", y = "Differences")
    # coord_flip()
    # ggtitle("Non-Word - Word decile differences")
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
                    "diffBootstrapDiff" = listHSFBootstrapDiff)
  
  return(returnHSF)
}