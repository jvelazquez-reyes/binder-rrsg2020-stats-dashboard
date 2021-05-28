hierarchical_shift_function <- function(dataSites){
  #e <- environment()
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
    subdata = subset(dataSites, sid_long == a[x] & sph_long == input$DispHSF)
    listdat1[[x]] = subdata$siteData
    listdat2[[x]] = subdata$t1_long
    lengthDAT[x] = nrow(subdata)
  }
  dat1 = stri_list2matrix(listdat1, byrow=TRUE, fill=NA)
  dat2 = stri_list2matrix(listdat2, byrow=TRUE, fill=NA)
  dat1 = `dim<-`(as.numeric(dat1), dim(dat1))
  dat2 = `dim<-`(as.numeric(dat2), dim(dat2))
  nt = max(lengthDAT)
  
  qdiff <- t(apply(dat1, 1, quantile, probs = qseq, type = 8, names = FALSE, na.rm = TRUE) - 
               apply(dat2, 1, quantile, probs = qseq, type = 8, names = FALSE, na.rm = TRUE))
  
  gptm <- apply(qdiff, 2, mean, trim = tr)
  
  df1 <- tibble(difference = as.vector(qdiff),
                quantile = rep(qseq, each = np),
                participants = factor(rep(seq(1:np), nq)))
  
  df1.md <- tibble(difference = gptm,
                   quantile = qseq)
  
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
    
    boot_qdiff[B,,] <- apply(boot_data1, 1, quantile, probs = qseq, type = 8, names = FALSE, na.rm = TRUE) - 
      apply(boot_data2, 1, quantile, probs = qseq, type = 8, names = FALSE, na.rm = TRUE)
    
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
  
  df3 <- tibble(boot_samp = as.vector(boot_tm),
                quantile = rep(qseq, each = nboot))
  
  returnHSF <- list("diffDeciles" = df1,
                    "diffDecilesMD" = df1.md,
                    "diffBootstrap" = df2,
                    "diffBootstrapMD" = df2.md,
                    "densitiesBootstrap" = df3)
}