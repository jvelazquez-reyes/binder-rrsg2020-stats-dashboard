library("Metrics")
library("ggplot2")
library("epiR")
library("rjson")
library("lme4")
library("irr")
library("sjPlot")
library("plotly")
library("shiny")
library("shinythemes")
library("shinydashboard")

source("allStats.R")

# Define UI for application that draws a histogram
ui <- navbarPage("T1 mapping challenge statistics", theme = shinytheme("flatly"),
                 
                 #TAB 1
                 tabPanel("Overview",
                          fluidPage(
                              htmlOutput("overviewTxt1")
                          ),
                          fluidPage(
                              htmlOutput("overviewTxt2")
                          ),
                          fluidPage(
                              htmlOutput("overviewTxt3")
                          ),
                          fluidPage(
                              htmlOutput("overviewTxt4")
                          )
                 ),
                 
                 #TAB 2
                 tabPanel("Magnitude VS Complex",
                          sidebarLayout(
                              sidebarPanel(
                                  selectizeInput(
                                      inputId = "DiffSitesID", 
                                      label = "Select a site", 
                                      choices = unique(magVScomp$dataMagComp$sid),
                                      selected = unique(magVScomp$dataMagComp$sid),
                                      multiple = TRUE
                                  ),
                                  
                                  radioButtons(inputId = "typeComparison",
                                               label = "Choose the type of plot to display",
                                               choices = c("Difference", "Difference (%)"),
                                               selected = "Difference (%)"
                                  ),
                                  
                                  selectizeInput(
                                      inputId = "CorrSitesID", 
                                      label = "Select a site to show a dispersion plot", 
                                      choices = unique(magVScomp$dataCorr$sid),
                                      #selected = "1.001",
                                      multiple = FALSE
                                  ),
                                  
                                  h2("Correlation coefficients"),
                                  tableOutput(outputId = "PearsonCorr"),
                                  
                              ),
                              
                              mainPanel(
                                  h3("Difference between Magnitude and Complex"),
                                  #h5("If site 1.001 is selected, sites 1.001 and 1.002 are actually being compared, where
                                  #1.001 - Magnitude data and 1.002 - Complex data. One more example, 6.009 - Magnitude data
                                  #   and 6.010 - Complex data."),
                                  plotlyOutput(outputId = "MagComp"),
                                  h3("Correlation between Magnitude and Complex"),
                                  #h5("If site 1.001 is selected, sites 1.001 and 1.002 are actually being compared, where
                                  #1.001 - Magnitude data and 1.002 - Complex data. One more example, 6.009 - Magnitude data
                                  #   and 6.010 - Complex data."),
                                  plotlyOutput(outputId = "CorrMagComp")
                              )
                          )
                 ),
                 
                 #TAB 3
                 tabPanel("Comparison across sites",
                          tabsetPanel(sidebarLayout(
                              sidebarPanel(
                                  selectizeInput(
                                      inputId = "FiltSitesID", 
                                      label = "Select a group", 
                                      choices = unique(MeasSites$dataSite$Site),
                                      selected = unique(MeasSites$dataSite$Site),
                                      multiple = TRUE
                                  ),
                                  
                              ),
                              
                              mainPanel(
                                  h3("Data per Site/Scanner"),
                                  plotlyOutput(outputId = "CompFiltSites")
                              )
                          )
                          ),
                          
                          tabsetPanel(sidebarLayout(
                              sidebarPanel(
                                  selectInput(inputId = "DispAllSite",
                                              label = "Choose a site:",
                                              choices = c("Montreal","Germany","US","London","Australia"),
                                              selected = "Montreal")
                                  
                              ),
                              
                              mainPanel(
                                  h3("Dispersion plot, pixelwise"),
                                  plotlyOutput(outputId = "DispAllPoints")
                              )
                          )
                          ),
                          
                          tabsetPanel(sidebarLayout(
                              sidebarPanel(
                                  selectInput(inputId = "AcErrorAllSite",
                                              label = "Choose a site:",
                                              choices = c("Montreal","Germany","US","London","Australia"),
                                              selected = "Montreal"),
                                  sliderInput(inputId = "errorThr",
                                              label = "Accuracy error range",
                                              min = 0, max = 50,
                                              value = c(0,50), step = 1)
                                  
                              ),
                              
                              mainPanel(
                                  h3("Accuracy Error"),
                                  plotlyOutput(outputId = "AcErrorAllPoints")
                              )
                          )
                          )
                 ),
                 
                 #TAB 4
                 navbarMenu("Measured VS Reference T1",
                            tabPanel("Site",
                                     sidebarLayout(
                                         sidebarPanel(
                                             selectInput(inputId = "selectCompSite",
                                                         label = "Choose a site:",
                                                         choices = c("Montreal","Germany","US","London","Australia"),
                                                         selected = "Montreal")
                                         ),
                                         
                                         mainPanel(
                                             h3("Bland-Altman plot"),
                                             plotlyOutput(outputId = "BA4Site")
                                         )
                                     ),
                                     sidebarLayout(
                                         sidebarPanel(
                                             h2("Correlation coefficients"),
                                             tableOutput(outputId = "CorrSites")
                                         ),
                                         mainPanel(
                                             h3("Dispersion plot"),
                                             plotlyOutput(outputId = "Disp4Site")
                                         )
                                     )
                            ),
                            
                            tabPanel("MRI vendor",
                                     sidebarLayout(
                                         sidebarPanel(
                                             selectInput(inputId = "selectCompVendor",
                                                         label = "Choose an MRI Vendor:",
                                                         choices = c("Siemens","GE","Philips"),
                                                         selected = "Siemens")
                                         ),
                                         
                                         mainPanel(
                                             h3("Bland-Altman plot"),
                                             plotlyOutput(outputId = "BA4Vendor")
                                         )
                                     ),
                                     sidebarLayout(
                                         sidebarPanel(
                                             h2("Correlation coefficients"),
                                             tableOutput(outputId = "CorrVendor")
                                         ),
                                         mainPanel(
                                             h3("Dispersion plot"),
                                             plotlyOutput(outputId = "Disp4Vendor")
                                         )
                                     )
                                     
                            )
                 ),
                 
                 #TAB 5
                 tabPanel("Standard Deviation",
                          tabsetPanel(sidebarLayout(
                              sidebarPanel(
                                  selectizeInput(
                                      inputId = "RefVSMeasID", 
                                      label = "Select a group", 
                                      choices = unique(RefVSMeas$stdData$sid),
                                      selected = unique(RefVSMeas$stdData$sid),
                                      multiple = TRUE
                                  ),
                                  
                              ),
                              
                              mainPanel(
                                  h3("Measurement per Site/Scanner"),
                                  plotlyOutput(outputId = "sdFilteredSites")
                              )
                          )
                          )
                )
                
                #TAB 6
                #tabPanel("LMEM",
                #         sidebarLayout(
                #             sidebarPanel(
                #                 selectizeInput(
                #                     inputId = "boxPlotSite", 
                #                     label = "Select a site", 
                #                     choices = unique(sitesLMEM$dataLME$sid),
                #                     #selected = "1.001",
                #                     multiple = FALSE
                #                 ),
                #                 
                #                 radioButtons(inputId = "diagnosticLME",
                #                              label = "LME Diagnostic",
                #                              choices = c("Linearity", "Normality of Residuals"),
                #                              selected = "Linearity"),
                #                 
                #                 helpText("Mathieu, B., et al. MathieuPaperName")
                #             ),
                #             
                #             mainPanel(
                #                 h3("Linear Mixed Effects Model"),
                #                 plotlyOutput(outputId = "boxPlotLME"),
                #                 h3("LME Model Summary"),
                #                 htmlOutput(outputId = "summaryLME"),
                #                 h3("Linear Mixed Effects Model Diagnostic"),
                #                 plotOutput(outputId = "diagLME")
                #             )
                #         ))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #TAB 1
    output$overviewTxt1 <- renderUI({
        a("Challenge pitch", href=paste("https://blog.ismrm.org/2019/12/12/reproducibility-challenge-2020-join-the-reproducible-research-and-quantitative-mr-study-groups-in-their-efforts-to-standardize-t1-mapping/"), target="_blank")
    })
    
    output$overviewTxt2 <- renderUI({
        a("GitHub repo", href=paste("https://github.com/rrsg2020/"), target="_blank")
    })
    
    output$overviewTxt3 <- renderUI({
        a("Link to the data", href=paste("https://osf.io/ywc9g/"), target="_blank")
    })
    
    output$overviewTxt4 <- renderUI({
        a("General dashboard", href=paste("http://rrsg2020.herokuapp.com/"), target="_blank")
    })
    
    #TAB 2
    MagCom_colors <- setNames(rainbow(nrow(magVScomp$dataMagComp)), magVScomp$dataMagComp$sid)
    output$MagComp <- renderPlotly({
        if (input$typeComparison == "Difference"){
            plot_ly(magVScomp$dataMagComp, x = ~refT1, y = ~diff, split = ~sid, color = ~sid, colors = MagCom_colors) %>%
                filter(sid %in% input$DiffSitesID) %>%
                #group_by(sid) %>%
                add_trace(type = 'scatter', mode = 'lines+markers',
                          hoverinfo = 'text',
                          text = ~paste('<br> Site: ', sid,
                                        '<br> Difference: ', signif(diff,3),
                                        '<br> Sphere: ', sph)) %>%
                layout(xaxis = list(title = "Reference T1 value (ms)"), yaxis = list(title = "Absolute difference (ms)"),
                       legend = list(title = list(text = "<b>Site ID</b>")))
        }
        else if (input$typeComparison == "Difference (%)"){
            plot_ly(magVScomp$dataMagComp, x = ~refT1, y = ~percDiff, split = ~sid, color = ~sid, colors = MagCom_colors) %>%
                filter(sid %in% input$DiffSitesID) %>%
                #group_by(sid) %>%
                add_trace(type = 'scatter', mode = 'lines+markers',
                          hoverinfo = 'text',
                          text = ~paste('<br> Site: ', sid,
                                        '<br> Difference (%): ', signif(percDiff,4),
                                        '<br> Sphere: ', sph)) %>%
                layout(xaxis = list(title = "Reference T1 value (ms)"), yaxis = list(title = "Percentage difference (%)"),
                       legend = list(title = list(text = "<b>Site ID</b>")))
        }
    })
        
    output$CorrMagComp <- renderPlotly({
        p <- ggplot(data = filter(magVScomp$dataCorr, sid %in% input$CorrSitesID)) +
            geom_point(aes(x = Complex, y = Magnitude,
                           text = paste0('<br> Complex: ', signif(Complex,5),
                                        '<br> Magnitude: ', signif(Magnitude,5),
                                        '<br> Sphere: ', sph)),
                       color = "black", size = 1.5) +
            labs(x = "Complex T1 value (ms)", y = "Magnitude T1 value (ms)") +
            geom_smooth(aes(x = Complex, y = Magnitude), method = "lm", se = TRUE, color = "red", lwd = 0.5,
                        text = paste('<br> Confidence intervals: ')) +
            geom_abline(intercept = 0, slope = 1, lwd = 0.7, col = "blue") +
            theme(axis.line = element_line(colour = "black"), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background = element_blank()) +
            theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                               axis.title = element_text(size = 12),
                               axis.text = element_text(size = 12))
        
        ggplotly(p, tooltip = "text")
    })
    
    output$PearsonCorr <- renderTable(magVScomp$PearsonCorr)
    
    #TAB 3
    sitesFiltered_colors <- setNames(rainbow(nrow(MeasSites$dataSite)), MeasSites$dataSite$ID_Site)
    output$CompFiltSites <- renderPlotly({
        plot_ly(MeasSites$dataSite, x = ~refT1, y = ~Mean, split = ~ID_Site, color = ~ID_Site, colors = sitesFiltered_colors) %>%
            filter(Site %in% input$FiltSitesID) %>%
            #group_by(sid) %>%
            add_trace(type = 'scatter', mode = 'lines+markers',
                      hoverinfo = 'text',
                      text = ~paste('<br> Site: ', Site,
                                    '<br> Measured T1: ', signif(Mean,6),
                                    '<br> Reference T1: ', signif(refT1,6),
                                    '<br> Sphere: ', Sphere)) %>%
            layout(xaxis = list(title = "Reference T1 value (ms)"), yaxis = list(title = "T1 value (ms)"),
                   legend = list(title = list(text = "<b>Site ID</b>")))
    })
    
    output$DispAllPoints <- renderPlotly({
        if (input$DispAllSite == "Montreal"){
            DispersionAllPoints = SiteMontreal
        }
        else if (input$DispAllSite == "Germany"){
            DispersionAllPoints = SiteGermany
        }
        else if (input$DispAllSite == "US"){
            DispersionAllPoints = SiteUS
        }
        else if (input$DispAllSite == "London"){
            DispersionAllPoints = SiteLondon
        }
        else if (input$DispAllSite == "Australia"){
            DispersionAllPoints = SiteAustralia
        }
        
        p <- ggplot(data = DispersionAllPoints$dataSite_long) +
            geom_point(aes(x = t1_long, y = siteData, fill = ID_Site_long,
                           text = paste('<br> Measured T1 Value: ', signif(siteData,6),
                                        '<br> Reference T1 Value: ', signif(t1_long,6),
                                        '<br> Sphere: ', sph_long)),
                       color = "black", size = 1.5) +
            labs(x = "Reference T1 value (ms)", y = "Measured T1 value (ms)") +
            geom_smooth(aes(x = t1_long, y = siteData), method = "lm", formula = y~x,
                        se = FALSE, color = "red", lwd = 0.5) +
            geom_abline(intercept = 0, slope = 1, lwd = 0.7, col = "blue") +
            theme(axis.line = element_line(colour = "black"), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background = element_blank()) +
            theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                               axis.title = element_text(size = 12),
                               axis.text = element_text(size = 12))
        p <- p + guides(fill=guide_legend(title="Site ID"))
        ggplotly(p, tooltip = "text")
    })
    
    output$AcErrorAllPoints <- renderPlotly({
        if (input$AcErrorAllSite == "Montreal"){
            AcErrorAllPoints = SiteMontreal
        }
        else if (input$AcErrorAllSite == "Germany"){
            AcErrorAllPoints = SiteGermany
        }
        else if (input$AcErrorAllSite == "US"){
            AcErrorAllPoints = SiteUS
        }
        else if (input$AcErrorAllSite == "London"){
            AcErrorAllPoints = SiteLondon
        }
        else if (input$AcErrorAllSite == "Australia"){
            AcErrorAllPoints = SiteAustralia
        }
        
        AcErrorAllPoints = subset(AcErrorAllPoints$dataSite_long, ac_error <= input$errorThr[2])
        
        p <- ggplot(data = AcErrorAllPoints) +
            geom_point(aes(x = t1_long, y = ac_error, fill = ID_Site_long,
                           text = paste('<br> Accuracy Error: ', signif(ac_error,3),
                                        '<br> Reference T1 Value: ', signif(t1_long,6),
                                        '<br> Sphere: ', sph_long)),
                       color = "black", size = 1.5) +
            labs(x = "Reference T1 value (ms)", y = "Accuracy Error (%)") +
            theme(axis.line = element_line(colour = "black"), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background = element_blank()) +
            theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                               axis.title = element_text(size = 12),
                               axis.text = element_text(size = 12))
        p <- p + guides(fill=guide_legend(title="Site ID"))
        ggplotly(p, tooltip = "text")
    })
    
    
    
    #TAB 4
    output$Disp4Site <- renderPlotly({
        if (input$selectCompSite == "Montreal"){
            RefVSMeas = measuredT1_against_referenceT1(scans = Montreal)
        }
        else if (input$selectCompSite == "Germany"){
            RefVSMeas = measuredT1_against_referenceT1(scans = Germany)
        }
        else if (input$selectCompSite == "US"){
            RefVSMeas = measuredT1_against_referenceT1(scans = US)
        }
        else if (input$selectCompSite == "London"){
            RefVSMeas = measuredT1_against_referenceT1(scans = London)
        }
        else if (input$selectCompSite == "Australia"){
            RefVSMeas = measuredT1_against_referenceT1(scans = Australia)
        }
 
        p <- ggplot(data = RefVSMeas$BAData) +
            geom_point(aes(x = reference, y = measValue, fill = ID_Site,
                           text = paste('<br> Measured T1 Value: ', signif(measValue,6),
                                        '<br> Reference T1 Value: ', signif(reference,6),
                                        '<br> Sphere: ', sph)),
                       color = "black", size = 1.5) +
            labs(x = "Reference T1 value (ms)", y = "Measured T1 value (ms)") +
            geom_smooth(aes(x = reference, y = measValue), method = "lm", formula = y~x,
                        se = FALSE, color = "red", lwd = 0.5) +
            geom_abline(intercept = 0, slope = 1, lwd = 0.7, col = "blue") +
            theme(axis.line = element_line(colour = "black"), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background = element_blank()) +
            theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                               axis.title = element_text(size = 12),
                               axis.text = element_text(size = 12))
        p <- p + guides(fill=guide_legend(title="Site ID"))
        ggplotly(p, tooltip = "text")
    })
    
    output$BA4Site <- renderPlotly({
        if (input$selectCompSite == "Montreal"){
            RefVSMeas = measuredT1_against_referenceT1(scans = Montreal)
        }
        else if (input$selectCompSite == "Germany"){
            RefVSMeas = measuredT1_against_referenceT1(scans = Germany)
        }
        else if (input$selectCompSite == "US"){
            RefVSMeas = measuredT1_against_referenceT1(scans = US)
        }
        else if (input$selectCompSite == "London"){
            RefVSMeas = measuredT1_against_referenceT1(scans = London)
        }
        else if (input$selectCompSite == "Australia"){
            RefVSMeas = measuredT1_against_referenceT1(scans = Australia)
        }
        
        p <- ggplot(data = RefVSMeas$BAData) +
            geom_point(aes(x = average, y = perc_difference, fill = ID_Site,
                           text = paste('<br> Difference (%): ', signif(perc_difference,4),
                                        '<br> Average T1: ', signif(average,5),
                                        '<BR> Reference T1: ', signif(reference,5),
                                        '<br> Sphere: ', sph)), 
                       pch = 1, size = 1.5, col = "black") +
            labs(x = "Average T1 (ms)", 
                 y = "Difference (%)") +
            xlim(200,2150) +
            ylim(-40, 40) +
            # Bias line
            geom_hline(yintercept = mean(RefVSMeas$BAData$perc_difference), lwd = 1) +
            # Line: y=0
            #geom_hline(yintercept = 0, lty = 3, col = "grey30") +
            # Limits of Agreement
            geom_hline(yintercept = mean(RefVSMeas$BAData$perc_difference) + 
                           1.96 * sd(RefVSMeas$BAData$perc_difference), 
                       lty = 2, col = "firebrick") +
            geom_hline(yintercept = mean(RefVSMeas$BAData$perc_difference) - 
                           1.96 * sd(RefVSMeas$BAData$perc_difference), 
                       lty = 2, col = "firebrick") +
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()) +
            geom_text(label = paste("Mean = ",signif(mean(RefVSMeas$BAData$perc_difference),3)), 
                      x = 1800, y = mean(RefVSMeas$BAData$perc_difference) + 2, size = 3, 
                      colour = "black") +
            geom_text(label = paste("Mean+1.96SD = ",signif(mean(RefVSMeas$BAData$perc_difference)+1.96*sd(RefVSMeas$BAData$perc_difference),3)),
                      x = 1800, y = mean(RefVSMeas$BAData$perc_difference) + 
                          1.96*sd(RefVSMeas$BAData$perc_difference) + 2,
                      size = 3, colour = "firebrick") +
            geom_text(label = paste("Mean-1.96SD = ",signif(mean(RefVSMeas$BAData$perc_difference)-1.96*sd(RefVSMeas$BAData$perc_difference),3)), 
                      x = 1800, y = mean(RefVSMeas$BAData$perc_difference) - 
                          1.96 * sd(RefVSMeas$BAData$perc_difference) - 2, 
                      size = 3, colour = "firebrick") +
            theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                               axis.title = element_text(size = 12),
                               axis.text = element_text(size = 12))
        p <- p + guides(fill=guide_legend(title="Site ID"))
        ggplotly(p, tooltip = "text")
    })
    
    output$CorrSites <- renderTable({
        if (input$selectCompSite == "Montreal"){
            RefVSMeas = measuredT1_against_referenceT1(scans = Montreal)
        }
        else if (input$selectCompSite == "Germany"){
            RefVSMeas = measuredT1_against_referenceT1(scans = Germany)
        }
        else if (input$selectCompSite == "US"){
            RefVSMeas = measuredT1_against_referenceT1(scans = US)
        }
        else if (input$selectCompSite == "London"){
            RefVSMeas = measuredT1_against_referenceT1(scans = London)
        }
        else if (input$selectCompSite == "Australia"){
            RefVSMeas = measuredT1_against_referenceT1(scans = Australia)
        }
        
        RefVSMeas$corr_coef_site
    })
    
    output$Disp4Vendor <- renderPlotly({
        if (input$selectCompVendor == "Siemens"){
            RefVSMeas = measuredT1_against_referenceT1(scans = Siemens)
        }
        else if (input$selectCompVendor == "GE"){
            RefVSMeas = measuredT1_against_referenceT1(scans = GE)
        }
        else if (input$selectCompVendor == "Philips"){
            RefVSMeas = measuredT1_against_referenceT1(scans = Philips)
        }
        
        p <- ggplot(data = RefVSMeas$BAData) +
            geom_point(aes(x = reference, y = measValue, fill = ID_Vendor,
                           text = paste('<br> Measured T1 Value: ', signif(measValue,6),
                                        '<br> Reference T1 Value: ', signif(reference,6),
                                        '<br> Sphere: ', sph)),
                       color = "black", size = 1.5) +
            labs(x = "Reference T1 value (ms)", y = "Measured T1 value (ms)") +
            geom_smooth(aes(x = reference, y = measValue), method = "lm", formula = y~x,
                        se = FALSE, color = "red", lwd = 0.5) +
            geom_abline(intercept = 0, slope = 1, lwd = 0.7, col = "blue") +
            theme(axis.line = element_line(colour = "black"), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background = element_blank()) +
            theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                               axis.title = element_text(size = 12),
                               axis.text = element_text(size = 12))
        p <- p + guides(fill=guide_legend(title="MRI Vendor"))
        ggplotly(p, tooltip = "text")
    })
    
    output$BA4Vendor <- renderPlotly({
        if (input$selectCompVendor == "Siemens"){
            RefVSMeas = measuredT1_against_referenceT1(scans = Siemens)
        }
        else if (input$selectCompVendor == "GE"){
            RefVSMeas = measuredT1_against_referenceT1(scans = GE)
        }
        else if (input$selectCompVendor == "Philips"){
            RefVSMeas = measuredT1_against_referenceT1(scans = Philips)
        }
        
        p <- ggplot(data = RefVSMeas$BAData) +
            geom_point(aes(x = average, y = perc_difference, fill = ID_Vendor,
                           text = paste('<br> Difference (%): ', signif(perc_difference,4),
                                        '<br> Average T1: ', signif(average,5),
                                        '<BR> Reference T1: ', signif(reference,5),
                                        '<br> Sphere: ', sph)), 
                       pch = 1, size = 1.5, col = "black") +
            labs(x = "Average T1 (ms)", 
                 y = "Difference (%)") +
            xlim(200,2150) +
            ylim(-35, 35) +
            # Bias line
            geom_hline(yintercept = mean(RefVSMeas$BAData$perc_difference), lwd = 1) +
            # Line: y=0
            #geom_hline(yintercept = 0, lty = 3, col = "grey30") +
            # Limits of Agreement
            geom_hline(yintercept = mean(RefVSMeas$BAData$perc_difference) + 
                           1.96 * sd(RefVSMeas$BAData$perc_difference), 
                       lty = 2, col = "firebrick") +
            geom_hline(yintercept = mean(RefVSMeas$BAData$perc_difference) - 
                           1.96 * sd(RefVSMeas$BAData$perc_difference), 
                       lty = 2, col = "firebrick") +
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()) +
            geom_text(label = paste("Mean = ",signif(mean(RefVSMeas$BAData$perc_difference),3)), 
                      x = 1800, y = mean(RefVSMeas$BAData$perc_difference) + 2, size = 3, 
                      colour = "black") +
            geom_text(label = paste("Mean+1.96SD = ",signif(mean(RefVSMeas$BAData$perc_difference)+1.96*sd(RefVSMeas$BAData$perc_difference),3)),
                      x = 1800, y = mean(RefVSMeas$BAData$perc_difference) + 
                          1.96*sd(RefVSMeas$BAData$perc_difference) + 2,
                      size = 3, colour = "firebrick") +
            geom_text(label = paste("Mean-1.96SD = ",signif(mean(RefVSMeas$BAData$perc_difference)-1.96*sd(RefVSMeas$BAData$perc_difference),3)), 
                      x = 1800, y = mean(RefVSMeas$BAData$perc_difference) - 
                          1.96 * sd(RefVSMeas$BAData$perc_difference) - 2, 
                      size = 3, colour = "firebrick") +
            theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                               axis.title = element_text(size = 12),
                               axis.text = element_text(size = 12))
        p <- p + guides(fill=guide_legend(title="MRI Vendor"))
        ggplotly(p, tooltip = "text")
    })
    
    output$CorrVendor <- renderTable({
        if (input$selectCompSite == "Siemens"){
            RefVSMeas = measuredT1_against_referenceT1(scans = Siemens)
        }
        else if (input$selectCompSite == "GE"){
            RefVSMeas = measuredT1_against_referenceT1(scans = GE)
        }
        else if (input$selectCompSite == "Philips"){
            RefVSMeas = measuredT1_against_referenceT1(scans = Philips)
        }
        
        RefVSMeas$corr_coef_site
    })
    
        #req(input$SitesID)
        #if (identical(input$SitesID, "")) return(NULL)
        #plot_ly(RefVSMeas$stdData, x = ~sph, y = ~stdValues, split = ~sid) %>%
        #    filter(sid %in% input$SitesID) %>%
        #    #group_by(sid) %>%
        #    add_lines()

    #output$multPlot <- renderPlotly({
    #    req(input$SitesID)
    #    if (identical(input$SitesID, "")) return(NULL)
    #    multPlot <- ggplot(RefVSMeas$test, aes(x = RefVSMeas$test$sph, y = RefVSMeas$test$stdValues)) +
    #        geom_line(size = 1.5) +
    #        scale_colour_manual(values = c("darkred", "blue", "dark green", "red"))
    #    ggplotly(multPlot)
    #})
    
    #TAB 5
    output$sdFilteredSites <- renderPlotly({
        sdFiltered_colors <- setNames(rainbow(nrow(sdFilteredSites$stdData)), sdFilteredSites$stdData$ID_Site)
        plot_ly(sdFilteredSites$stdData, x = ~reference, y = ~stdValues/reference, split = ~ID_Site, color = ~ID_Site, colors = sdFiltered_colors) %>%
            filter(sid %in% input$RefVSMeasID) %>%
            #group_by(sid) %>%
            add_trace(type = 'scatter', mode = 'lines+markers',
                      hoverinfo = 'text',
                      text = ~paste('<br> Site: ', sid,
                                    '<br> SD: ', signif(stdValues,3),
                                    '<br> Reference T1: ', signif(reference,6),
                                    '<br> Sphere: ', sph)) %>%
            layout(xaxis = list(title = "Reference T1 value (ms)"), yaxis = list(title = "SD/Reference T1"),
                   legend = list(title = list(text = "<b>Site ID</b>")))
    })

    #TAB 6
    #output$boxPlotLME <- renderPlotly({
    #    p <- ggplot(data = filter(sitesLMEM$dataLME, sid %in% input$boxPlotSite)) +
    #        geom_boxplot(aes(x = sphere, y = dataSphere, fill = factor(sphere))) +
    #        geom_jitter(aes(x = sphere, y = dataSphere, fill = factor(sphere),
    #                    text = paste('<br> Measured Value: ', signif(dataSphere,6),
    #                                 '<br> Reference Value: ', signif(t1ref,6),
    #                                 '<br> Sphere: ', sphere)),
    #                    position = position_nudge(x=0.4)) +
    #        labs(x = "Reference T1 value (ms)", y = "Measured T1 value (ms)", color = "Sphere") +
    #        scale_x_reverse() +
    #        scale_x_discrete(labels = c("14"="21.35","13"="30.32","12"="42.78","11"="60.06","10"="85.35",
    #                                      "9"="120.89","8"="174.70","7"="240.71","6"="341.99","5"="485.90",
    #                                      "4"="692.25","3"="994.84","2"="1342.53","1"="1911.16"))
    #        theme(axis.line = element_line(colour = "black"), 
    #              panel.grid.major = element_blank(), 
    #              panel.grid.minor = element_blank(), 
    #              panel.border = element_blank(), 
    #              panel.background = element_blank()) +
    #        theme_classic() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    #                           axis.title = element_text(size = 12),
    #                           axis.text = element_text(size = 12))
    #    ggplotly(p, tooltip = "text")
    #})
    #
    #firstLME <- lmer(dataSphere ~ t1ref + MRIversion + (1 + MRIversion|sid), data = sitesLMEM$dataLME)
    #
    #output$summaryLME <- renderUI({HTML(tab_model(firstLME, show.se = TRUE)$knitr)})
    #
    #output$diagLME <- renderPlot({
    #    if (input$diagnosticLME == "Linearity"){
    #        plot(fitted(firstLME),residuals(firstLME))
    #    }
    #    else if (input$diagnosticLME == "Normality of Residuals"){
    #        hist(residuals(firstLME))
    #    }
    #})
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
