library("Metrics")
library("ggplot2")
library("rogme")
library("tibble")
library("stringi")
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
                              htmlOutput("overviewTxt1"),
                              mainPanel(img(src='challengePitch.png', height="130%", width="130%", align = "center"))
                          ),
                          fluidPage(
                              htmlOutput("overviewTxt2"),
                              mainPanel(img(src='rrsg2020Repo.png', height="90%", width="90%", align = "center"))
                          ),
                          fluidPage(
                              htmlOutput("overviewTxt3"),
                              mainPanel(img(src='dataChallenge.png', height="90%", width="90%", align = "center"))
                          ),
                          fluidPage(
                              htmlOutput("overviewTxt4"),
                              mainPanel(img(src='generalDashboard.png', height="90%", width="90%", align = "center"))
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
                                  )
                              ),
                              
                              mainPanel(
                                  h3("Difference between Magnitude and Complex"),
                                  #h5("If site 1.001 is selected, sites 1.001 and 1.002 are actually being compared, where
                                  #1.001 - Magnitude data and 1.002 - Complex data. One more example, 6.009 - Magnitude data
                                  #   and 6.010 - Complex data."),
                                  plotlyOutput(outputId = "MagComp")
                              )
                          ),
                          shinyjs::useShinyjs(),
                          sidebarLayout(
                              sidebarPanel(
                                  radioButtons(inputId = "radButtMagComp",
                                               label = "Compare Magnitude VS Complex Per Site or All Sites",
                                               choices = c("Per Site", "All Sites"),
                                               selected = "Per Site"),
                                  
                                  selectizeInput(
                                      inputId = "CorrSitesID", 
                                      label = "Select a site to show a dispersion plot", 
                                      choices = unique(magVScomp$dataCorr$sid),
                                      multiple = FALSE
                                  ),
                                  
                                  h4("Correlation coefficients"),
                                  h6("Complete phantom"),
                                  tableOutput(outputId = "PearsonCorr"),
                                  h6("Per Sphere"),
                                  tableOutput(outputId = "PearsonMagCompPerSphere")
                              ),
                              
                              mainPanel(
                                  h3("Correlation between Magnitude and Complex"),
                                  #h5("If site 1.001 is selected, sites 1.001 and 1.002 are actually being compared, where
                                  #1.001 - Magnitude data and 1.002 - Complex data. One more example, 6.009 - Magnitude data
                                  #   and 6.010 - Complex data."),
                                  plotlyOutput(outputId = "CorrMagComp"),
                                  h3("Correlation between Magnitude and Complex per Sphere"),
                                  plotlyOutput(outputId = "DispAllPointsMagComp")
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
                                  )
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
                                              selected = "Montreal"),
                                  
                                  h2("Correlation coefficients per sphere"),
                                  tableOutput(outputId = "CorrSphSites")
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
                 tabPanel("HSF",
                          tabsetPanel(sidebarLayout(
                              sidebarPanel(width = 0.5),
                              mainPanel(
                                  h3("Bootstrapped Differences"),
                                  plotOutput(outputId = "BootstrapDiff", height = 1600, width = 1600),
                                  #plotOutput(outputId = "BootstrapDensities")
                              )
                          )
                          ),
                          
                          tabsetPanel(sidebarLayout(
                              sidebarPanel(width = 0.5),
                              mainPanel(
                                  h3("Decile Differences"),
                                  plotlyOutput(outputId = "DecilesDiff", height = 1600, width = 1600)
                              )
                          )
                          )
                 ),
                 
                 #TAB 5
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
                 
                 #TAB 6
                 tabPanel("Standard Deviation",
                          tabsetPanel(sidebarLayout(
                              sidebarPanel(
                                  selectizeInput(
                                      inputId = "RefVSMeasID", 
                                      label = "Select a group", 
                                      choices = unique(RefVSMeas$stdData$sid),
                                      selected = unique(RefVSMeas$stdData$sid),
                                      multiple = TRUE
                                  )
                                  
                              ),
                              
                              mainPanel(
                                  h3("Measurement per Site/Scanner"),
                                  plotlyOutput(outputId = "sdFilteredSites")
                              )
                          )
                          )
                ),
                
                #TAB 7
                tabPanel("Human Dataset",
                         tabsetPanel(sidebarLayout(
                             sidebarPanel(
                                 helpText("HUMAN DATASET")
                             ),
                             
                             mainPanel(
                                 h3("ALL SITES"),
                                 plotlyOutput(outputId = "boxPlotHuman")
                             )
                        )),

                         tabsetPanel(sidebarLayout(
                             sidebarPanel(
                                 selectInput(inputId = "selectMEXVendor",
                                             label = "Choose an MRI Vendor:",
                                             choices = c("Philips","GE"),
                                             selected = "Philips")
                             ),
                             
                             mainPanel(
                                 h3("Mexico MRI vendor"),
                                 plotlyOutput(outputId = "humanMEX_vendor"),
                                 h3("Mexico all"),
                                 plotlyOutput(outputId = "humanMEX_all"),
                                 h3("Canada all"),
                                 plotlyOutput(outputId = "humanCAN_all"),
                                 h3("US all"),
                                 plotlyOutput(outputId = "humanUS_all"),
                                 h3("Italy all"),
                                 plotlyOutput(outputId = "humanITA_all"),
                                 h3("Germany all"),
                                 plotlyOutput(outputId = "humanGER_all"),
                                 h3("Australia all"),
                                 plotlyOutput(outputId = "humanAUS_all"),
                                 h3("T1 value VS Age"),
                                 plotlyOutput(outputId = "humanAge_all")
                             )
                        )),
                        
                        tabsetPanel(sidebarLayout(
                            sidebarPanel(
                                selectizeInput(
                                    inputId = "FiltHumanSitesID", 
                                    label = "Select a site", 
                                    choices = unique(dfmeanHuman$Site),
                                    selected = unique(dfmeanHuman$Site),
                                    multiple = TRUE
                                )
                            ),
                            
                            mainPanel(
                                h3("Difference (%) with MEX as reference"),
                                plotlyOutput(outputId = "CompHumanSites")
                            )
                        )
                        )
                )

                #TAB 8
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
        a(HTML("<font size=6>Challenge pitch</font></b>"), href=paste("https://blog.ismrm.org/2019/12/12/reproducibility-challenge-2020-join-the-reproducible-research-and-quantitative-mr-study-groups-in-their-efforts-to-standardize-t1-mapping/"), target="_blank")
    })
    
    output$overviewTxt2 <- renderUI({
        a(HTML("<font size=6>GitHub repo</font></b>"), href=paste("https://github.com/rrsg2020/"), target="_blank")
    })
    
    output$overviewTxt3 <- renderUI({
        a(HTML("<font size=6>Dataset OSF</font></b>"), href=paste("https://osf.io/ywc9g/"), target="_blank")
    })
    
    output$overviewTxt4 <- renderUI({
        a(HTML("<font size=6>General dashboard</font></b>"), href=paste("http://rrsg2020.herokuapp.com/"), target="_blank")
    })
    
    #TAB 2
    MagCom_colors <- setNames(rainbow(length(cases)), unique(magVScomp$dataMagComp$sid))
    output$MagComp <- renderPlotly({
        if (input$typeComparison == "Difference"){
            plot_ly(magVScomp$dataMagComp, x = ~refT1, y = ~diff, split = ~as.factor(sid), color = ~as.factor(sid), colors = MagCom_colors) %>%
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
            plot_ly(magVScomp$dataMagComp, x = ~refT1, y = ~percDiff, split = ~as.factor(sid), color = ~as.factor(sid), colors = MagCom_colors) %>%
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
        p <- ggplot(data = filter(magVScomp$dataCorr, sid %in% input$CorrSitesID),
                    aes(x = Complex, y = Magnitude,
                        text = paste0('<br> Complex: ', signif(Complex,5),
                                      '<br> Magnitude: ', signif(Magnitude,5),
                                      '<br> Sphere: ', sph,
                                      '<br> ID: ', sid)),
                    color = "black", size = 1.5) +
            geom_point() +
            labs(x = "Complex T1 value (ms)", y = "Magnitude T1 value (ms)") +
            geom_smooth(aes(x = Complex, y = Magnitude), method = "lm", formula = y~x,
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
        
        ggplotly(p, tooltip = "text")
    })
    
    output$PearsonCorr <- renderTable(magVScomp$PearsonCorr)
    
    toListen <- reactive({
        list(input$radButtMagComp,input$CorrSitesID)
    })
    observeEvent(toListen(), {
        if(input$radButtMagComp=="Per Site"){
            dataMagCompSphere = comparison_magnitude_complex(cases)
            
            
            spheres = 1:14
            corr_per_sphere = reactiveValues(df = data.frame(Sphere=as.integer(), Pearson=as.numeric(), Lin=as.numeric()))
                
            DispersionAllPointsMagComp <- subset(dataMagCompSphere$PearsonCorrSphere, sid_long == input$CorrSitesID)
            for (ii in seq(1,length(spheres))){
                data_per_sphere = subset(dataMagCompSphere$PearsonCorrSphere, sid_long == input$CorrSitesID & sph_long == spheres[ii])
                corr_per_sphere$df[ii,1] = spheres[ii]
                corr_per_sphere$df[ii,2] = cor(data_per_sphere$magData,data_per_sphere$compData)
                corr_per_sphere$df[ii,3] = epi.ccc(data_per_sphere$magData,data_per_sphere$compData)[[1]][1]
            }
            
        }else if(input$radButtMagComp=="All Sites"){
            dataMagCompSphere = comparison_magnitude_complex(cases)
            DispersionAllPointsMagComp <- dataMagCompSphere$PearsonCorrSphere
            spheres = 1:14
            corr_per_sphere = reactiveValues(df = data.frame(Sphere=as.integer(), R=as.numeric(), Lin=as.numeric()))
            for (ii in seq(1,length(spheres))){
                data_per_sphere = subset(dataMagCompSphere$PearsonCorrSphere, sph_long == spheres[ii])
                corr_per_sphere$df[ii,1] = spheres[ii]
                corr_per_sphere$df[ii,2] = cor(data_per_sphere$magData,data_per_sphere$compData)
                corr_per_sphere$df[ii,3] = epi.ccc(data_per_sphere$magData,data_per_sphere$compData)[[1]][1]
            }
        }
        
        output$DispAllPointsMagComp <- renderPlotly({
            p <- ggplot(data = DispersionAllPointsMagComp,
                        aes(x = compData, y = magData,
                            text = paste('<br> Magnitude: ', signif(magData,6),
                                         '<br> Complex: ', signif(compData,6),
                                         '<br> Sphere: ', sph_long,
                                         '<br> ID: ', sid_long)),
                        color = "black", size = 1.5) +
                geom_point() +
                labs(x = "Complex T1 value (ms)", y = "Magnitude T1 value (ms)") +
                geom_smooth(aes(x = compData, y = magData), method = "lm", formula = y~x,
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
        
        output$PearsonMagCompPerSphere <- renderTable({corr_per_sphere$df})
    })

    #TAB 3
    sitesFiltered_colors <- setNames(rainbow(length(filteredSites)), unique(MeasSites$dataSite$ID_Site))
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
        
        p <- ggplot(data = DispersionAllPoints$dataSite_long,
                    aes(x = t1_long, y = siteData, fill = ID_Site_long,
                        text = paste('<br> Measured T1 Value: ', signif(siteData,6),
                                     '<br> Reference T1 Value: ', signif(t1_long,6),
                                     '<br> Sphere: ', sph_long)),
                    color = "black", size = 1.5) +
            geom_point() +
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
        
        p <- ggplot(data = AcErrorAllPoints,
                    aes(x = t1_long, y = ac_error, fill = ID_Site_long,
                        text = paste('<br> Accuracy Error: ', signif(ac_error,3),
                                     '<br> Reference T1 Value: ', signif(t1_long,6),
                                     '<br> Sphere: ', sph_long)),
                    color = "black", size = 1.5) +
            geom_point() +
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
    
    output$CorrSphSites <- renderTable({
        if (input$DispAllSite == "Montreal"){
            CorrTableSites = SiteMontreal
        }
        else if (input$DispAllSite == "Germany"){
            CorrTableSites = SiteGermany
        }
        else if (input$DispAllSite == "US"){
            CorrTableSites = SiteUS
        }
        else if (input$DispAllSite == "London"){
            CorrTableSites = SiteLondon
        }
        else if (input$DispAllSite == "Australia"){
            CorrTableSites = SiteAustralia
        }
        
        CorrTableSites$corrSph_across_sites
    })
    
    #TAB 4
    #################HSF#####################
    HSFData <- hierarchical_shift_function(dataSites)
    output$BootstrapDiff <- renderPlot({
        #HSFData <- hierarchical_shift_function(dataSites, input$DispHSF)
        g14 <- HSFData$diffBootstrapDiff[[14]]
        g13 <- HSFData$diffBootstrapDiff[[13]]
        g12 <- HSFData$diffBootstrapDiff[[12]]
        g11 <- HSFData$diffBootstrapDiff[[11]]
        g10 <- HSFData$diffBootstrapDiff[[10]]
        g9 <- HSFData$diffBootstrapDiff[[9]]
        g8 <- HSFData$diffBootstrapDiff[[8]]
        g7 <- HSFData$diffBootstrapDiff[[7]]
        g6 <- HSFData$diffBootstrapDiff[[6]]
        g5 <- HSFData$diffBootstrapDiff[[5]]
        g4 <- HSFData$diffBootstrapDiff[[4]]
        g3 <- HSFData$diffBootstrapDiff[[3]]
        g2 <- HSFData$diffBootstrapDiff[[2]]
        g1 <- HSFData$diffBootstrapDiff[[1]]
        gs = list(g14,g13,g12,g11,g10,g9,g8,g7,g6,g5,g4,g3,g2,g1)
        gridExtra::grid.arrange(grobs=gs, ncol = 4)
    })
    
    output$DecilesDiff <- renderPlotly({
        g14 <- HSFData$diffDeciles[[14]]
        g13 <- HSFData$diffDeciles[[13]]
        g12 <- HSFData$diffDeciles[[12]]
        g11 <- HSFData$diffDeciles[[11]]
        g10 <- HSFData$diffDeciles[[10]]
        g9 <- HSFData$diffDeciles[[9]]
        g8 <- HSFData$diffDeciles[[8]]
        g7 <- HSFData$diffDeciles[[7]]
        g6 <- HSFData$diffDeciles[[6]]
        g5 <- HSFData$diffDeciles[[5]]
        g4 <- HSFData$diffDeciles[[4]]
        g3 <- HSFData$diffDeciles[[3]]
        g2 <- HSFData$diffDeciles[[2]]
        g1 <- HSFData$diffDeciles[[1]]
        subplot(g14,g13,g12,g11,g10,g9,g8,g7,g6,g5,g4,g3,g2,g1, nrows = 4)
    })

    #output$BootstrapDensities <- renderPlotly({
    #    HSFData <- hierarchical_shift_function(dataSites, input$DispHSF)
    #    p <- ggplot(HSFData$densitiesBootstrap, aes(x = boot_samp, y = quantile)) +
    #        theme_classic() +
    #        stat_halfeye(#fill = "orange", 
    #            point_interval = mode_hdi,
    #            .width = c(0.5, 0.9)
    #        ) +
    #        geom_vline(xintercept = 0) +
    #        scale_y_continuous(breaks = seq(0.1,0.9,0.1)) +
    #        theme(plot.title = element_text(size=22),
    #              axis.title.x = element_text(size = 18),
    #              axis.text = element_text(size = 16, colour = "black"),
    #              axis.title.y = element_text(size = 18)) + 
    #        xlab("Bootstrap differences") +
    #        ylab("Deciles") +
    #        #coord_cartesian(xlim = c(-1, 0.1)) +
    #        coord_flip()
    #    p
    #})

    #TAB 5
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
 
        p <- ggplot(data = RefVSMeas$BAData,
                    aes(x = reference, y = measValue, fill = ID_Site,
                        text = paste('<br> Measured T1 Value: ', signif(measValue,6),
                                     '<br> Reference T1 Value: ', signif(reference,6),
                                     '<br> Sphere: ', sph)),
                    color = "black", size = 1.5) +
            geom_point() +
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
        
        p <- ggplot(data = RefVSMeas$BAData,
                    aes(x = average, y = perc_difference, fill = ID_Site,
                        text = paste('<br> Difference (%): ', signif(perc_difference,4),
                                     '<br> Average T1: ', signif(average,5),
                                     '<BR> Reference T1: ', signif(reference,5),
                                     '<br> Sphere: ', sph)), 
                    pch = 1, size = 1.5, col = "black") +
            geom_point() +
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
        
        p <- ggplot(data = RefVSMeas$BAData,
                    aes(x = reference, y = measValue, fill = ID_Vendor,
                        text = paste('<br> Measured T1 Value: ', signif(measValue,6),
                                     '<br> Reference T1 Value: ', signif(reference,6),
                                     '<br> Sphere: ', sph)),
                    color = "black", size = 1.5) +
            geom_point() +
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
        
        p <- ggplot(data = RefVSMeas$BAData,
                    aes(x = average, y = perc_difference, fill = ID_Vendor,
                        text = paste('<br> Difference (%): ', signif(perc_difference,4),
                                     '<br> Average T1: ', signif(average,5),
                                     '<BR> Reference T1: ', signif(reference,5),
                                     '<br> Sphere: ', sph)), 
                    pch = 1, size = 1.5, col = "black") +
            geom_point() +
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
    
    #TAB 6
    output$sdFilteredSites <- renderPlotly({
        sdFiltered_colors <- setNames(rainbow(length(filteredSites)), unique(sdFilteredSites$stdData$ID_Site))
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

    #TAB 7
    output$boxPlotHuman <- renderPlotly({
        p <- plot_ly(sitesHuman$dataLong_human, x = ~roi_long, y = ~siteData, split = ~roi_long,
                     type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE), points = 'all'
                     ) %>%
            add_trace(hoverinfo = 'text',
                      text = ~paste0('<br> Measured value: ', signif(siteData,5),
                                    '<br>ROI: ', roi_long,
                                    '<br>SID: ', factor(sid_long)), hoveron = 'points')
        #p <- p %>% layout(hovermode = "x unified")
            #         text = ~paste('</br> Measured value: ', signif(siteData,5),
            #                       '</br> ROI: ', roi_long,
            #                       '</br> SID: ', factor(sid_long)))
        #p <- ggplot(data = sitesHuman$dataLong_human, aes(x = roi_long, y = siteData, fill = factor(roi_long))) +
            #geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
            #geom_point(aes(y = siteData, color = factor(roi_long)),
            #           position = position_jitter(width = .15), size = .5, alpha = 0.8) +
            #geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
            #geom_boxplot(data = sitesHuman$dataLong_human, aes(x = roi_long, y = siteData, fill = factor(roi_long),
            #                                                   text = paste('<br> Measured Value: ', signif(siteData,5),
            #                                                                '<br> ROI: ', roi_long,
            #                                                                '<br> SID: ', factor(sid_long))),
            #             position = position_nudge(x=0.4),
            #             width = 0.5, color="grey", alpha=0.2) +
            #geom_jitter(data = sitesHuman$dataLong_human, aes(x = roi_long, y = siteData, fill = factor(roi_long),
            #                                                  text = paste('<br> Measured Value: ', signif(siteData,5),
            #                                                               '<br> ROI: ', roi_long,
            #                                                               '<br> SID: ', factor(sid_long))),
            #            position = position_nudge(x=0.4)) +
        #p <- ggplot(data = filter(sitesHuman$dataLong_human, sid %in% input$boxPlotSite)) +
            #labs(x = "Region of Interest (ROI)", y = "Measured T1 value (ms)", color = "SID") +
            #theme(axis.line = element_line(colour = "black"), 
            #      panel.grid.major = element_blank(), 
            #      panel.grid.minor = element_blank(), 
            #      panel.border = element_blank(), 
            #      panel.background = element_blank()) +
            #theme_classic() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            #                   axis.title = element_text(size = 12),
            #                   axis.text = element_text(size = 12))
        #ggplotly(p, tooltip = "text")
        p
    })
    
    output$humanMEX_vendor <- renderPlotly({
        if (input$selectMEXVendor == "Philips"){
            dataMEX_vendor = subset(sitesHuman_Mexico$dataLong_human, as.character(vendor_long)=="Philips")
        }
        else if (input$selectMEXVendor == "GE"){
            dataMEX_vendor = subset(sitesHuman_Mexico$dataLong_human, as.character(vendor_long)=="GE")
        }
        
        p <- ggplot(data = dataMEX_vendor, aes(x = roi_long, y = siteData, fill = factor(roi_long))) +
            geom_violin(width = 0.5) +
            geom_boxplot(width = 0.5, color="grey", alpha=0.2) +
            geom_jitter(aes(text = paste('<br> Measured Value: ', signif(siteData,5),
                                         '<br> ROI: ', roi_long,
                                         '<br> SID: ', factor(sid_long))),
                        position = position_nudge(x=0.4)) +
            #p <- ggplot(data = filter(sitesHuman$dataLong_human, sid %in% input$boxPlotSite)) +
            labs(x = "Region of Interest (ROI)", y = "Measured T1 value (ms)", color = "SID") +
            theme(axis.line = element_line(colour = "black"), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background = element_blank()) +
            theme_classic() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                                    axis.title = element_text(size = 12),
                                    axis.text = element_text(size = 12))
        ggplotly(p, tooltip = "text")
    })
    
    output$humanMEX_all <- renderPlotly({
        p <- ggplot(data = sitesHuman_Mexico$dataLong_human, aes(x = roi_long, y = siteData, fill = factor(roi_long))) +
            geom_violin(width = 0.5) +
            geom_boxplot(width = 0.5, color="grey", alpha=0.2) +
            geom_jitter(aes(text = paste('<br> Measured Value: ', signif(siteData,5),
                                         '<br> ROI: ', roi_long,
                                         '<br> SID: ', factor(sid_long))),
                        position = position_nudge(x=0.4)) +
            #p <- ggplot(data = filter(sitesHuman$dataLong_human, sid %in% input$boxPlotSite)) +
            labs(x = "Region of Interest (ROI)", y = "Measured T1 value (ms)", color = "SID") +
            #scale_x_discrete(labels = c("1"="Genu WM","2"="Splenium WM","3"="Deep GM","4"="Cortical GM")) +
            theme(axis.line = element_line(colour = "black"), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background = element_blank()) +
            theme_classic() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                                    axis.title = element_text(size = 12),
                                    axis.text = element_text(size = 12))
        ggplotly(p, tooltip = "text")
    })
    
    output$humanCAN_all <- renderPlotly({
        p <- ggplot(data = sitesHuman_Canada$dataLong_human, aes(x = roi_long, y = siteData, fill = factor(roi_long))) +
            geom_violin(width = 0.5) +
            geom_boxplot(width = 0.5, color="grey", alpha=0.2) +
            geom_jitter(aes(text = paste('<br> Measured Value: ', signif(siteData,5),
                                         '<br> ROI: ', roi_long,
                                         '<br> SID: ', factor(sid_long))),
                        position = position_nudge(x=0.4)) +
            #p <- ggplot(data = filter(sitesHuman$dataLong_human, sid %in% input$boxPlotSite)) +
            labs(x = "Region of Interest (ROI)", y = "Measured T1 value (ms)", color = "SID") +
            theme(axis.line = element_line(colour = "black"), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background = element_blank()) +
            theme_classic() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                                    axis.title = element_text(size = 12),
                                    axis.text = element_text(size = 12))
        ggplotly(p, tooltip = "text")
    })
    
    output$humanUS_all <- renderPlotly({
        p <- ggplot(data = sitesHuman_US$dataLong_human, aes(x = roi_long, y = siteData, fill = factor(roi_long))) +
            geom_violin(width = 0.5) +
            geom_boxplot(width = 0.5, color="grey", alpha=0.2) +
            geom_jitter(aes(text = paste('<br> Measured Value: ', signif(siteData,5),
                                         '<br> ROI: ', roi_long,
                                         '<br> SID: ', factor(sid_long))),
                        position = position_nudge(x=0.4)) +
            #p <- ggplot(data = filter(sitesHuman$dataLong_human, sid %in% input$boxPlotSite)) +
            labs(x = "Region of Interest (ROI)", y = "Measured T1 value (ms)", color = "SID") +
            theme(axis.line = element_line(colour = "black"), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background = element_blank()) +
            theme_classic() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                                    axis.title = element_text(size = 12),
                                    axis.text = element_text(size = 12))
        ggplotly(p, tooltip = "text")
    })
    
    output$humanITA_all <- renderPlotly({
        p <- ggplot(data = sitesHuman_Italy$dataLong_human, aes(x = roi_long, y = siteData, fill = factor(roi_long))) +
            geom_violin(width = 0.5) +
            geom_boxplot(width = 0.5, color="grey", alpha=0.2) +
            geom_jitter(aes(text = paste('<br> Measured Value: ', signif(siteData,5),
                                         '<br> ROI: ', roi_long,
                                         '<br> SID: ', factor(sid_long))),
                        position = position_nudge(x=0.4)) +
            #p <- ggplot(data = filter(sitesHuman$dataLong_human, sid %in% input$boxPlotSite)) +
            labs(x = "Region of Interest (ROI)", y = "Measured T1 value (ms)", color = "SID") +
            theme(axis.line = element_line(colour = "black"), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background = element_blank()) +
            theme_classic() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                                    axis.title = element_text(size = 12),
                                    axis.text = element_text(size = 12))
        ggplotly(p, tooltip = "text")
    })
    
    output$humanGER_all <- renderPlotly({
        p <- ggplot(data = sitesHuman_Germany$dataLong_human, aes(x = roi_long, y = siteData, fill = factor(roi_long))) +
            geom_violin(width = 0.5) +
            geom_boxplot(width = 0.5, color="grey", alpha=0.2) +
            geom_jitter(aes(text = paste('<br> Measured Value: ', signif(siteData,5),
                                         '<br> ROI: ', roi_long,
                                         '<br> SID: ', factor(sid_long))),
                        position = position_nudge(x=0.4)) +
            #p <- ggplot(data = filter(sitesHuman$dataLong_human, sid %in% input$boxPlotSite)) +
            labs(x = "Region of Interest (ROI)", y = "Measured T1 value (ms)", color = "SID") +
            theme(axis.line = element_line(colour = "black"), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background = element_blank()) +
            theme_classic() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                                    axis.title = element_text(size = 12),
                                    axis.text = element_text(size = 12))
        ggplotly(p, tooltip = "text")
    })
    
    output$humanAUS_all <- renderPlotly({
        p <- ggplot(data = sitesHuman_Australia$dataLong_human, aes(x = roi_long, y = siteData, fill = factor(roi_long))) +
            geom_violin(width = 0.5) +
            geom_boxplot(width = 0.5, color="grey", alpha=0.2) +
            geom_jitter(aes(text = paste('<br> Measured Value: ', signif(siteData,5),
                                         '<br> ROI: ', roi_long,
                                         '<br> SID: ', factor(sid_long))),
                        position = position_nudge(x=0.4)) +
            #p <- ggplot(data = filter(sitesHuman$dataLong_human, sid %in% input$boxPlotSite)) +
            labs(x = "Region of Interest (ROI)", y = "Measured T1 value (ms)", color = "SID") +
            theme(axis.line = element_line(colour = "black"), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background = element_blank()) +
            theme_classic() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                                    axis.title = element_text(size = 12),
                                    axis.text = element_text(size = 12))
        ggplotly(p, tooltip = "text")
    })
    
    sitesHuman$dataLong_human$age_long=as.numeric(as.character(sitesHuman$dataLong_human$age_long))
    output$humanAge_all <- renderPlotly({
        #sdFiltered_colors <- setNames(rainbow(nrow(sdFilteredSites$stdData)), sdFilteredSites$stdData$ID_Site)
        plot_ly(sitesHuman$dataLong_human[order(sitesHuman$dataLong_human$age_long),], x = ~age_long, y = ~siteData, split = ~roi_long) %>%
            #group_by(sid) %>%
            add_trace(type = 'scatter', mode = 'lines+markers',
                      hoverinfo = 'text',
                      text = ~paste('<br> Site: ', sid_long,
                                    '<br> ROI: ', roi_long,
                                    '<br> T1 value: ', signif(siteData,5))) %>%
            layout(xaxis = list(title = "Age (years)",categoryarray = ~names, categoryorder = "array"), yaxis = list(title = "Measured T1 value (ms)"),
                   legend = list(title = list(text = "<b>ROI</b>")))
    })
    
    sitesHuman_colors <- setNames(rainbow(length(listHumanData)), unique(dfmeanHuman$Site))
    output$CompHumanSites <- renderPlotly({
        plot_ly(dfmeanHuman, x = ~roi_lab, y = ~dif, split = ~Site, color = ~Site, colors = sitesHuman_colors) %>%
            filter(Site %in% input$FiltHumanSitesID) %>%
            #group_by(sid) %>%
            add_trace(type = 'scatter', mode = 'lines+markers',
                      hoverinfo = 'text',
                      text = ~paste('<br> Site: ', Site,
                                    '<br> Difference (%): ', signif(dif,4),
                                    '<br> ROI: ', roi_lab)) %>%
            layout(xaxis = list(title = "ROI"), yaxis = list(title = "T1 value (ms)"),
                   legend = list(title = list(text = "<b>Site ID</b>")))
    })
    
    #TAB 8
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
