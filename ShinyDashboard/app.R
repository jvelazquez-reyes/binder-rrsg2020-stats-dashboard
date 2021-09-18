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
library("dashboardthemes")

source("allStats.R")

### creating custom logo object
customLogo <- shinyDashboardLogoDIY(
  
  boldText = "T1 Mapping Challenge"
  ,mainText = "Themes"
  ,textSize = 16
  ,badgeText = "v1.0"
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = "#40E0D0"
  ,badgeBorderRadius = 3
  
)

### creating custom theme object
customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "rgb(23,103,124)"
  
  ,headerButtonBackColor = "rgb(238,238,238)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(238,238,238)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(20,97,117)"
    ,colorMiddle = "rgb(56,161,187)"
    ,colorEnd = "rgb(3,22,56)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

### ui
ui <- dashboardPage(
  
  ### ui header
  dashboardHeader(
    title = customLogo
 
    ),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Home", tabName = "menuHome", icon = icon("home")),
        menuItem("NIST phantom", tabName = "menuNISTphantom", icon = icon("dashboard"),
                 menuSubItem(text = "Magnitude/Complex signal", tabName = "subMagComp", icon = icon("chart-bar")),
                 menuSubItem(text = "Measurements/Reference T1", tabName = "subMeasRef", icon = icon("chart-bar"))),
        menuItem("Human brain", tabName = "menuHuman", icon = icon("brain"))
        
      )
    ),
    
    ### ui body
    dashboardBody(
      
      customTheme,
      tabItems(
        # First tab content
        tabItem(tabName = "menuHome",
                h2("Welcome to the ISMRM2020 Reproducible Challenge!"),
                
                br(),
                
                fluidRow(
                  box(width=6, status="primary",
                      htmlOutput("overviewTxt1"),
                      mainPanel(img(src='challengePitch.png', height="400%", width="150%", align = "center"))),
                  box(width=6, status="warning",
                      htmlOutput("overviewTxt2"),
                      mainPanel(img(src='rrsg2020Repo.png', height="300%", width="150%", align = "center")))
                ),
                
                fluidRow(
                  box(width=6, status="primary",
                      htmlOutput("overviewTxt3"),
                      mainPanel(img(src='dataChallenge.png', height="100%", width="150%", align = "center"))),
                  box(width=6, status="warning",
                      htmlOutput("overviewTxt4"),
                      mainPanel(img(src='generalDashboard.png', height="300%", width="150%", align = "center")))
                )
        ),
        
        # Second tab content
        tabItem(tabName = "subMagComp",
                fluidRow(
                  box(width=2, solidHeader=TRUE, status="primary",
                    selectizeInput(
                      inputId = "DiffSitesID",
                      label = "Select site",
                      choices = unique(magVScomp$dataMagComp$sid),
                      selected = unique(magVScomp$dataMagComp$sid),
                      multiple = TRUE),
                    
                    radioButtons(inputId = "typeComparison",
                                 label = "Choose the type of plot to display",
                                 choices = c("Difference", "Difference (%)"),
                                 selected = "Difference (%)"
                    )),
                  
                  box(title="Comparison Magnitude/Complex", width=5, status="warning", solidHeader=TRUE,
                      plotlyOutput(outputId = "MagComp")),
                  
                  box(title="Bland-Altman plot", width=5, status="warning", solidHeader=TRUE,
                      plotlyOutput(outputId = "BAMagComp"))
                ),
                
                fluidRow(
                  box(width=2, solidHeader=TRUE, status="primary",
                      selectizeInput(
                        inputId = "CorrSitesID",
                        label = "Select site", 
                        choices = unique(magVScomp$dataCorr$sid),
                        multiple = FALSE
                      )),
                  
                  box(title="Correlation Magnitude/Complex (mean values)", width=5, solidHeader=TRUE, status="warning",
                      plotlyOutput(outputId = "CorrMagComp")),
                  
                  box(title="Correlation coefficients Magnitude/Complex (mean values)", width=2, solidHeader=TRUE, status="warning",
                      tableOutput(outputId = "PearsonCorr"))
                  ),
                
                fluidRow(
                  box(width=2, solidHeader=TRUE, status="primary",
                      selectizeInput(
                        inputId = "CorrSitesIDallDP",
                        label = "Select site", 
                        choices = unique(magVScomp$PearsonCorrSphere$sid_long),
                        multiple = FALSE
                      )),
                  
                  box(title="Correlation Magnitude/Complex (one site - all datapoints)", width=5, solidHeader=TRUE, status="warning",
                      plotlyOutput(outputId = "DispAllPointsMagComp_oneSite")),
                  
                  box(title="Correlation coefficients Magnitude/Complex (one site - all datapoints)", width=3, solidHeader=TRUE, status="warning",
                      tableOutput(outputId = "PearsonAllPointsMagComp_oneSite"))
                ),
                
                fluidRow(
                  column(2),
                  box(title="Correlation Magnitude/Complex (all sites - all datapoints)", width=5, solidHeader=TRUE, 
                      collapsible=TRUE, status="warning",
                      plotlyOutput(outputId = "DispAllPointsMagComp_allSites")),
                  
                  box(title="Correlation coefficients Magnitude/Complex (all sites - all datapoints)", width=3, 
                      collapsible=TRUE, solidHeader=TRUE, status="warning",
                      tableOutput(outputId = "PearsonAllPointsMagComp_allSites"))
                )
        ),
        
        tabItem(tabName = "subMeasRef",
                tabBox(width = 12,
                  tabPanel(title="General",
                           fluidRow(
                             box(width=2, solidHeader=TRUE, status="primary",
                                 selectizeInput(
                                   inputId = "FiltSitesID", 
                                   label = "Select group", 
                                   choices = unique(MeasSites$dataSite$sid),
                                   selected = unique(MeasSites$dataSite$sid),
                                   multiple = TRUE)),
                             
                             box(title="Mean Measured/Reference T1 values (all sites)", width=5, solidHeader=TRUE, status="warning",
                                 plotlyOutput(outputId = "CompFiltSites")),
                             
                             box(title="Standard deviation Measured/Reference T1 (all sites)", width=5, solidHeader=TRUE, status="warning",
                                 plotlyOutput(outputId = "sdFilteredSites"))
                           ),
                           
                           fluidRow(
                             box(width=2, solidHeader=TRUE, status="primary",
                                 selectInput(inputId = "DispAllSite",
                                             label = "Choose site:",
                                             choices = c("Montreal","Germany","US","London","Australia"),
                                             selected = "Montreal")),
                             box(title="Measured/Reference T1 values (all sites - all datapoints)", width=5, solidHeader=TRUE, status="warning",
                                 plotlyOutput(outputId = "DispAllPoints"))
                           ),
                           
                           fluidRow(
                             box(width=2, solidHeader=TRUE, status="primary",
                                 selectInput(inputId = "selectCompSite",
                                             label = "Choose site:",
                                             choices = c("Montreal","Germany","US","London","Australia"),
                                             selected = "Montreal")),
                             
                             box(title="Correlation Measured/Reference T1 values (by Site)", width=5, solidHeader=TRUE, status="warning",
                                 plotlyOutput(outputId = "Disp4Site")),
                             
                             box(title="Bland-Altman plot Measured/Reference T1 (by Site)", width=5, solidHeader=TRUE, status="warning",
                                 plotlyOutput(outputId = "BA4Site"))
                           ),
                           
                           fluidRow(
                             box(width=2, solidHeader=TRUE, status="primary",
                                 selectInput(inputId = "selectCompVendor",
                                             label = "Choose MRI Vendor:",
                                             choices = c("Siemens","GE","Philips"),
                                             selected = "Siemens")),
                             
                             box(title="Correlation Measured/Reference T1 values (by MRI Vendor)", width=5, solidHeader=TRUE, status="warning",
                                 plotlyOutput(outputId = "Disp4Vendor")),
                             
                             box(title="Bland-Altman plot Measured/Reference T1 (by MRI Vendor)", width=5, solidHeader=TRUE, status="warning",
                                 plotlyOutput(outputId = "BA4Vendor"))
                           ),
                           
                           fluidRow(
                             box(width=2, solidHeader=TRUE, status="primary",
                                 selectInput(inputId = "AcErrorAllSite",
                                             label = "Choose site:",
                                             choices = c("Montreal","Germany","US","London","Australia"),
                                             selected = "Montreal"),
                                 sliderInput(inputId = "errorThr",
                                             label = "Accuracy error range",
                                             min = 0, max = 50,
                                             value = c(0,50), step = 1)),
                             
                             box(title="Accuracy Error", width=5, solidHeader=TRUE, status="warning",
                                 plotlyOutput(outputId = "AcErrorAllPoints"))
                           )
                           
                           ),
                  tabPanel(title="Hierarchical Shift Function (HSF)",
                           h2("HSF with bootstrapped confidence intervals"),
                           fluidRow(
                             box(width=4, plotlyOutput(outputId = "HSF1")),
                             box(width=4, plotlyOutput(outputId = "HSF2")),
                             box(width=4, plotlyOutput(outputId = "HSF3"))
                           ),
                           
                           fluidRow(
                             box(width=4, plotlyOutput(outputId = "HSF4")),
                             box(width=4, plotlyOutput(outputId = "HSF5")),
                             box(width=4, plotlyOutput(outputId = "HSF6"))
                           ),
                           
                           fluidRow(
                             box(width=4, plotlyOutput(outputId = "HSF7")),
                             box(width=4, plotlyOutput(outputId = "HSF8")),
                             box(width=4, plotlyOutput(outputId = "HSF9"))
                           ),
                           
                           fluidRow(
                             box(width=4, plotlyOutput(outputId = "HSF10")),
                             box(width=4, plotlyOutput(outputId = "HSF11")),
                             box(width=4, plotlyOutput(outputId = "HSF12"))
                           ),
                           
                           fluidRow(
                             box(width=4, plotlyOutput(outputId = "HSF13")),
                             box(width=4, plotlyOutput(outputId = "HSF14"))
                           )
                           )
                )
                
        ),
        
        # Third tab content
        tabItem(tabName = "menuHuman",
                h2("T1 value measured in different regions of the human brain"),
                fluidRow(
                  box(title="All Sites", width=4, solidHeader=TRUE, status="warning",
                      plotlyOutput(outputId = "boxPlotHuman")),
                  box(title="Mexico", width=4, solidHeader=TRUE, status="warning",
                      plotlyOutput(outputId = "humanMEX_all")),
                  box(title="Canada", width=4, solidHeader=TRUE, status="warning",
                      plotlyOutput(outputId = "humanCAN_all"))
                ),
                
                fluidRow(
                  box(title="United States", width=4, solidHeader=TRUE, status="warning",
                      plotlyOutput(outputId = "humanUS_all")),
                  box(title="Italy", width=4, solidHeader=TRUE, status="warning",
                      plotlyOutput(outputId = "humanITA_all")),
                  box(title="Germany", width=4, solidHeader=TRUE, status="warning",
                      plotlyOutput(outputId = "humanGER_all"))
                ),
                
                fluidRow(
                  box(title="Australia", width=4, solidHeader=TRUE, status="warning",
                      plotlyOutput(outputId = "humanAUS_all"))
                ),
                
                h2("T1 values by two vendors (MEXICO)"),
                fluidRow(
                  box(width=2, solidHeader=TRUE, status="primary",
                      selectInput(inputId = "selectMEXVendor",
                                  label = "Choose MRI Vendor:",
                                  choices = c("Philips","GE"),
                                  selected = "Philips")),
                  
                  box(title="Measured T1 values (by MRI Vendor)", width=5, solidHeader=TRUE, status="warning",
                      plotlyOutput(outputId = "humanMEX_vendor"))
                ),
                
                fluidRow(
                  box(width=2, solidHeader=TRUE, status="primary",
                      selectizeInput(
                        inputId = "FiltHumanSitesID", 
                        label = "Select site", 
                        choices = unique(dfmeanHuman$Site),
                        selected = unique(dfmeanHuman$Site),
                        multiple = TRUE)),
                  
                  box(title="Percentage difference (%) with MEX as reference", width=5, solidHeader=TRUE, status="warning",
                      plotlyOutput(outputId = "CompHumanSites"))
                ),
                
                fluidRow(
                  column(2),
                  box(title="Measured T1 value VS Age (all sites)", width=5, solidHeader=TRUE, status="warning",
                      plotlyOutput(outputId = "humanAge_all"))
                ),
                
                fluidRow(
                  column(2),
                  box(title="NIST phantom and Human data", width=5, solidHeader=TRUE, status="warning",
                      plotlyOutput(outputId = "NISTHumanStats"))
                )
        )
      )
      ### changing theme
      #shinyDashboardThemes(
      #  theme = "grey_dark"
      #)
      
      )
    #)
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  ###MENU HOME###
  output$overviewTxt1 <- renderUI({
    a(HTML("<font size=6>About the Challenge</font></b>"), href=paste("https://blog.ismrm.org/2019/12/12/reproducibility-challenge-2020-join-the-reproducible-research-and-quantitative-mr-study-groups-in-their-efforts-to-standardize-t1-mapping/"), target="_blank")
  })
  
  output$overviewTxt2 <- renderUI({
    a(HTML("<font size=6>GitHub Organization</font></b>"), href=paste("https://github.com/rrsg2020/"), target="_blank")
  })
  
  output$overviewTxt3 <- renderUI({
    a(HTML("<font size=6>Dataset OSF</font></b>"), href=paste("https://osf.io/ywc9g/"), target="_blank")
  })
  
  output$overviewTxt4 <- renderUI({
    a(HTML("<font size=6>General Dashboard</font></b>"), href=paste("http://rrsg2020.herokuapp.com/"), target="_blank")
  })
  
  ###MAGNITUDE VS COMPLEX ANALYSIS###
  MagCom_colors <- setNames(rainbow(length(cases)), unique(magVScomp$dataMagComp$sid))
  output$MagComp <- renderPlotly({
    if (input$typeComparison == "Difference"){
      plot_ly(magVScomp$dataMagComp, x = ~refT1, y = ~abs(diff), split = ~as.factor(sid), color = ~as.factor(sid), colors = MagCom_colors) %>%
        filter(sid %in% input$DiffSitesID) %>%
        #group_by(sid) %>%
        add_trace(type = 'scatter', mode = 'lines+markers',
                  hoverinfo = 'text',
                  text = ~paste('<br> Site: ', sid,
                                '<br> Difference (ms): ', signif(abs(diff),3),
                                '<br> Reference T1 (ms): ', signif(refT1,5))) %>%
        layout(xaxis = list(title=list(text="Reference T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T),
               yaxis = list(title=list(text="Absolute difference (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                            range=list(0,unname(apply(abs(magVScomp$dataMagComp),2,max))[4]+5)),
               legend = list(title=list(text="<b>Site ID</b>"), x=0.8, y=0.95))
    }
    else if (input$typeComparison == "Difference (%)"){
      plot_ly(magVScomp$dataMagComp, x = ~refT1, y = ~abs(percDiff), split = ~as.factor(sid), color = ~as.factor(sid), colors = MagCom_colors) %>%
        filter(sid %in% input$DiffSitesID) %>%
        #group_by(sid) %>%
        add_trace(type = 'scatter', mode = 'lines+markers',
                  hoverinfo = 'text',
                  text = ~paste('<br> Site: ', sid,
                                '<br> Difference (%): ', signif(abs(percDiff),4),
                                '<br> Reference T1 (ms): ', signif(refT1,5))) %>%
        layout(xaxis = list(title=list(text="Reference T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T),
               yaxis = list(title=list(text="Percentage difference (%)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                            range=list(0,unname(apply(abs(magVScomp$dataMagComp),2,max))[5]+5)),
               legend = list(title=list(text="<b>Site ID</b>"), x=0.8, y=0.95))
    }
  })
  
  output$BAMagComp <- renderPlotly({
    plot_ly(magVScomp$dataMagComp) %>%
      add_trace(magVScomp$dataMagComp, x = ~average, y = ~percDiff, color = ~as.factor(sid), 
                colors = MagCom_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Difference (%): ', signif(percDiff,4),
                              '<br> Average T1: ', signif(average,5),
                              '<BR> Reference T1: ', signif(refT1,5),
                              '<br> ID: ', sid)) %>%
      layout(xaxis = list(title=list(text="Average T1 (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,unname(apply(magVScomp$dataMagComp,2,max))[8]+100)),
             yaxis = list(title=list(text="Percentage difference (%)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(unname(apply(magVScomp$dataMagComp,2,min))[5]-20,unname(apply(magVScomp$dataMagComp,2,max))[5]+20)),
             legend = list(title=list(text="<b>Site ID</b>"))) %>%
      add_trace(x = c(0,unname(apply(magVScomp$dataMagComp,2,max))[8]+100), y = mean(magVScomp$dataMagComp$percDiff),
                type='scatter', mode = "lines", line = list(dash = "solid", width = 2, color = "black"),
                showlegend = FALSE, hoverinfo = "none") %>%
      add_trace(x = c(0,unname(apply(magVScomp$dataMagComp,2,max))[8]+100),
                y = mean(magVScomp$dataMagComp$percDiff) + 1.96*sd(magVScomp$dataMagComp$percDiff),
                type='scatter', mode = "lines", line = list(dash = "dash", color = "firebrick"),
                showlegend = FALSE, hoverinfo = "none") %>%
      add_trace(x = c(0,unname(apply(magVScomp$dataMagComp,2,max))[8]+100),
                y = mean(magVScomp$dataMagComp$percDiff) - 1.96*sd(magVScomp$dataMagComp$percDiff),
                type='scatter', mode = "lines", line = list(dash = "dash", color = "firebrick"),
                showlegend = FALSE, hoverinfo = "none") %>%
      layout(annotations = list(x=1700, y=mean(magVScomp$dataMagComp$percDiff)-10,
                                text=paste("Mean = ",signif(mean(magVScomp$dataMagComp$percDiff),3)),
                                showarrow = FALSE, font = list(size=12, color="black"))) %>%
      layout(annotations = list(x=1700, y=mean(magVScomp$dataMagComp$percDiff) + 1.96*sd(magVScomp$dataMagComp$percDiff) + 10,
                                text=paste("Mean+1.96SD = ",signif(mean(magVScomp$dataMagComp$percDiff)+1.96*sd(magVScomp$dataMagComp$percDiff),3)),
                                showarrow = FALSE, font = list(size=12, color="firebrick"))) %>%
      layout(annotations = list(x=1700, y=mean(magVScomp$dataMagComp$percDiff) - 1.96*sd(magVScomp$dataMagComp$percDiff) - 10,
                                text=paste("Mean-1.96SD = ",signif(mean(magVScomp$dataMagComp$percDiff)-1.96*sd(magVScomp$dataMagComp$percDiff),3)),
                                showarrow = FALSE, font = list(size=12, color="firebrick")))
    
  })
  
  sphere_colors <- setNames(rainbow(14), unique(signif(magVScomp$dataCorr$refT1,5)))
  output$CorrMagComp <- renderPlotly({
    plotly_plot <- plot_ly(magVScomp$dataCorr) %>%
      filter(sid %in% input$CorrSitesID) %>%
      add_trace(magVScomp$dataCorr, x = ~Complex, y = ~Magnitude, color = ~as.factor(signif(refT1,5)), 
                colors = sphere_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Complex (ms): ', signif(Complex,5),
                              '<br> Magnitude (ms): ', signif(Magnitude,5),
                              '<br> Reference T1 (ms): ', signif(refT1,5),
                              '<br> ID: ', sid)) %>%
      layout(xaxis = list(title=list(text="Complex T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,unname(apply(magVScomp$dataCorr,2,max))[5]+100)),
             yaxis = list(title=list(text="Magnitude T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,unname(apply(magVScomp$dataCorr,2,max))[4]+100)),
             legend = list(title=list(text="<b>Reference T1 (ms)</b>"))) %>%
      add_trace(x = c(0, unname(apply(magVScomp$dataCorr,2,max))[5]+100), y = c(0, unname(apply(magVScomp$dataCorr,2,max))[4]+100),
                type = "scatter", mode = "lines", line = list(color = 'blue', width = 2), showlegend = FALSE)
  })
  
  output$PearsonCorr <- renderTable(magVScomp$PearsonCorr)
  
  output$DispAllPointsMagComp_oneSite <- renderPlotly({
    plot_ly(magVScomp$PearsonCorrSphere) %>%
      filter(sid_long %in% input$CorrSitesIDallDP) %>%
      add_trace(magVScomp$PearsonCorrSphere, x = ~compData, y = ~magData, color = ~as.factor(signif(refT1_long,5)), 
                colors = sphere_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Complex (ms): ', signif(compData,5),
                              '<br> Magnitude (ms): ', signif(magData,5),
                              '<br> Reference T1 (ms): ', signif(refT1_long,5),
                              '<br> ID: ', sid_long)) %>%
      layout(xaxis = list(title=list(text="Complex T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,unname(apply(magVScomp$PearsonCorrSphere,2,max))[5]+100)),
             yaxis = list(title=list(text="Magnitude T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,unname(apply(magVScomp$PearsonCorrSphere,2,max))[4]+100)),
             legend = list(title=list(text="<b>Reference T1 (ms)</b>"))) %>%
      add_trace(x = c(0, unname(apply(magVScomp$PearsonCorrSphere,2,max))[5]+100), y = c(0, unname(apply(magVScomp$PearsonCorrSphere,2,max))[4]+100),
                type = "scatter", mode = "lines", line = list(color = 'blue', width = 2), showlegend = FALSE)
  })
  
  toListen <- reactive({input$CorrSitesIDallDP})
  observeEvent(toListen(), {
    dataMagCompSphere = comparison_magnitude_complex(cases)
    spheres = 1:14
    refT1 = temperature_correction(20,42)
    corr_per_sphere = reactiveValues(df = data.frame(Sphere=as.integer(), ReferenceT1=as.numeric(), Pearson=as.numeric(), Lin=as.numeric()))
    
    for (ii in seq(1,length(spheres))){
      data_per_sphere = subset(dataMagCompSphere$PearsonCorrSphere, sid_long == input$CorrSitesIDallDP & sph_long == spheres[ii])
      corr_per_sphere$df[ii,1] = spheres[ii]
      corr_per_sphere$df[ii,2] = signif(refT1[ii],5)
      corr_per_sphere$df[ii,3] = cor(data_per_sphere$magData,data_per_sphere$compData)
      corr_per_sphere$df[ii,4] = epi.ccc(data_per_sphere$magData,data_per_sphere$compData)[[1]][1]
    }
    
    output$PearsonAllPointsMagComp_oneSite <- renderTable({corr_per_sphere$df})
  })
  
  output$DispAllPointsMagComp_allSites <- renderPlotly({
    plot_ly(magVScomp$PearsonCorrSphere) %>%
      add_trace(magVScomp$PearsonCorrSphere, x = ~compData, y = ~magData, color = ~as.factor(signif(refT1_long,5)), 
                colors = sphere_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Complex (ms): ', signif(compData,5),
                              '<br> Magnitude (ms): ', signif(magData,5),
                              '<br> Reference T1 (ms): ', signif(refT1_long,5),
                              '<br> ID: ', sid_long)) %>%
      layout(xaxis = list(title=list(text="Complex T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,unname(apply(magVScomp$PearsonCorrSphere,2,max))[5]+100)),
             yaxis = list(title=list(text="Magnitude T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,unname(apply(magVScomp$PearsonCorrSphere,2,max))[4]+100)),
             legend = list(title=list(text="<b>Reference T1 (ms)</b>"))) %>%
      add_trace(x = c(0, unname(apply(magVScomp$PearsonCorrSphere,2,max))[5]+100), y = c(0, unname(apply(magVScomp$PearsonCorrSphere,2,max))[4]+100),
                type = "scatter", mode = "lines", line = list(color = 'blue', width = 2), showlegend = FALSE)
  })
  
  spheres = 1:14
  refT1 = temperature_correction(20,42)
  corr_per_sphere = data.frame(Sphere=as.integer(), ReferenceT1=as.numeric(), Pearson=as.numeric(), Lin=as.numeric())
  
  for (ii in seq(1,length(spheres))){
    data_per_sphere = subset(magVScomp$PearsonCorrSphere, sph_long == spheres[ii])
    corr_per_sphere[ii,1] = spheres[ii]
    corr_per_sphere[ii,2] = signif(refT1[ii],5)
    corr_per_sphere[ii,3] = cor(data_per_sphere$magData,data_per_sphere$compData)
    corr_per_sphere[ii,4] = epi.ccc(data_per_sphere$magData,data_per_sphere$compData)[[1]][1]
  }
  
  output$PearsonAllPointsMagComp_allSites <- renderTable({corr_per_sphere})
  
  ###NIST PHANTOM (MEASURED VS REFRENCE T1 ANALYSIS)###
  sitesFiltered_colors <- setNames(rainbow(length(filteredSites)), unique(MeasSites$dataSite$ID_Site))
  output$CompFiltSites <- renderPlotly({
    plot_ly(MeasSites$dataSite, x = ~refT1, y = ~Mean, split = ~ID_Site, color = ~ID_Site, colors = sitesFiltered_colors) %>%
      filter(sid %in% input$FiltSitesID) %>%
      add_trace(type = 'scatter', mode = 'lines+markers',
                hoverinfo = 'text',
                text = ~paste('<br> Site: ', sid,
                              '<br> Measured T1: ', signif(Mean,6),
                              '<br> Reference T1: ', signif(refT1,6),
                              '<br> Sphere: ', Sphere)) %>%
      layout(xaxis = list(title=list(text="Reference T1 value (ms)",font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,as.numeric(unname(apply(MeasSites$dataSite,2,max))[4])+100)),
             yaxis = list(title=list(text="Mean T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,as.numeric(unname(apply(MeasSites$dataSite,2,max))[5])+100)),
             legend = list(title=list(text="<b>Site ID</b>")))
  })
  
  output$sdFilteredSites <- renderPlotly({
    sdFiltered_colors <- setNames(rainbow(length(filteredSites)), unique(sdFilteredSites$stdData$ID_Site))
    plot_ly(sdFilteredSites$stdData, x = ~reference, y = ~stdValues/reference, split = ~ID_Site, color = ~ID_Site, colors = sdFiltered_colors) %>%
      filter(sid %in% input$FiltSitesID) %>%
      add_trace(type = 'scatter', mode = 'lines+markers',
                hoverinfo = 'text',
                text = ~paste('<br> SD/Reference T1: ', signif(stdValues/reference,3),
                              '<br> SD: ', signif(stdValues,3),
                              '<br> Reference T1: ', signif(reference,5),
                              '<br> Site: ', ID_Site)) %>%
      layout(xaxis = list(title=list(text="Reference T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T),
             yaxis = list(title=list(text="SD/Reference T1", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,0.2)),
             legend = list(title=list(text="<b>Site ID</b>")))
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
    
    specSite_colors <- setNames(rainbow(length(unique(DispersionAllPoints$dataSite_long$ID_Site_long))), unique(DispersionAllPoints$dataSite_long$ID_Site_long))
    plot_ly(DispersionAllPoints$dataSite_long) %>%
      add_trace(DispersionAllPoints$dataSite_long, x = ~t1_long, y = ~siteData, color = ~ID_Site_long,
                colors = specSite_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Measured T1 value (ms): ', signif(siteData,5),
                              '<br> Reference T1 value (ms): ', signif(t1_long,5),
                              '<br> ID: ', sid_long)) %>%
      layout(xaxis = list(title=list(text="Reference T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,as.numeric(unname(apply(DispersionAllPoints$dataSite_long,2,max))[4])+100)),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,as.numeric(unname(apply(DispersionAllPoints$dataSite_long,2,max))[5])+100)),
             legend = list(title=list(text="</b>Site ID</b>"))) %>%
      add_trace(x = c(0, as.numeric(unname(apply(DispersionAllPoints$dataSite_long,2,max))[4])+100),
                y = c(0, as.numeric(unname(apply(DispersionAllPoints$dataSite_long,2,max))[5])+100),
                type = "scatter", mode = "lines", line = list(color = 'blue', width = 2), showlegend = FALSE)
  })
  
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
    
    sitesFiltBA_colors <- setNames(rainbow(length(unique(RefVSMeas$BAData$ID_Site))), unique(RefVSMeas$BAData$ID_Site))
    plot_ly(RefVSMeas$BAData) %>%
      add_trace(RefVSMeas$BAData, x = ~reference, y = ~measValue, color = ~ID_Site, 
                colors = sitesFiltBA_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Measured T1 value (ms): ', signif(measValue,5),
                              '<br> Reference T1 value (ms): ', signif(reference,5),
                              '<br> Sphere: ', sph)) %>%
      layout(xaxis = list(title=list(text="Reference T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[6])+100)),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[5])+100)),
             legend = list(title=list(text="<b>Site ID</b>"))) %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[6])+100), y = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[5])+100),
                type = "scatter", mode = "lines", line = list(color = 'blue', width = 2), showlegend = FALSE)
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
    
    sitesFiltBA_colors <- setNames(rainbow(length(unique(RefVSMeas$BAData$ID_Site))), unique(RefVSMeas$BAData$ID_Site))
    plot_ly(RefVSMeas$BAData) %>%
      add_trace(RefVSMeas$BAData, x = ~average, y = ~perc_difference, color = ~ID_Site, 
                colors = sitesFiltBA_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Difference (%): ', signif(perc_difference,4),
                              '<br> Average T1: ', signif(average,5),
                              '<BR> Reference T1: ', signif(reference,5),
                              '<br> Sphere: ', sph)) %>%
      layout(xaxis = list(title=list(text="Average T1 (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+100)),
             yaxis = list(title=list(text="Percentage difference (%)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(-40,40)),
             legend = list(title=list(text="<b>Site ID</b>"))) %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+100), y = mean(RefVSMeas$BAData$perc_difference),
                type='scatter', mode = "lines", line = list(dash = "solid", width = 2, color = "black"),
                showlegend = FALSE, hoverinfo = "none") %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+100),
                y = mean(RefVSMeas$BAData$perc_difference) + 1.96*sd(RefVSMeas$BAData$perc_difference),
                type='scatter', mode = "lines", line = list(dash = "dash", color = "firebrick"),
                showlegend = FALSE, hoverinfo = "none") %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+100),
                y = mean(RefVSMeas$BAData$perc_difference) - 1.96*sd(RefVSMeas$BAData$perc_difference),
                type='scatter', mode = "lines", line = list(dash = "dash", color = "firebrick"),
                showlegend = FALSE, hoverinfo = "none") %>%
      layout(annotations = list(x=1700, y=mean(RefVSMeas$BAData$perc_difference)-3,
                                text=paste("Mean = ",signif(mean(RefVSMeas$BAData$perc_difference),3)),
                                showarrow = FALSE, font = list(size=12, color="black"))) %>%
      layout(annotations = list(x=1700, y=mean(RefVSMeas$BAData$perc_difference) + 1.96*sd(RefVSMeas$BAData$perc_difference) + 5,
                                text=paste("Mean+1.96SD = ",signif(mean(RefVSMeas$BAData$perc_difference)+1.96*sd(RefVSMeas$BAData$perc_difference),3)),
                                showarrow = FALSE, font = list(size=12, color="firebrick"))) %>%
      layout(annotations = list(x=1700, y=mean(RefVSMeas$BAData$perc_difference) - 1.96*sd(RefVSMeas$BAData$perc_difference) - 5,
                                text=paste("Mean-1.96SD = ",signif(mean(RefVSMeas$BAData$perc_difference)-1.96*sd(RefVSMeas$BAData$perc_difference),3)),
                                showarrow = FALSE, font = list(size=12, color="firebrick")))
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
    
    vendorFiltBA_colors <- setNames(rainbow(length(unique(RefVSMeas$BAData$ID_Vendor))), unique(RefVSMeas$BAData$ID_Vendor))
    plot_ly(RefVSMeas$BAData) %>%
      add_trace(RefVSMeas$BAData, x = ~reference, y = ~measValue, color = ~ID_Vendor, 
                colors = vendorFiltBA_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Measured T1 value (ms): ', signif(measValue,5),
                              '<br> Reference T1 value (ms): ', signif(reference,5),
                              '<br> Sphere: ', sph)) %>%
      layout(xaxis = list(title=list(text="Reference T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[6])+100)),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[5])+100)),
             legend = list(title=list(text="<b>MRI vendor</b>"))) %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[6])+100), y = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[5])+100),
                type = "scatter", mode = "lines", line = list(color = 'blue', width = 2), showlegend = FALSE)
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
    
    vendorFiltBA_colors <- setNames(rainbow(length(unique(RefVSMeas$BAData$ID_Vendor))), unique(RefVSMeas$BAData$ID_Vendor))
    plot_ly(RefVSMeas$BAData) %>%
      add_trace(RefVSMeas$BAData, x = ~average, y = ~perc_difference, color = ~ID_Vendor, 
                colors = vendorFiltBA_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Difference (%): ', signif(perc_difference,4),
                              '<br> Average T1: ', signif(average,5),
                              '<BR> Reference T1: ', signif(reference,5),
                              '<br> Sphere: ', sph)) %>%
      layout(xaxis = list(title=list(text="Average T1 (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+100)),
             yaxis = list(title=list(text="Percentage difference (%)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(-40,40)),
             legend = list(title=list(text="<b>MRI vendor</b>"))) %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+100), y = mean(RefVSMeas$BAData$perc_difference),
                type='scatter', mode = "lines", line = list(dash = "solid", width = 2, color = "black"),
                showlegend = FALSE, hoverinfo = "none") %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+100),
                y = mean(RefVSMeas$BAData$perc_difference) + 1.96*sd(RefVSMeas$BAData$perc_difference),
                type='scatter', mode = "lines", line = list(dash = "dash", color = "firebrick"),
                showlegend = FALSE, hoverinfo = "none") %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+100),
                y = mean(RefVSMeas$BAData$perc_difference) - 1.96*sd(RefVSMeas$BAData$perc_difference),
                type='scatter', mode = "lines", line = list(dash = "dash", color = "firebrick"),
                showlegend = FALSE, hoverinfo = "none") %>%
      layout(annotations = list(x=1700, y=mean(RefVSMeas$BAData$perc_difference)-3,
                                text=paste("Mean = ",signif(mean(RefVSMeas$BAData$perc_difference),3)),
                                showarrow = FALSE, font = list(size=12, color="black"))) %>%
      layout(annotations = list(x=1700, y=mean(RefVSMeas$BAData$perc_difference) + 1.96*sd(RefVSMeas$BAData$perc_difference) + 5,
                                text=paste("Mean+1.96SD = ",signif(mean(RefVSMeas$BAData$perc_difference)+1.96*sd(RefVSMeas$BAData$perc_difference),3)),
                                showarrow = FALSE, font = list(size=12, color="firebrick"))) %>%
      layout(annotations = list(x=1700, y=mean(RefVSMeas$BAData$perc_difference) - 1.96*sd(RefVSMeas$BAData$perc_difference) - 5,
                                text=paste("Mean-1.96SD = ",signif(mean(RefVSMeas$BAData$perc_difference)-1.96*sd(RefVSMeas$BAData$perc_difference),3)),
                                showarrow = FALSE, font = list(size=12, color="firebrick")))
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
    
    specSite_colors <- setNames(rainbow(length(unique(AcErrorAllPoints$ID_Site_long))), unique(AcErrorAllPoints$ID_Site_long))
    plot_ly(AcErrorAllPoints) %>%
      add_trace(AcErrorAllPoints, x = ~t1_long, y = ~ac_error, color = ~ID_Site_long,
                colors = specSite_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Accuracy Error (%): ', signif(ac_error,5),
                              '<br> Reference T1 value (ms): ', signif(t1_long,5),
                              '<br> ID: ', sid_long)) %>%
      layout(xaxis = list(title=list(text="Reference T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,as.numeric(unname(apply(AcErrorAllPoints,2,max))[4])+100)),
             yaxis = list(title=list(text="Accuracy Error (%)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,60)),
             legend = list(title=list(text="<bSite ID</b>")))
  })
  
  output$HSF1 <- renderPlotly({HSFData$diffBootstrapDiff[[14]]})
  output$HSF2 <- renderPlotly({HSFData$diffBootstrapDiff[[13]]})
  output$HSF3 <- renderPlotly({HSFData$diffBootstrapDiff[[12]]})
  output$HSF4 <- renderPlotly({HSFData$diffBootstrapDiff[[11]]})
  output$HSF5 <- renderPlotly({HSFData$diffBootstrapDiff[[10]]})
  output$HSF6 <- renderPlotly({HSFData$diffBootstrapDiff[[9]]})
  output$HSF7 <- renderPlotly({HSFData$diffBootstrapDiff[[8]]})
  output$HSF8 <- renderPlotly({HSFData$diffBootstrapDiff[[7]]})
  output$HSF9 <- renderPlotly({HSFData$diffBootstrapDiff[[6]]})
  output$HSF10 <- renderPlotly({HSFData$diffBootstrapDiff[[5]]})
  output$HSF11 <- renderPlotly({HSFData$diffBootstrapDiff[[4]]})
  output$HSF12 <- renderPlotly({HSFData$diffBootstrapDiff[[3]]})
  output$HSF13 <- renderPlotly({HSFData$diffBootstrapDiff[[2]]})
  output$HSF14 <- renderPlotly({HSFData$diffBootstrapDiff[[1]]})
  
  ###HUMAN DATASET ANALYSIS###
  output$boxPlotHuman <- renderPlotly({
    sitesHuman$dataLong_human$roi_long <- factor(sitesHuman$dataLong_human$roi_long,
                                                 c("Genu WM", "splenium WM", "Deep GM", "Cortical GM"))
    plot_ly(sitesHuman$dataLong_human, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(as.numeric(unname(apply(sitesHuman$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>")))
  })
  
  output$humanMEX_all <- renderPlotly({
    plot_ly(sitesHuman_Mexico$dataLong_human, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(as.numeric(unname(apply(sitesHuman$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>")))
  })
  
  output$humanCAN_all <- renderPlotly({
    plot_ly(sitesHuman_Canada$dataLong_human, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(as.numeric(unname(apply(sitesHuman$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>")))
  })
  
  output$humanUS_all <- renderPlotly({
    sitesHuman_US$dataLong_human$roi_long <- factor(sitesHuman_US$dataLong_human$roi_long,
                                                 c("Genu WM", "splenium WM", "Deep GM", "Cortical GM"))
    plot_ly(sitesHuman_US$dataLong_human, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(as.numeric(unname(apply(sitesHuman$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>")))
  })
  
  output$humanITA_all <- renderPlotly({
    plot_ly(sitesHuman_Italy$dataLong_human, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(as.numeric(unname(apply(sitesHuman$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>")))
  })
  
  output$humanGER_all <- renderPlotly({
    plot_ly(sitesHuman_Germany$dataLong_human, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(as.numeric(unname(apply(sitesHuman$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>")))
  })
  
  output$humanAUS_all <- renderPlotly({
    plot_ly(sitesHuman_Australia$dataLong_human, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(as.numeric(unname(apply(sitesHuman$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>")))
  })
  
  output$humanMEX_vendor <- renderPlotly({
    if (input$selectMEXVendor == "Philips"){
      dataMEX_vendor = subset(sitesHuman_Mexico$dataLong_human, as.character(vendor_long)=="Philips")
    }
    else if (input$selectMEXVendor == "GE"){
      dataMEX_vendor = subset(sitesHuman_Mexico$dataLong_human, as.character(vendor_long)=="GE")
    }
    
    plot_ly(dataMEX_vendor, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(as.numeric(unname(apply(sitesHuman_Mexico$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman_Mexico$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>")))
  })
  
  sitesHuman_colors <- setNames(rainbow(length(listHumanData)), unique(dfmeanHuman$Site))
  output$CompHumanSites <- renderPlotly({
    plot_ly(dfmeanHuman, x = ~roi_lab, y = ~dif, split = ~Site, color = ~Site, colors = sitesHuman_colors) %>%
      filter(Site %in% input$FiltHumanSitesID) %>%
      add_trace(type = 'scatter', mode = 'lines+markers',
                hoverinfo = 'text',
                text = ~paste('<br> Site: ', Site,
                              '<br> Difference (%): ', signif(dif,4),
                              '<br> ROI: ', roi_lab)) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T),
             yaxis = list(title=list(text="Percentage difference (%)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(-30,70)),
             legend = list(title=list(text="<b>Site ID</b>")))
  })
  
  sitesHuman$dataLong_human$age_long=as.numeric(as.character(sitesHuman$dataLong_human$age_long))
  output$humanAge_all <- renderPlotly({
    #sdFiltered_colors <- setNames(rainbow(nrow(sdFilteredSites$stdData)), sdFilteredSites$stdData$ID_Site)
    plot_ly(sitesHuman$dataLong_human[order(sitesHuman$dataLong_human$age_long),], x = ~age_long, y = ~siteData, split = ~roi_long) %>%
      add_trace(type = 'scatter', mode = 'markers',
                hoverinfo = 'text',
                text = ~paste('<br> Site: ', sid_long,
                              '<br> ROI: ', roi_long,
                              '<br> T1 value: ', signif(siteData,5))) %>%
      layout(xaxis = list(title=list(text="Age (years)", font=list(size=18)), categoryarray = ~names, categoryorder = "array", tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>")))
    
  })
  
  #covs_colors <- setNames(rainbow(12), unique(compNISTHuman$Site))
  output$NISTHumanStats <- renderPlotly({
    plot_ly(compNISTHuman) %>%
      add_trace(compNISTHuman, x = ~Mean, y = ~Std, color = ~as.factor(NPHuman),
                colors = c('blue','red'), type = 'scatter', mode = 'markers', marker = list(size = ~Site),
                hoverinfo = 'text',
                text = ~paste('<br> Sigma (ms): ', signif(Std,4),
                              '<br> Mean (ms): ', signif(Mean,5),
                              '<br> CoV (%): ', signif(100*Std/Mean,4),
                              '<br> Reference T1 (ms)/ROI: ', t1ROI,
                              '<br> Site ID: ', Site)) %>%
      layout(xaxis = list(title=list(text="Mean value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,as.numeric(unname(apply(compNISTHuman,2,max))[3])+100)),
             yaxis = list(title=list(text="Sigma value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T,
                          range=list(0,as.numeric(unname(apply(compNISTHuman,2,max))[4])+100)),
             legend = list(title=list(text="<b>Site ID</b>")))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)