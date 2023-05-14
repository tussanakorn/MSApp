## app.R ##
library(shinydashboard)
library(shiny)
library(rgdal)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)
library(plotly)
library(shinyWidgets)
library(mapview)
library(spdep)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(rgeos)
library(mapproj)
library(htmlwidgets)
library(rmarkdown)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tigris)
# library(INLA)
library(sp)
library(geodata)
library(shinycssloaders)
library(ggthemes)
library(bsplus)
library(shinyBS)
library(shinythemes)
library(maptools)
library("maptools")


# By default the file size limit is 5MB. Here limit is 70MB.
options(shiny.maxRequestSize = 70*1024^2)
# Increase memory limit
# memory.size(max = FALSE)

options(encoding="utf-8")
Sys.setlocale(category = "LC_ALL", locale = "Thai")

# Options for Spinner
options(spinner.color="red")

# load("D:/Desktop/Year 4/Sr project MU PSCM/Model/results/MODELS_inla_icar_test.RData")
# Model <- MODELS.inla.icar.test$TypeI_RW1
load("results/MODELS_icar_t1_5f.RData")
load("results/MODELS_inla_icar_prediction.RData")


# load("C:/Users/tussa/OneDrive/Documents/MSApp/results/MODELS_icar_t1.RData")
Model_cluster <- icar.t1.1.f$cluster_model
Model_association <- icar.t1.1.f$association_model
Model_prediction <- MODELS.inla.icar.prediction

ui <- dashboardPage(
  
  
  skin = "red",
  dashboardHeader(title = "MSApp",titleWidth = 300),
  
  ## Sidebar content
  dashboardSidebar(width = 300,
                   sidebarMenu(id = "menu",
                               HTML(paste0(
                                 "<br>",
                                 "<a target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='MSAppLogo.png' width = '200'></a>",
                                 "<br>",
                                 "<p style = 'text-align: center;'><small><a href='https://www.nps.gov/subjects/hfc/arrowhead-artwork.htm' target='_blank'>Multivariate Spatio-temporal Analysis</a></small></p>",
                                 "<br>"
                               )),
                               menuItem("Home", tabName = "home", icon=icon("home"), selected=TRUE),
                               menuItem("Data import", tabName = "import", icon=icon("table")),
                               menuItem("Data explore", tabName = "explore", icon = icon("line-chart")),
                               menuItem("Analysis", tabName = "analysis", icon = icon("map")),
                               menuItem("ReadMe", tabName = "readme", icon = icon("readme")),
                               menuItem("About", tabName = "about", icon = icon("user", lib = "glyphicon")),
                               # menuItem("Releases", tabName = "releases", icon = icon("tasks")),
                               HTML(paste0(
                                 "<br><br>",
                                 "<table style='margin-left:auto; margin-right:auto;'>",
                                 "<tr>",
                                 # "<td style='padding: 5px;'><a href='https://www.youtube.com/nationalparkservice' target='_blank'><i class='fab fa-youtube fa-lg'></i></a></td>",
                                 # "<td style='padding: 5px;'><a href='https://www.flickr.com/nationalparkservice' target='_blank'><i class='fab fa-github fa-lg'></i></a></td>",
                                 # "</tr>",
                                 "</table>",
                                 "<br>")
                               ),
                               HTML(paste0(
                                 "<script>",
                                 "var today = new Date();",
                                 "var yyyy = today.getFullYear();",
                                 "</script>",
                                 "<p style = 'text-align: center;'><small>&copy; - <a href='https://MSApp.com' target='_blank'>MSApp.com</a> - <script>document.write(yyyy);</script></small></p>")
                               )
                   )
  ),
  ## Body content
  dashboardBody(
    tags$head(
      tags$style("
                  .main-sidebar { font-size: 17px!important; }
                  .treeview-menu>li>a { font-size: 12px!important; }
                  .progress-bar{ background-color:#3c763d; }
                  .nav-tabs-custom .nav-tabs li.active {border-top-color: #d73925;}
                  
                 "
      )
    ),
    
    tabItems(
      
      # First tab content
      tabItem(tabName = "home",
              includeMarkdown("www/home_page.md"),
              br(),
              div(align="right",actionButton("next1", "Get Started now", class = "btn-danger",style="color: #fff"))
      ), # End First tab content
      
      # Second tab content
      tabItem(tabName = "import",
              fluidRow(
                column(
                  width = 12,
                  box(
                    width = 6, title = "1. Upload map",
                    helpText("Upload all map files at once: shp, dbf, shx and prj."),
                    status = "danger", solidHeader = FALSE,
                    fileInput(inputId = "filemap", label = "", accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE),
                    # helpText("Select columns id and name of the areas in the map."),
                    
                    # fluidRow(
                    #   column(6, selectInput("columnidareainmap",   label = "area id",   choices = c(""), selected = "")),
                    #   column(6, selectInput("columnnameareainmap", label = "area name", choices = c(""), selected = ""))
                    #   ),
                    fluidRow(
                      column(
                        12,
                        HTML(paste0(
                          "You can select and download free geographic data (shapefile) for any country in the world from ",
                          a(icon("cloud-download-alt"), href = "http://www.diva-gis.org/gdata", style = "color:red;", target = "_blank")
                        ))
                      )
                    )
                  ),
                  box(
                    width = 6, title="2. Upload data (.csv file)", status = "danger", solidHeader = FALSE,
                    helpText("Data (.csv file) must include area, time, OBS, EXP, pop, type, and covariate columns."),
                    fileInput(inputId = "filedata",
                              label = "Choose csv file",
                              accept = c(".csv")
                    ),
                    # helpText("Select columns id, date, population and cases in the data."),
                    # fluidRow(column(6, selectInput("columnnameareaindata",  label = "area name",  choices = c(""), selected = "")),
                    #          column(6, selectInput("columndateindata", label = "date", choices = c(""), selected = ""))),
                    # fluidRow(column(6, selectInput("columnpopindata", label = "population", choices = c(""), selected = "")),
                    #          column(6, selectInput("columncasesindata", label = "cases", choices = c(""), selected = "")))
                  )
                )
              ),
              fluidRow(column(12,"   Data analysis was calculated using the Multivariate Intrinsic Conditional Auto-Regressive (M-ICAR) model."),
                       br(),
                       br(),
                       br(),
                       column(3, offset=3, actionButton("startAnalysisButton",strong( "Start analysis"), style='background-color: #F1A8A8')),
                       column(3, checkboxInput("useSampleData", "Use sample data", FALSE))
                       # column(4, actionButton("restart", "Reset data"))
              ),
              fluidRow(style = "margin:10px",
                       h3("Contents map"),
                       leafletOutput("mapmap"),
                       verbatimTextOutput("uploadmapsummary"),
                       div(dataTableOutput('uploadmaptable'),  style = "font-size:80%")),
              fluidRow(style = "margin:10px",
                       h3("Contents data"),
                       verbatimTextOutput("uploaddatasummary"),
                       div(dataTableOutput('uploaddatatable'),  style = "font-size:80%")),
              fluidRow(
                column(
                  width = 12,
                  style = "display: flex; padding:30px; top: -30px",
                  div(style = "flex: 50%", align="left",actionButton("previous2", "<<< Previous", class = "btn-danger",style="color: #fff", )),
                  div(style = "flex: 50%", align="right",actionButton("next2", "Next >>>", class = "btn-danger",style="color: #fff"))
                )
              )
      ), # End Second tab content
      
      
      
      # Third tab content
      tabItem(tabName = "explore",
              includeMarkdown("www/explore_page.md"),
              "Use the dropdown menus below to filter the map by area, type and time",
              fluidRow(
                # setSliderColor(c("red ", "#FF4500", "", "Teal"), c(1, 2, 4)),
                br(),
                column(3, selectInput(inputId = "areafilter", label = "Select area", choices = c(""), selected = "")
                       %>%
                         shinyInput_label_embed(
                           icon("info") %>%
                             bs_embed_tooltip(title = "Apply filters only to Time plots")
                         ),
                       h4(div("Select area > apply filters only to Time plots", style = "color: grey;"))
                ),
                column(3, selectInput(inputId = "typefilter", label = "Select type", choices = c(""), selected = "")
                       %>%
                         shinyInput_label_embed(
                           icon("info") %>%
                             bs_embed_tooltip(title = "Apply filters for both Map plots and Time plots")
                         ),
                       h4(div("Select type > apply filters for both Map plots and Time plots", style = "color: grey;"))
                ),
                column(3, selectInput(inputId = "timefilter", label = "Select time", choices = c(""), selected = "")
                       %>%
                         shinyInput_label_embed(
                           icon("info") %>%
                             bs_embed_tooltip(title = "Apply filters only to Map plots")
                         ),
                       h4(div("Select time > apply filters only to Map plots", style = "color: grey;"))
                )
              ),
              br(),
              fluidRow(
                column(6, tags$h3("Map of Observed Cases"),
                       h4(div("Explore the map by", strong("zooming in and out"), "and", strong("hover and click"), "to find out more information for a given geography.", style = "color: grey;")),
                       leafletOutput('map_obs') %>% withSpinner(color = "red")),
                column(6, tags$h3("Time plots of Observed Cases"), 
                       h4(div("Explore the time plot by", strong("hover and click"), "to find out more information at the provincial level for each time period.", style = "color: grey;")),
                       plotlyOutput("plotline_obs") %>% withSpinner(color = "red"))
              ),
              br(),
              br(),
              fluidRow(
                column(6, tags$h3("Map of Standard Morbidity Ratio (SMR)"),
                       tags$ul(
                         tags$li("Standard Morbidity Ratio (SMR) is calculated as Observed Cases divided by Expected Cases.", style = "color: grey;")
                       ),
                       h4(div("Explore the map by", strong("zooming in and out"), "and", strong("hover and click"), "to find out more information for a given geography.", style = "color: grey;")),
                       br(),
                       leafletOutput("map_smr") %>% withSpinner(color = "red")),
                column(6, tags$h3("Time plots of Standard Morbidity Ratio (SMR)"),
                       tags$ul(
                         tags$li("Standard Morbidity Ratio (SMR) is calculated as Observed Cases divided by Expected Cases.", style = "color: grey;")
                       ),
                       h4(div("Explore the time plot by", strong("hover and click"), "to find out more information at the provincial level for each time period.", style = "color: grey;")),
                       br(),
                       plotlyOutput("plotline_smr") %>% withSpinner(color = "red"))
              ),
              fluidRow(
                column(
                  width = 12,
                  style = "display: flex; padding:30px; top: -30px",
                  div(style = "flex: 50%", align="left",actionButton("previous3", "<<< Previous", class = "btn-danger",style="color: #fff", )),
                  div(style = "flex: 50%", align="right",actionButton("next3", "Next >>>", class = "btn-danger",style="color: #fff"))
                )
              )
      ), # End Third tab content
      
      
      
      # Fourth tab content
      tabItem(tabName = "analysis",
              includeMarkdown("www/analysis_page.md"),
              
              fluidRow(
                column(width = 12,
                       tabsetPanel(
                         id = "tabset1",
                         tabPanel(tags$h4("Clusters", style = "color:black;"),
                                  br(),
                                  # setSliderColor(c("red ", "#FF4500", "", "Teal"), c(1, 2, 4)),
                                  column(3, selectInput(inputId = "typefilter1", label = "Select type", choices = c(""), selected = "")),
                                  column(3, selectInput(inputId = "timefilter1", label = "Select time", choices = c(""), selected = "")),
                                  column(3),
                                  column(3,actionButton("cluster_helpButton", "How to interpret Cluster detection results?")),
                                  br(),
                                  column(12,
                                         withSpinner(leafletOutput("map_detectcluster", width = "100%", height = "700px"), type = 8)
                                  )
                         ),
                         tabPanel(tags$h4("Associations", style = "color:black;"),
                                  br(),
                                  column(3, selectInput(inputId = "typefilter2", label = "Select type", choices = c(""), selected = "")),
                                  column(3, selectInput(inputId = "columnfactordata",  label = "Select factor",  choices = c(""), selected = "")),
                                  column(3),
                                  column(3,actionButton("association_helpButton", "How to interpret associations results?")),
                                  br(),
                                  # fluidRow(
                                  #   column(6, withSpinner(DT::dataTableOutput("association_table"), type = 8)),
                                  #   column(6, withSpinner(leafletOutput("map_association", width = "100%", height = "700px"), type = 8))
                                  # )
                                  column(12,
                                         withSpinner(DT::dataTableOutput("association_table"), type = 8)
                                  ),
                                  column(12,
                                         withSpinner(leafletOutput("map_association", width = "100%", height = "700px"), type = 8)
                                  )
                         ),
                         tabPanel(tags$h4("Predictions", style = "color:black;"),
                                  br(),
                                  column(3, selectInput(inputId = "typefilter3", label = "Select type", choices = c(""), selected = "")),
                                  column(3, selectInput(inputId = "areafilter1", label = "Select area", choices = c(""), selected = "")),
                                  br(),
                                  column(12,
                                         withSpinner(plotlyOutput("lineplot_prediction", width = "100%", height = "700px"), type = 8)
                                  )
                         ),
                       )
                )
              ),
              HTML("<br>"),
              fluidRow(
                column(
                  width = 12,
                  style = "display: flex; padding:30px; top: -30px",
                  div(style = "flex: 50%", align="left",actionButton("previous4", "<<< Previous", class = "btn-danger",style="color: #fff", ))
                  # div(style = "flex: 50%", align="right",actionButton("next3", "Next >>>", class = "btn-danger",style="color: #fff"))
                )
              )
      ), # End Fourth tab content
      tabItem(tabName = "readme", includeMarkdown("www/readme.md")),
      tabItem(tabName = "about", includeMarkdown("www/about.md"))
      # tabItem(tabName = "releases", includeMarkdown("www/releases.md")),
    )
  )
)

server <- function(input, output, session) {
  
  # Restart Shiny session
  observeEvent(input$restart, {
    session$reload()
  })
  
  # Get started page
  observeEvent(input$next1, {
    updateTabItems(session, "menu", "import") 
  })
  
  # Data import page
  observeEvent(input$previous2, {
    updateTabItems(session, "menu", "home") 
  })
  
  observeEvent(input$next2, {
    updateTabItems(session, "menu", "explore") 
  })
  
  # Data explore page
  observeEvent(input$previous3, {
    updateTabItems(session, "menu", "import") 
  })
  
  observeEvent(input$next3, {
    updateTabItems(session, "menu", "analysis") 
  })
  
  # Data analysis page
  observeEvent(input$previous4, {
    updateTabItems(session, "menu", "explore") 
  })
  
  observeEvent(input$next4, {
    updateTabItems(session, "menu", "analysis") 
  })
  
  
  observe({
    if (is.null(names(rv$datosOriginal)))
      xd <- character(0)
    
    xd<-names(rv$datosOriginal)
    xd2 <- xd[!(names(rv$datosOriginal) %in% c("area", "time", "OBS", "EXP", "pop", "type"))]
    
    if (is.null(xd))
      xd <- character(0)
    
    # xd2<- c("-", xd2)
    
    # Can also set the label and select items
    #label = paste("Select input label", length(x)),
    updateSelectInput(session, "columnnameareaindata", choices = xd, selected = head(xd, 1))
    updateSelectInput(session, "columndateindata",   choices = xd, selected = head(xd, 1))
    updateSelectInput(session, "columnpopindata",    choices = xd, selected = head(xd, 1))
    updateSelectInput(session, "columncasesindata",  choices = xd, selected = head(xd, 1))
    updateSelectInput(session, "columnfactordata",  choices = xd2, selected = head(xd2, 1))
  })
  
  observe({
    x <- names(rv$map)
    xd<-c("-",x)
    # Can use character(0) to remove all choices
    if (is.null(x)){
      x <- character(0)
      xd<-x
    }
    
    # updateSelectInput(session, "columnidareainmap", choices = x,  selected = head(x, 1))
    updateSelectInput(session, "columnnameareainmap", choices = x,  selected = head(x, 1))
  })
  
  # Upload shapefile
  observe({
    shpdf <- input$filemap
    if(is.null(shpdf)){
      return()
    }
    previouswd <- getwd()
    uploaddirectory <- dirname(shpdf$datapath[1])
    setwd(uploaddirectory)
    for(i in 1:nrow(shpdf)){
      file.rename(shpdf$datapath[i], shpdf$name[i])
    }
    setwd(previouswd)
    
    #map <- readShapePoly(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp", shpdf$name)], sep="/"),  delete_null_obj=TRUE)
    #reads the file that finishes with .shp using $ at the end: grep(pattern="*.shp$", shpdf$name)
    map <- readOGR(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/"))#,  delete_null_obj=TRUE)
    map <- spTransform(map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    rv$map<-map
  })
  
  rv <- reactiveValues(
    columnidareainmap=NULL,  columnnameareainmap=NULL, columnnamesuperareainmap=NULL,
    idpolyhighlighted = NULL, posinmapFilteredIdpolyhighlighted=NULL, colores=NULL,
    minrisk=0, maxrisk=1,
    vblePintar="Risk", textareareactive="NULL",messageCheckDataText="",
    map=NULL,datosOriginal=NULL,
    datoswithvaluesforeachidandtime=NULL,
    datossatscan=NULL,
    lastselectstage=NULL,
    usedcovs=NULL,
    usedarealcovs=NULL,
    selectstage='stageuploaddata')
  
  ## Upload data
  observe({
    inFile <- input$filedata
    if (is.null(inFile))
      return(invisible())
    rv$datosOriginal<-read.csv(inFile$datapath)
  })
  
  
  observe({
    if (is.null(rv$datosOriginal))
      return(NULL)
    datas <- rv$datosOriginal
    datass <- c(unique(datas$type))
    # mintime <- min(datas$time)
    # maxtime <- max(datas$time)
    areadata <- c(unique(datas$area))
    timedata <- c(unique(datas$time))
    updateSelectInput(session, "typefilter", choices = datass, selected = head(datass, 1))
    updateSelectInput(session, "timefilter", choices = timedata, selected = head(timedata, 1))
    updateSelectInput(session, "areafilter", choices = areadata)
    updateSelectInput(session, "typefilter1", choices = datass, selected = head(datass, 1))
    updateSelectInput(session, "typefilter2", choices = datass, selected = head(datass, 1))
    updateSelectInput(session, "typefilter3", choices = datass, selected = head(datass, 1))
    updateSelectInput(session, "timefilter1", choices = timedata, selected = head(timedata, 1))
    updateSelectInput(session, "areafilter1", choices = areadata)
    
  })
  
  observeEvent(input$cluster_helpButton, {
    showModal(
      modalDialog(
        title = tags$b("How to Interpret Cluster Detection Results?"),
        "Example showing clusters of dengue fever patients in January 2011.",
        br(),
        tags$img(src="result_clustermap.png", 
                 style = "max-width: 100%; height: auto;"),
        br(),
        br(),
        "   The values shown in the map show the hotspots. The probability of clustering of infected people in the area is higher than the confidence we set(> 0.95).
        If compared with the confidence that 0.95 if any area has a probability greater than the confidence value specified Indicates that an area has a high risk of clustering, which will be marked with red symbols for that area.
        The area that is less than the confidence value will be assigned a symbol representing that area as gray (Non-Cluster).",
        br(),
        easyClose = TRUE,
        footer = NULL,
        size = "l"
      )
    )
  })
  
  observeEvent(input$association_helpButton, {
    showModal(
      modalDialog(
        title = tags$b("How to interpret associations results?"),
        "Example of showing the relationship between risk factors Monthly mean cloud volume variables and number of dengue fever cases in 2011.",
        br(),
        tags$img(src="association_map.png", 
                 style = "max-width: 100%; height: auto;"),
        br(),
        "The map shows the hotspots of each area in Thailand to indicate whether the risk factors studied affect the number of infected people or not. According to statistical significance (Statistical Significant)",
        br(),
        "by replacing the symbol of that area in red When the risk factors studied affect the number of infected people with statistical significance.",
        br(),
        "Users can view the details of the results of the relationship between the risk factors to be studied and the target groups in addition to the information provided in the table. The results can be explained as follows.",
        tags$img(src="association_table.png", 
                 style = "max-width: 100%; height: auto;"),
        br(),
        "for example If we have data in the column area named Amnat Charoen in row 1, we specify the Significant column as yes, meaning that the average cloud volume factor significantly affects the number of infected people. The mean column describes how a 1-unit increase in average cloud volume contributes to an increase in infection rates of about 25 percent, or an increase in the range of 9 percent to 40 percent at high altitudes. Confidence at 0.95.",
        br(),
        easyClose = TRUE,
        footer = NULL,
        size = "l"
      )
    )
  })
  
  
  
  observeEvent(input$startAnalysisButton, {
    
    observe({
      if (input$useSampleData) {
        rv$datosOriginal <- read.csv("data/DataDengue_2011_5factor.csv")
        shape_files <- list.files("data", pattern = "\\.shp$|\\.dbf$|\\.sbn$|\\.sbx$|\\.shx$|\\.prj$", full.names = TRUE)
        rv$map <- readShapePoly(shape_files, delete_null_obj=TRUE)
      } else {
        if (is.null(rv$map)) {
          rv$messageCheckDataText <- "Error: Map is not uploaded."
          return(NULL)
        }
        
        if (is.null(rv$datosOriginal)) {
          rv$messageCheckDataText <- "Error: Data are not uploaded."
          return(NULL)
        }
      }
    })
    
    
    # # Names of the columns in the map
    # rv$columnnameareainmap<-input$columnnameareainmap
    # 
    # # DELETE input$useSampleData 5
    # if(input$useSampleData){
    #   rv$columnnameareainmap<-"area"
    # }
    # 
    
    output$uploadmapsummary <- renderPrint({
      if (!is.null(rv$map)){
        print(summary(rv$map@data))
      }
    })
    
    output$uploadmaptable  <- renderDataTable({
      if (is.null(rv$map))
        return(NULL)
      rv$map@data
    })
    
    output$mapmap <- renderLeaflet({
      if (is.null(rv$map))
        return(NULL)
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$Stamen.Watercolor, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "Nasa Earth at Night", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen Terrain Background", options = providerTileOptions(noWrap = TRUE)) %>%
        addPolygons(data = rv$map,
                    color = "green",
                    fillColor = "gray", 
                    fillOpacity = 0.75, 
                    weight = 1) %>%
        addLayersControl(baseGroups = c("Open Street Map", "Esri World Imagery","Stamen Watercolor","Nasa Earth at Night","Stamen Terrain Background"),
                         position = c("topleft"),
                         options = layersControlOptions(collapsed = TRUE))
    })
    
    output$uploaddatasummary <- renderPrint({
      if (!is.null(rv$datosOriginal)){
        print(summary(rv$datosOriginal))
      }
    })
    
    output$uploaddatatable  <- renderDataTable({
      if (is.null(rv$datosOriginal))
        return(NULL)
      rv$datosOriginal
    })
    
    output$map_obs <- renderLeaflet({
      if (is.null(rv$datosOriginal))
        return(NULL)
      
      map <- rv$map
      data <- rv$datosOriginal
      data <- data %>%
        filter(
          time %in% input$timefilter,
          type %in% input$typefilter
        )
      
      # datafiltered <- data[which(data$level == 1), ]
      datafiltered <- data
      ordercounties <- match(map@data$NAME_1, datafiltered$area)
      map@data <- datafiltered[ordercounties, ]
      
      # Create leaflet
      l <- leaflet(map) %>% addTiles()
      pal <- colorNumeric(palette = "YlOrRd", domain = map$OBS)
      labels <- sprintf("<strong> %s </strong> <br/> Time: %s <br/> Type: %s <br/> Populations: %s <br/> Observed number of cases: %s ",
                        map$area, map$time, map$type, map$pop, map$OBS
      ) %>%
        lapply(htmltools::HTML)
      
      l %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$Stamen.Watercolor, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "Nasa Earth at Night", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen Terrain Background", options = providerTileOptions(noWrap = TRUE)) %>%
        addPolygons(
          color = "grey", weight = 1,
          fillColor = ~ pal(OBS), fillOpacity = 0.7,
          highlightOptions = highlightOptions(weight = 4),
          label = labels,
          labelOptions = labelOptions(
            style = list(
              "font-weight" = "normal",
              padding = "3px 8px"
            ),
            textsize = "15px", direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal, values = ~OBS, opacity = 0.7,
          title = "OBS", position = "bottomright"
        ) %>%
        addLayersControl(baseGroups = c("Open Street Map", "Esri World Imagery","Stamen Watercolor","Nasa Earth at Night","Stamen Terrain Background"),
                         position = c("topleft"),
                         options = layersControlOptions(collapsed = TRUE))
    })
    
    
    output$map_smr <- renderLeaflet({
      if (is.null(rv$datosOriginal))
        return(NULL)
      
      map <- rv$map
      data <- rv$datosOriginal
      data <- data %>%
        filter(
          time %in% input$timefilter,
          type %in% input$typefilter
        )
      # Add data to map
      data$SMR <- round(data$OBS / data$EXP, 3)
      # datafiltered <- data[which(data$level == 1), ]
      datafiltered <- data
      ordercounties <- match(map@data$NAME_1, datafiltered$area)
      map@data <- datafiltered[ordercounties, ]
      
      # Create leaflet
      l <- leaflet(map) %>% addTiles()
      pal <- colorNumeric(palette = "YlOrRd", domain = map$SMR)
      labels <- sprintf("<strong> %s </strong> <br/> Time: %s <br/> Type: %s <br/> Populations: %s <br/> Expected: %s <br/> Observed number of cases: %s <br/> SMR: %s ",
                        map$area, map$time, map$type, map$pop, map$EXP, map$OBS, map$SMR
      ) %>%
        lapply(htmltools::HTML)
      l %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$Stamen.Watercolor, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "Nasa Earth at Night", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen Terrain Background", options = providerTileOptions(noWrap = TRUE)) %>%
        addPolygons(
          color = "grey", weight = 1,
          fillColor = ~ pal(SMR), fillOpacity = 0.7,
          highlightOptions = highlightOptions(weight = 4),
          label = labels,
          labelOptions = labelOptions(
            style = list(
              "font-weight" = "normal",
              padding = "3px 8px"
            ),
            textsize = "15px", direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal, values = ~SMR, opacity = 0.7,
          title = "SMR", position = "bottomright"
        ) %>%
        addLayersControl(baseGroups = c("Open Street Map", "Esri World Imagery","Stamen Watercolor","Nasa Earth at Night","Stamen Terrain Background"),
                         position = c("topleft"),
                         options = layersControlOptions(collapsed = TRUE))
      
    })
    
    output$plotline_obs <- renderPlotly({
      if (is.null(rv$datosOriginal))
        return(NULL)
      data <- rv$datosOriginal
      data <- data %>%
        filter(
          area %in% input$areafilter,
          type %in% input$typefilter
        )
      ggplotly(
        ggplot(data, aes(x = time, y = OBS, group = area, color = area)) + 
          labs(x="Time", y = "Observation Case") +
          geom_line() + geom_point(size = 2) + theme_bw()
      )
    })
    
    output$plotline_smr <- renderPlotly({
      if (is.null(rv$datosOriginal))
        return(NULL)
      data <- rv$datosOriginal
      data$SMR <- data$OBS / data$EXP
      data <- data %>%
        filter(
          area %in% input$areafilter,
          type %in% input$typefilter
        )
      ggplotly(
        ggplot(data, aes(x = time, y = SMR, 
                         group = area, color = area)) + 
          labs(x="Time", y = "Standard Morbidity Ratio (SMR)") +
          geom_line() + geom_point(size = 2) + theme_bw()
      )
    })
    
    
    output$map_detectcluster <- renderLeaflet({
      if (is.null(rv$datosOriginal))
        return(NULL)
      
      map <- rv$map
      data <- rv$datosOriginal
      
      J <- length(unique(data$type))
      S <- length(unique(data$area))
      T <- length(unique(data$time))
      
      t.from <- min(data$time)
      t.to <- max(data$time)
      
      ## Maps of posterior exceedence probabilities ##
      probs <- matrix(1-Model_cluster$summary.fitted.values$`1 cdf`,S*T,J,byrow=F)
      prob = c(probs[,1],probs[,2],probs[,3])
      data <- cbind(data, prob)
      
      data <- data %>%
        filter(
          type %in% input$typefilter1,
          time %in% input$timefilter1
        )
      
      # Add data to map
      datafiltered <- data
      ordercounties <- match(map@data$NAME_1, datafiltered$area)
      map@data <- datafiltered[ordercounties, ]
      
      # Create a new variable 'var1_group' that groups the values of 'var1' into two ranges
      map$prob_group <- cut(map$prob, c(0, 0.95, 1), labels = c("Non-Cluster", "Cluster"))
      
      
      l <- leaflet(map) %>% addTiles()
      # Create a new color palette for the two groups
      pal <- colorFactor(palette = c("#BCBCBC", "#FF2D00"), domain = map$prob_group)
      labels <- sprintf("<strong> %s </strong> <br/> Exceedance Probability: %s ", map$area, map$prob) %>%
        lapply(htmltools::HTML)
      # Use 'var1_group' instead of 'var1' in the fillColor argument
      l %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$Stamen.Watercolor, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "Nasa Earth at Night", options = providerTileOptions(noWrap = TRUE)) %>%
        addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen Terrain Background", options = providerTileOptions(noWrap = TRUE)) %>%
        addPolygons(
          color = "white", weight = 1,
          fillColor = ~ pal(prob_group), fillOpacity = 0.8,
          highlightOptions = highlightOptions(weight = 4),
          label = labels,
          labelOptions = labelOptions(
            style = list(
              "font-weight" = "normal",
              padding="3px 8px"
            ),
            textsize = "15px", direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal, values = ~prob_group, opacity = 0.7, position = "topright", title = "Cluster"
        ) %>%
        addLayersControl(baseGroups = c("Open Street Map", "Esri World Imagery","Stamen Watercolor","Nasa Earth at Night","Stamen Terrain Background"),
                         position = c("topleft"),
                         options = layersControlOptions(collapsed = TRUE))
      
    })
    
    
    output$association_table = DT::renderDataTable({
      if (is.null(rv$map))
        return(NULL)
      if (is.null(rv$datosOriginal))
        return(NULL)
      
      map <- rv$map
      data <- rv$datosOriginal
      
      thai_map <- rv$map
      
      J <- length(unique(data$type))
      S <- length(unique(data$area))
      
      thai_map@data$ID_area <- seq(1,S)
      
      summary_random <- Model_association[["summary.random"]]
      summary_random <- summary_random[!(names(summary_random) %in% c("idx", "idy"))]
      
      # initialize empty vector to store type_list values
      type_list <- vector()
      for (j in 1:J) {
        for (s in 1:S) {
          type_list <- c(type_list, rep(j))
        }
      }
      # loop over sublists in summary_random
      for (i in 1:length(summary_random)) {
        # add type_list as a new column to the i-th sublist
        summary_random[[i]]$type <- type_list
      }
      
      
      col_names<-names(data)
      col_factor <- col_names[!(names(data) %in% c("area", "time", "OBS", "EXP", "pop", "type"))]
      summary_random <- setNames(summary_random, col_factor)
      
      summary_random_filtered <- summary_random[[input$columnfactordata]]
      
      summary_random_filtered <- summary_random_filtered %>%
        filter(
          type %in% input$typefilter2
        )
      
      summary_random_filtered$ID <-  seq(1,S)
      rownames(summary_random_filtered) <- NULL
      
      
      thai_map@data <- merge(x = thai_map@data , y = summary_random_filtered , by.x = "ID_area" , by.y = "ID")
      thai_map@data <- thai_map@data %>% mutate(Significant =
                                                  case_when(`0.025quant` / `0.975quant` > 0 ~ "yes", 
                                                            `0.025quant` / `0.975quant` < 0 ~ "no"))
      
      col <- c("NAME_1","mean","0.025quant","0.5quant","0.975quant","Significant")
      # col <- c("NAME_1","Significant")
      shp_info <- thai_map@data[,names(thai_map@data) %in% col]
      colnames(shp_info)[1] <- "area"
      shp_info <- shp_info %>%
        mutate(across(c(mean, `0.025quant`, `0.5quant`, `0.975quant`), round, 2))
      shp_info
    })
    
    output$map_association <- renderLeaflet({
      if (is.null(rv$map))
        return(NULL)
      if (is.null(rv$datosOriginal))
        return(NULL)
      
      map <- rv$map
      data <- rv$datosOriginal
      
      thai_map <- rv$map
      
      J <- length(unique(data$type))
      S <- length(unique(data$area))
      thai_map@data$ID_area <- seq(1,S)
      
      summary_random <- Model_association[["summary.random"]]
      summary_random <- summary_random[!(names(summary_random) %in% c("idx", "idy"))]
      
      # initialize empty vector to store type_list values
      type_list <- vector()
      for (j in 1:J) {
        for (s in 1:S) {
          type_list <- c(type_list, rep(j))
        }
      }
      # loop over sublists in summary_random
      for (i in 1:length(summary_random)) {
        # add type_list as a new column to the i-th sublist
        summary_random[[i]]$type <- type_list
      }
      
      
      col_names<-names(data)
      col_factor <- col_names[!(names(data) %in% c("area", "time", "OBS", "EXP", "pop", "type"))]
      summary_random <- setNames(summary_random, col_factor)
      
      summary_random_filtered <- summary_random[[input$columnfactordata]]
      
      summary_random_filtered <- summary_random_filtered %>%
        filter(
          type %in% input$typefilter2
        )
      
      summary_random_filtered$ID <-  seq(1,S)
      rownames(summary_random_filtered) <- NULL
      
      thai_map@data <- merge(x = thai_map@data , y = summary_random_filtered , by.x = "ID_area" , by.y = "ID")
      thai_map@data <- thai_map@data %>% mutate(Significant =
                                                  case_when(`0.025quant` / `0.975quant` > 0 ~ "yes", 
                                                            `0.025quant` / `0.975quant` < 0 ~ "no"))
      
      col <- c("NAME_1","mean","sd","0.025quant","0.5quant","0.975quant","Significant","type")
      shp_info <- thai_map@data[,names(thai_map@data) %in% col]
      colnames(shp_info)[1] <- "area"
      
      datafiltered <- shp_info
      ordercounties <- match(map@data$NAME_1, datafiltered$area)
      map@data <- datafiltered[ordercounties, ]
      
      # Create leaflet
      l <- leaflet(map) %>% addTiles()
      
      # Convert Significant column to factor
      map$Significant <- factor(map$Significant, levels = c("no", "yes"))
      
      pal <- colorFactor(palette = "YlOrRd", domain = map$Significant)
      labels <- sprintf("<strong> %s </strong> <br/> Significant: %s ",
                        map$area, map$Significant
      ) %>%
        lapply(htmltools::HTML)
      
      l %>%
        addPolygons(
          color = "grey", weight = 1,
          fillColor = ~ pal(Significant), fillOpacity = 0.7,
          highlightOptions = highlightOptions(weight = 4),
          label = labels,
          labelOptions = labelOptions(
            style = list(
              "font-weight" = "normal",
              padding = "3px 8px"
            ),
            textsize = "15px", direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal, values = ~Significant, opacity = 0.7,
          title = "Significant", position = "bottomright"
        )
      
    })
    
    output$lineplot_prediction <- renderPlotly({
      if (is.null(rv$map))
        return(NULL)
      if (is.null(rv$datosOriginal))
        return(NULL)
      
      data <- rv$datosOriginal
      thai_map <- rv$map
      colnames(thai_map@data)[4] <- "area"
      
      
      summary_fitted_values <-  Model_prediction$summary.fitted.values
      summary_fitted_values <- round(summary_fitted_values, digits = 4)
      
      J <- length(unique(data$type))
      S <- length(unique(data$area))
      T <- length(unique(data$time))
      
      type_list <- vector()
      for (j in 1:J) {
        for (s in 1:S) {
          type_list <- c(type_list, rep(j))
        }
      }
      
      type_S <- vector()
      for (j in 1:J) {
        for (s in 1:S) {
          type_S <- c(type_S, rep(s))
        }
      }
      
      summary_fitted_values$ID <- type_S
      summary_fitted_values$type <- type_list
      num_col <- length((summary_fitted_values))
      summary_fitted_values <- summary_fitted_values[,1:num_col]
      
      thai_map$ID <- seq(1:S)
      name_area <- thai_map[,c('area','ID')]
      
      
      data_predict <- merge(x = summary_fitted_values , y = name_area , by.x = "ID" , by.y = "ID")
      data_predict <- data_predict[,c('ID','area', "0.025quant", "0.975quant",'mean','type')]
      data_predict <- data_predict[order(data_predict$type),]
      data_predict$time <- rep(1:(T+1), length.out=nrow(data_predict))
      names(data_predict)[names(data_predict) == 'mean'] <- 'OBS'
      names(data_predict)[names(data_predict) == '0.975quant'] <- 'upper_bound'
      names(data_predict)[names(data_predict) == '0.025quant'] <- 'lower_bound'
      
      
      data_predict <- data_predict %>%
        filter(
          area %in% input$areafilter1,
          type %in% input$typefilter3
        )
      
      merged_data <- merge(data_predict, data, by = c("area", "type", "time"), all.x = TRUE)
      merged_data$OBS.y[is.na(merged_data$OBS.y)] <- merged_data$OBS.x[merged_data$time == 13]
      data_predict <- merged_data[,c("area", "OBS.y", "upper_bound","lower_bound" ,"type", "time")]
      colnames(data_predict) <- c("area", "OBS", "upper_bound","lower_bound","type", "time")
      
      
      actual_data <- data_predict[-nrow(data_predict),]
      predicted_mean <- data_predict[nrow(data_predict), "OBS"]
      filtered_data <- data_predict[data_predict$time > (max(data_predict$time)-2), ]
      filtered_data$upper_bound[1] <- 0
      filtered_data$lower_bound[1] <- 0
      
      plot <- plot_ly() %>%
        # add_trace(data = actual_data, x = ~time, y = ~OBS, type = "scatter", mode = "lines+markers", name = "Actual",
        #           line = list(color = "blue"), 
        #           marker = list(color = ifelse(actual_data$time == max(actual_data$time), "red", "blue"))) %>%
        # add_trace(data = data_predict, x = c(nrow(data_predict)-1, nrow(data_predict)), y = c(tail(data_predict$OBS, n = 2)[1], predicted_mean),
        #           type = "scatter", mode = "lines+markers", name = "Predict",
        #           line = list(color = "red"), marker = list(color = "red")) %>%
        
        add_trace(data = data_predict, x = c(nrow(data_predict)-1, nrow(data_predict)), y = c(tail(data_predict$OBS, n = 2)[1], predicted_mean), 
                  type = "scatter", mode = "lines+markers", name = "Predict",
                  line = list(color = "red"), 
                  marker = list(color = "red")) %>%
        add_trace(data = actual_data, x = ~time, y = ~OBS, type = "scatter", mode = "lines+markers", name = "Actual",
                  line = list(color = "blue"),
                  marker = list(color = "blue", size = 5)) %>%
        add_ribbons(data = filtered_data, x = ~c(nrow(data_predict)-1, nrow(data_predict)), ymin = ~lower_bound, ymax = ~upper_bound,
                    fillcolor = "rgba(128, 128, 128, 0.2)", name = "Upper and lower bounds", line = list(color = "transparent")) %>%
        
        layout(xaxis = list(title = "time"),
               yaxis = list(title = "Case"))
      
    })
    
  })
}


shinyApp(ui, server)
