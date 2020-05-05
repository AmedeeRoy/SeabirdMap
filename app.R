library(shiny)
library(DT)
library(leaflet)
library(plotly)
library(RSQLite)
library(plyr)
library(dplyr)
library(stringr)
library(markdown)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyBS)
library(shinyjs)
library(GeoLight)
library(lubridate)
library(crosstalk)
library(raster)
library(htmlwidgets)
library(htmltools)

lapply(list.files(path = "./tools", pattern = ".R", full.names = TRUE),  source, 
       print.eval = FALSE, verbose = FALSE)

### General
pal <- c(    '#1f77b4',  # muted blue
             '#ff7f0e',  # safety orange
             '#2ca02c',  # cooked asparagus green
             '#d62728',  # brick red
             '#9467bd',  # muted purple
             '#8c564b',  # chestnut brown
             '#e377c2',  # raspberry     yogurt pink
             '#7f7f7f',  # middle gray
             '#bcbd22',  # curry yellow-green
             '#17becf'   # blue-teal
)
pal <- rep(pal, 1000)

cat("Connecting to server...\n")
drv = RSQLite::SQLite()
con <- dbConnect(drv, "./data/seabirdbank.db")

ui <- dashboardPage(
    # Header --------------------------------------------
    dashboardHeader(title = span(icon("crow"), "MaFalDa"),
                    
                dropdownMenu(
                      type = "notifications", 
                      headerText = strong("HELP"), 
                      icon = icon("question"), 
                      badgeStatus = NULL,
                      
                      notificationItem(
                        text = 'Overview of birds deployment',
                        icon = icon("binoculars")),
                      notificationItem(
                        text = 'Observe dynamically birds trip for each fieldwork',
                        icon = icon("clock")),
                      notificationItem(
                        text = 'Explore Acoustic data',
                        icon = icon("fish")),
                      notificationItem(
                        text = 'Explore Satellite data',
                        icon = icon("globe-americas"))
                    ),
                    tags$li(
                      a(
                        strong("ABOUT MaFalDa"),
                        height = 40,
                        href = "https://amedeeroy.github.io",
                        title = "",
                        target = "_blank"
                      ),
                      class = "dropdown"
                    )
    ),
    
    # SideBar Menu  --------------------------------------------
    
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        menuItem(
               "FIELDWORK",
               tabName = "fieldwork",
               icon = icon("binoculars"),
               selectizeInput(
                 inputId = "menu_place",
                 label = "Place :",
                 choices = list(Brazil = c("Fernando de Noronha", ""),
                                Peru = c("Guanape", "Pescadores")),
                 multiple = TRUE,
                 selected = "Guanape"),
               dateRangeInput("menu_date", "Date :",
                              start = "2007-11-16",
                              end = "2019-12-09",
                              min = "2007-01-01",
                              max = "2019-12-31")
        ),
        
        menuItem(
          "BIRD",
          tabName = "bird",
          icon = icon("crow"),
          selectizeInput(
            inputId = "menu_species",
            label = "Species :",
            choices = c("Sula dactylatra" = "SD",
                        "Sula leucogaster" = "SL",
                        "Sula sula" = "SS",
                        "Sula variegata" = "SV",
                        "Leucocarbo bougainvilli" = "LB",
                        "Pelecanus thagus" = "PP",
                        "Phaethon aethereus" = "PA"),
            multiple = TRUE,
            selected = "SV"),
          radioGroupButtons(
            inputId = "menu_sex",
            label = "Sex :",
            choices = c(`<i class='fas fa-venus-mars'></i>` = "Indifferent", 
                        `<i class='fas fa-mars'></i>` = "M", 
                        `<i class='fas fa-venus'></i>` = "F"),
            justified = TRUE),
          selectInput("menu_morph", "Morph :", c("Indifferent", "White", "Black"))
        ),
        
        menuItem(
          "BREEDING",
          tabName = "breeding",
          icon = icon("heart"),
          prettyCheckboxGroup(
            inputId = "menu_maturity",
            label = "Maturity :", 
            choices = c("Adult", "Pre-fledg", "Juvenile"),
            status = "primary",
            selected = "Adult",
            outline = TRUE,
            animation = "jelly"
          ),
          
          materialSwitch(
            inputId = "menu_partner",
            label = " Partner", 
            value = FALSE,
            right = TRUE,
            status = "primary"
          ),
          checkboxGroupButtons(
            inputId = "menu_eggs",
            label = "Eggs",
            choices = c(0, 1, 2, 3)
          ),
          checkboxGroupButtons(
            inputId = "menu_chicks_nb",
            label = "Chicks Number",
            choices = c(0, 1, 2, 3)
          ),
          checkboxGroupButtons(
            inputId = "menu_chicks_age",
            label = "Chicks Age",
            choices = c("N0", "N1", "N2", "N3")
          )
        ),
        
        menuItem("BIOMETRY",
                 tabName = "biometry",
                 icon = icon("ruler"),
                 
                 prettyCheckboxGroup(
                   inputId = "menu_biometry",
                   label = "",
                   choices = c("Bill", "Foot", "Tarsus",
                               "Wing", "Mass at Capture", "Mass at Recovery"),
                   status = "primary",
                   animation = "jelly"
                 ),
              
                 htmlOutput("menu_bill_slider"),
                 htmlOutput("menu_tarsus_slider"),
                 htmlOutput("menu_foot_slider"),
                 htmlOutput("menu_wing_slider"),
                 htmlOutput("menu_mass_c_slider"),
                 htmlOutput("menu_mass_r_slider")
        ),
        
        menuItem("DEPLOYMENT",
                 tabName = "deployment",
                 icon = icon("route"),
                 
                 prettyCheckboxGroup(
                   inputId = "menu_deployment",
                   label = "Deployment :", 
                   choices = c("ACC", "GLS", "GPS", "TDR", "FASTLOG"),
                   status = "primary",
                   animation = "jelly",
                   selected = 'GPS'
                 ),
                 
                 materialSwitch(
                   inputId = "menu_file",
                   label = " File Available",
                   value = TRUE,
                   right = TRUE,
                   status = "primary"
                 )
        ),
        
        
        menuItem("TOOLS",
                 tabName = "tools",
                 icon = icon("wrench"),
                  prettyCheckbox(
                    inputId = "menu_all",
                    label = " Load All Data",
                    status = "primary",
                    animation = "jelly"
                  )
        ),

        br(),
        br(),
        
        tags$head(
          tags$style(HTML('#menu_action{background-color:orange}'))
        ),
        
        withBusyIndicatorUI(
            actionBttn(
              inputId = "menu_action",
              label = "load data",
              style = "material-flat",
              color = "danger"
            )
          )
        ) 
      ),
    
    # Body  --------------------------------------------
    dashboardBody(
      useShinyjs(),

      tags$head(
        tags$style(HTML('.btn-primary {
                              color: white;
                              height: 45px;
                              width: 300px;
                              border-radius: 0;
                              margin: auto;
                              position: relative;
                              font-size: 18px;
                          }'))
      ),
      
      fluidRow(
        column(width = 3,
               bsButton("overview", 
                       label = "OVERVIEW", 
                       icon = icon("chart-pie"), 
                       style = "primary")),
        column(width = 3,
               bsButton("map", 
                        label = "MAP", 
                        icon = icon("map"), 
                        style = "primary")),
        column(width = 3,
               bsButton("schedule", 
                        label = "SCHEDULE", 
                        icon = icon("clock"), 
                        style = "primary")),
        column(width = 3,
               bsButton("trip", 
                        label = "TRIP", 
                        icon = icon("chart-line"), 
                        style = "primary"))
      ),
      
      br(),
      
      ### OVERVIEW -------------------------
      fluidRow(
        div(
          id = "overview_panel", 
          column(
            width = 6,
            box(
              height = "800px",
              width = 11,
              solidHeader = TRUE,
              # Title can include an icon
                   dropdownButton(
                         selectizeInput("overview_variable", "Choose Variable",
                                        c("Country" = "country",
                                          "Place" = "place",
                                          "Year" = "year",
                                          "Species" = "species",
                                          "Sex" = "sex",
                                          "Morph" = "morph",
                                          "Maturity" = "maturity",
                                          "Partner" = "partner",
                                          "Chicks Number" = "chicks_nb",
                                          "Chicks Age" = "chicks_age",
                                          "Eggs" = "eggs",
                                          "Band" = "band",
                                          "GPS Deployment" = "gps_dep",
                                          "GPS Data" = "gps_file",
                                          "TDR Deployment" = "tdr_dep",
                                          "TDR Data" = "tdr_file",
                                          "FASTLOG" = "fastlog",
                                          "GLS Deployment" = "gls_dep",
                                          "GLS Data" = "gls_file"),
                                        selected = c("country", "place", "species", "gps_dep"),
                                        multiple = TRUE),
                         circle = TRUE, status = "primary", icon = icon("gear"), width = "300px",
                         tooltip = tooltipOptions(title = "Click to see inputs !")

                       ),
                       plotlyOutput("overview_sunburst", height = "600px")
            )
          ),
          column(
            width = 6,
            tabBox(
              width = 11,
              height = "800px",
              # Title can include an icon
              title = tagList(icon("file-excel"), "Tables"),
              tabPanel("Fieldwork",
                       DTOutput("overview_table_fieldwork")
              ),
              tabPanel("Bird",
                       DTOutput("overview_table_bird")
              ),
              tabPanel("Deployment",
                       DTOutput("overview_table_deployment")
              ),
              tabPanel("Trip",
                       DTOutput("overview_table_trip")
              )
            )
          )
        )
      ),
      
      # tags$style(type = "text/css", "#map_leaflet {height: calc(100vh - 80px) !important;}"),
      
      ### MAP -------------------------
      fluidRow(
      div(
            id = "map_panel",
              
            leafletOutput("map_leaflet", height = "840px"),
            absolutePanel(id = "map_control", class = "panel panel-default",
                          top = 300, right = 50, width = 250, fixed=TRUE,
                          draggable = TRUE, height = "auto",
                          
                          h4(span(icon("fish"), "Acoustic Data")),
                          dateRangeInput("map_fish_date", "Date :",
                                         min = "2007-03-11",
                                         max = "2017-12-09",
                                         start = "2007-03-11",
                                         end = "2008-03-11"),
                          actionButton("map_fish_action", "Plot!"),
                          actionButton("map_fish_clear", "Clear!"),
                          br(),
                          h4(span(icon("globe-americas"), "Satellite Data")),
                          htmlOutput("map_satellite_select"),
                          dateInput("map_satellite_date", "Date :",
                                         min = "2007-01-01",
                                         max = "2019-12-31",
                                         value = "2007-11-18"),
                          prettyRadioButtons("map_satellite_var", "",
                                             choices = c("bathymetry",
                                                         "chlorophyll",
                                                         "sst",
                                                         "wind")),
                          actionButton("map_satellite_action", "Plot!")
            )
        )
      ),
      
      ### SCHEDULE -------------------------
      fluidRow(
        div(
          id = "schedule_panel",
          
          fluidRow(
            div(
              column(
                width = 3,
                box(
                  height = "150px",
                  solidHeader = TRUE,
                  htmlOutput("schedule_menu"),
                  htmlOutput("schedule_action")
                )
              ),
              column(
                width = 9,
                box(
                  width = 12,
                  height = "350px",
                  solidHeader = TRUE,
                  leafletOutput("schedule_map", height = "300px")
                )
              )
            )
          ),
          
          box(
            width = 12,
            height = "450px",
            solidHeader = TRUE,
            
            htmlOutput("schedule_slider"),
            plotlyOutput("schedule_gantt", height = "300px")
            
          )
        )
      ),
      ### TRIP -------------------------
      fluidRow(
        div(
          id = "trip_panel",
          
          fluidRow(
            div(
              column(
                width = 3,
                box(
                  height = "250px",
                  solidHeader = TRUE,
                  htmlOutput("trip_select_bird"),
                  htmlOutput("trip_select_trip"),
                  htmlOutput("trip_action")
                )
              ),
              column(
                width = 3,
                htmlOutput("trip_statistics_1"),
                htmlOutput("trip_statistics_2")
              ),
              column(
                width = 6,
                # box(
                #   width = 12,
                #   height = "350px",
                #   solidHeader = TRUE,
                  plotlyOutput("trip_map", height = "350px")
                # )
              )
            )
          ),
          
          # box(
          #   width = 12,
          #   height = "450px",
          #   solidHeader = TRUE,
            
            dropdownButton(
              htmlOutput("trip_variable_x1"),
              htmlOutput("trip_variable_y1"),
              circle = TRUE, status = "primary", icon = icon("gear"), width = "300px",
              tooltip = tooltipOptions(title = "Click to see inputs !")),
            plotlyOutput("trip_plot_1", height = "200px"),
            
            dropdownButton(
              htmlOutput("trip_variable_x2"),
              htmlOutput("trip_variable_y2"),
              circle = TRUE, status = "primary", icon = icon("gear"), width = "300px",
              tooltip = tooltipOptions(title = "Click to see inputs !")),
            plotlyOutput("trip_plot_2", height = "200px")
            
          # )
        )
      )
      ### End Panels -------------------------
    )
  )

server <- shinyServer(function(input, output, session) {
  
  ### Menu Selection  ---------------------------------------------------------    
  output$menu_bill_slider <- renderUI({
    if(is.element("Bill" , input$menu_biometry)){
      sliderInput("menu_bill", "Bill (mm) :", 0, 450, c(10,405))
    } else {
        return()
      }
  })
  
  output$menu_tarsus_slider <- renderUI({
    if(is.element("Tarsus" , input$menu_biometry)){
      sliderInput("menu_tarsus", "Tarsus (mm) :", 0, 100, c(31,79))
    } else {
      return()
    }
  })
  
  output$menu_foot_slider <- renderUI({
    if(is.element("Foot" , input$menu_biometry)){
      sliderInput("menu_foot", "Foot (mm) :", 0, 200, c(55,118))
    } else {
      return()
    }
  })
  
  output$menu_wing_slider <- renderUI({
    if(is.element("Wing" , input$menu_biometry)){
      sliderInput("menu_wing", "Wing (mm) :", 0, 500, c(215,457))
    } else {
      return()
    }
  })
  
  output$menu_mass_c_slider <- renderUI({
    if(is.element("Mass at Capture" , input$menu_biometry)){
      sliderInput("menu_mass", "Mass at Capture (g) :", 0, 3000, c(1080,2500), 10)
    } else {
      return()
    }
  })
  
  output$menu_mass_r_slider <- renderUI({
    if(is.element("Mass at Recovery" , input$menu_biometry)){
      sliderInput("menu_mass", "Mass at Recovery (g) :", 0, 3000, c(700,2700), 10)
    } else {
      return()
    }
  })
  
  ### Show correct panel
  # use action buttons as tab selectors
  shinyjs::hide("map_panel")
  shinyjs::hide("schedule_panel")
  shinyjs::hide("trip_panel")
  
  observeEvent(input$overview, {
    shinyjs::show("overview_panel")
    shinyjs::hide("map_panel")
    shinyjs::hide("schedule_panel")
    shinyjs::hide("trip_panel")
  })
  observeEvent(input$map, {
    shinyjs::hide("overview_panel")
    shinyjs::show("map_panel")
    shinyjs::hide("schedule_panel")
    shinyjs::hide("trip_panel")
  })
  observeEvent(input$schedule, {
    shinyjs::hide("overview_panel")
    shinyjs::hide("map_panel")
    shinyjs::show("schedule_panel")
    shinyjs::hide("trip_panel")
  })
  observeEvent(input$trip, {
    shinyjs::hide("overview_panel")
    shinyjs::hide("map_panel")
    shinyjs::hide("schedule_panel")
    shinyjs::show("trip_panel")
  })
  
  #### DATA SELECTION
  metadata <- dbGetQuery(con,
                     "SELECT
                           id, country, place, year, start, end, band, species, sex, morph,
                           maturity, partner, chicks_nb,
                           chicks_age,	eggs, bill, tarsus, foot, wing, mass_c, mass_r,
                           gps_dep, gps_file, tdr_dep, tdr_file, gls_dep, gls_file,
                           acc_dep, acc_file, fastlog_dep, fastlog_file,
                           start_dep, end_dep
                       FROM
                       (SELECT
                           fieldwork.country, fieldwork.place, fieldwork.year,
                           fieldwork.id as fw,
                           bird.species, bird.sex,	bird.morph,	bird.maturity,
                           bird.partner, bird.chicks_nb, bird.chicks_age,
                           bird.eggs, bird.band, bird.id, start, end,
                           bird.bill, bird.tarsus, bird.foot,
                           bird.wing, bird.mass_c, bird.mass_r
                           FROM bird
                           INNER JOIN fieldwork ON bird.fieldwork = fieldwork.id) a
                       LEFT JOIN
                       (SELECT
                           deployment.start as start_dep,
                           deployment.end as end_dep,
                           deployment.bird,
                           deployment.logger_type as gps_dep,
                           deployment.file_format IS NOT NULL as gps_file
                           FROM deployment
                           WHERE deployment.logger_type LIKE '%GPS') b
                       ON a.id = b.bird
                       LEFT JOIN
                       (SELECT
                           deployment.bird,
                           deployment.logger_type as tdr_dep,
                           deployment.file_format IS NOT NULL as tdr_file
                           FROM deployment
                           WHERE deployment.logger_type LIKE '%TDR') c
                       ON a.id = c.bird
                       LEFT JOIN
                       (SELECT
                           deployment.bird,
                           deployment.logger_type as fastlog_dep,
                           deployment.file_format IS NOT NULL as fastlog_file
                           FROM deployment
                           WHERE deployment.logger_type LIKE '%FASTLOG') d
                       ON a.id = d.bird
                       LEFT JOIN
                           (SELECT
                               deployment.bird,
                               deployment.logger_type as gls_dep,
                               deployment.file_original IS NOT NULL as gls_file
                               FROM deployment
                               WHERE deployment.logger_type = 'GLS') e
                       ON a.id = e.bird
                       LEFT JOIN
                           (SELECT
                               deployment.bird,
                               deployment.logger_type as acc_dep,
                               deployment.file_original IS NOT NULL as acc_file
                               FROM deployment
                               WHERE deployment.logger_type = 'ACC') f
                       ON a.id = f.bird;")
  
   id <- eventReactive( input$menu_action, {
    selection <- rep(TRUE, nrow(metadata))
    load = TRUE
    
    if(!input$menu_all){
      # place selection
      if(is.null(input$menu_place)){
        load <- FALSE
      } else {
        selection <- selection & is.element(metadata$place, input$menu_place) 
      }
      # date selection
      metadata$start_dep[is.na(metadata$start_dep)] <- metadata$start[is.na(metadata$start_dep)]
      metadata$end_dep[is.na(metadata$end_dep)] <- metadata$end[is.na(metadata$end_dep)]
      selection <-  selection &
        (as.Date(metadata$end_dep) >= input$menu_date[1]) & 
        (as.Date(metadata$start_dep) <= input$menu_date[2])
      # species selection
      if(is.null(input$menu_species)){
        load <- FALSE
      } else {
        selection <- selection & 
          is.element(metadata$species, input$menu_species) 
      }
      # sex selection
      if(is.null(input$menu_sex)){
        load <- FALSE
      } else {
        if(input$menu_sex != "Indifferent"){
          selection <- selection & 
            (metadata$sex == input$menu_sex)  
        }
      }
      # maturity selection
      if(is.null(input$menu_maturity)){
        load <- FALSE
      } else {
        selection <- selection &  is.element(metadata$maturity, input$menu_maturity) 
      }
      # partner selection
      if(input$menu_partner){
        selection <- selection & !is.na(metadata$partner)
      }
      # morph selection
      if(is.null(input$menu_morph)){
        load <- FALSE
      } else {
        if(input$menu_morph != "Indifferent"){
          selection <- selection & 
            (metadata$morph == input$menu_morph)  
        }
      }
      # eggs selection
      if(!is.null(input$menu_eggs)){
        selection <- selection & 
          is.element(metadata$eggs, input$menu_eggs) 
      }
      # chicks_nb selection
      if(!is.null(input$menu_chicks_nb)){
        selection <- selection & 
          is.element(metadata$chicks_nb, input$menu_chicks_nb) 
      }
      # chicks_age selection
      if(!is.null(input$menu_chicks_age)){
        selection <- selection & 
          is.element(metadata$chicks_age, input$menu_chicks_age) 
      }
      # bill selection
      if(!is.null(input$menu_bill)){
        selection <- selection & 
                     (metadata$bill >= input$menu_bill[1]) &
                     (metadata$bill <= input$menu_bill[2])
      }
      # tarsus selection
      if(!is.null(input$menu_tarsus)){
        selection <- selection & 
          (metadata$tarsus >= input$menu_tarsus[1]) &
          (metadata$tarsus <= input$menu_tarsus[2])
      }
      # foot selection
      if(!is.null(input$menu_foot)){
        selection <- selection & 
          (metadata$foot >= input$menu_foot[1]) &
          (metadata$foot <= input$menu_foot[2])
      }
      # wing selection
      if(!is.null(input$menu_wing)){
        selection <- selection & 
          (metadata$wing >= input$menu_wing[1]) &
          (metadata$wing <= input$menu_wing[2])
      }
      # mass capture selection
      if(!is.null(input$menu_mass_c)){
        selection <- selection & 
          (metadata$mass_c >= input$menu_mass_c[1]) &
          (metadata$mass_c <= input$menu_mass_c[2])
      }
      # mass recovery selection
      if(!is.null(input$menu_mass_r)){
        selection <- selection & 
          (metadata$mass_r >= input$menu_mass_r[1]) &
          (metadata$mass_r <= input$menu_mass_r[2])
      }
      # deployment selection
      if(!is.null(input$menu_deployment)){
        if(is.element("ACC", input$menu_deployment)){
          selection <- selection & grepl("ACC$", metadata$acc_dep)
          if(input$menu_file){
            selection <- selection & (metadata$acc_file == 1)
          }
        }
        if(is.element("GLS", input$menu_deployment)){
          selection <- selection & grepl("GLS$", metadata$gls_dep)
          if(input$menu_file){
            selection <- selection & (metadata$gls_file == 1)
          }
        }
        if(is.element("GPS", input$menu_deployment)){
          selection <- selection & grepl("GPS$", metadata$gps_dep)
          if(input$menu_file){
            selection <- selection & (metadata$gps_file == 1)
          }
        }
        if(is.element("TDR", input$menu_deployment)){
          selection <- selection & grepl("TDR$", metadata$tdr_dep)
          if(input$menu_file){
            selection <- selection & (metadata$tdr_file == 1)
          }
        }
        if(is.element("FASTLOG", input$menu_deployment)){
          selection <- selection & grepl("FASTLOG$", metadata$fastlog_dep)
          if(input$menu_file){
            selection <- selection & (metadata$fastlog_file == 1)
          }
        }
      }
    }
    
    withBusyIndicatorServer("menu_action", {
        if(!load){
          stop("wrong request")
        } else {
          id <- metadata$id[which(selection)]
        }
      })
  })
   
   fieldwork <- eventReactive( input$menu_action, {
     dbGetQuery(con, paste0(
                      "SELECT DISTINCT fieldwork.* FROM fieldwork
                      INNER JOIN bird ON bird.fieldwork == fieldwork.id
                      WHERE bird.id IN ('", paste(id(), collapse = "', '"), "');")
                )
   })
   
   bird <- eventReactive( input$menu_action, {
     dbGetQuery(con, paste0(
                      "SELECT DISTINCT bird.* FROM bird
                      WHERE bird.id IN ('", paste(id(), collapse = "', '"), "');")
     )
   })

   deployment <- eventReactive( input$menu_action, {
     data <- dbGetQuery(con, paste0(
             "SELECT DISTINCT deployment.* FROM deployment
                            INNER JOIN bird ON deployment.bird == bird.id
                            WHERE bird.id IN ('", paste(id(), collapse = "', '"), "');"))
     selection <- rep(TRUE, nrow(data))
     # deployment selection
     if(!is.null(input$menu_deployment)){
       selection <- rep(FALSE, nrow(data))
       if(is.element("ACC", input$menu_deployment)){
         selection <- selection | grepl("ACC$", data$logger_type)
       }
       if(is.element("GLS", input$menu_deployment)){
         selection <- selection | grepl("GLS$", data$logger_type)
       }
       if(is.element("GPS", input$menu_deployment)){
         selection <- selection | grepl("GPS$", data$logger_type)
       }
       if(is.element("TDR", input$menu_deployment)){
         selection <- selection | grepl("TDR$", data$logger_type)
       }
       if(is.element("FASTLOG", input$menu_deployment)){
         selection <- selection | grepl("FASTLOG$", data$logger_type)
       }
     }
     if(input$menu_file){
       selection <- selection & (!is.na(data$file_original))
     }
     
     data[which(selection),]
   })
   
   trip <- eventReactive( input$menu_action, {
     dbGetQuery(con, paste0(
       "SELECT DISTINCT trip.* FROM trip
                      INNER JOIN bird ON trip.bird == bird.id
                      WHERE bird.id IN ('", paste(id(), collapse = "', '"), "');"))
   })
   

  ### Overview ---------------------------------------------------------    

  output$overview_sunburst <- renderPlotly({
    
    data <- metadata[is.element(metadata$id, id()),]
    
    data$partner = ifelse(is.na(data$partner), 'NA', 'Partner')
    data$band = ifelse(is.na(data$band), 'NA', 'Band')
    data[is.na(data)] <- 'NA'
    
    dt <- ddply(data, input$overview_variable , nrow)
    sunburstDF <- as.sunburstDF(dt, valueCol = "V1", total = FALSE)
    
    # define color
    f <- factor(word(sunburstDF$ids, sep = " - "))
    colors <- pal[f]
    colors[sunburstDF$labels == 'NA'] <- "white"
    
    # https://plotly.com/r/reference/#sunburst
    plot_ly(data = sunburstDF, 
            ids = ~ids, labels= ~labels, parents = ~parents, values= ~values, 
            type='sunburst', branchvalues = 'total',
            marker = list(colors = colors),
            textfont = list(color = "white"),
            hoverinfo = "label+value+percent entry",
            insidetextorientation = "auto"
    )
  })
  
  output$overview_table_fieldwork <- renderDT({
    datatable(fieldwork(), class = 'cell-border stripe',
              options = list(paging = FALSE, lengthChange = FALSE, scrollY = "600px", scrollX = T),
              rownames = FALSE)})
  
  output$overview_table_bird <- renderDT({
    datatable(bird(), class = 'cell-border stripe',
              options = list(paging = FALSE, lengthChange = FALSE, scrollY = "600px", scrollX = T),
              rownames = FALSE)})
  
  output$overview_table_deployment <- renderDT({
    datatable(deployment(), class = 'cell-border stripe',
              options = list(paging = FALSE, lengthChange = FALSE, scrollY = "600px", scrollX = T),
              rownames = FALSE)})
  
  output$overview_table_trip <- renderDT({
    datatable(trip(), class = 'cell-border stripe',
              options = list(paging = FALSE, lengthChange = FALSE, scrollY = "600px", scrollX = T),
              rownames = FALSE)})
  
  ### Map ---------------------------------------------------------    
  
  position <- eventReactive( input$menu_action, {
      data = dbGetQuery(con, paste0(
        "SELECT DISTINCT gps.* , trip   FROM gps
                      WHERE trip IN ('", paste(trip()$id, collapse = "', '"), "');"))
      position = list(status = "OK", nb = NULL, data = data)
    position
  })

  observeEvent( input$menu_action, {
      output$map_leaflet <-  
            renderLeaflet({
              
              sendSweetAlert(
                session,
                title = "LOADING DATA",
                text = tags$div(
                  progressBar(
                    id = "map_progress",
                    value = 0,
                    total = length(trip()$id),
                    title = "",
                    display_pct = TRUE
                  )
                ),
                btn_labels = NA,
                type = "success",
                closeOnClickOutside = FALSE
              )
              
              m <-
                leaflet() %>%
                addProviderTiles(providers$Esri.OceanBasemap) %>%
                setView(lng = mean(fieldwork()$lon),
                        lat = mean(fieldwork()$lat),
                        zoom = 3) %>%
                addMarkers(lng = fieldwork()$lon,
                           lat = fieldwork()$lat,
                           icon = makeIcon("./www/home.png", iconWidth = 18, iconHeight = 18))
              
              i = 1
              data <- position()$data
              for(t in trip()$id){
                m <- addPolylines(m, 
                                  lng=data[data$trip==t,"lon"], 
                                  lat=data[data$trip==t,"lat"], 
                                  color=pal[i],
                                  popup = t)
                cat(i, " out of ", nrow(trip()), "\n")
                updateProgressBar(
                  session = session,
                  id = "map_progress",
                  value = i,
                  total = length(trip()$id)
                )
                i = i+1
              }
              closeSweetAlert(session = session)
              m
        })
  })
  
  # ### Fieldwork ---------------------------------------------------------    
  
  output$schedule_menu <- renderUI({
    selectInput("schedule_menu_fw", "Fieldwork :",
                choices = fieldwork()$id)
  })
  
  output$schedule_action <- renderUI({
    actionButton("schedule_action_fw", "Plot")
  })
  
  deployment_schedule <- eventReactive( input$schedule_action_fw, {
    data <- deployment()
    data <- data[which(word(data$bird, 1, sep = "_") == input$schedule_menu_fw),]
    data
  })

  trip_schedule <- eventReactive( input$schedule_action_fw, {
    data <- trip()
    data <- data[which(word(data$bird, 1, sep = "_") == input$schedule_menu_fw),]
    data
  })

  observeEvent(input$schedule_action_fw, {
    output$schedule_slider <- renderUI({
      sliderInput("schedule_anim", "Time:",
                  min = min(as.POSIXct(deployment_schedule()$start), na.rm = TRUE),
                  max = max(as.POSIXct(deployment_schedule()$end), na.rm = TRUE),
                  value = min(as.POSIXct(deployment_schedule()$start), na.rm = TRUE),
                  step = minutes(1),
                  animate = animationOptions(interval = 250, loop = FALSE))
    })
  })
  
  observeEvent( input$schedule_action_fw, {
      output$schedule_gantt <- renderPlotly({
        
        dep = deployment_schedule()
        trip = trip_schedule()
        
        dep$start <- as.POSIXct(dep$start)
        dep$end <- as.POSIXct(dep$end)
        trip$start <- as.POSIXct(trip$start)
        trip$end <- as.POSIXct(trip$end)
        

      # get sunrise/sunset
      longitude = unique(fieldwork()$lon[fieldwork()$id == input$schedule_menu_fw])
      latitude = unique(fieldwork()$lat[fieldwork()$id == input$schedule_menu_fw])
      dates = seq(min(dep$start, na.rm = TRUE), max(dep$end, na.rm = TRUE), by = "hour")
      twilight = c(
        twilight(dates, longitude, latitude, rise = TRUE, zenith = 96, iters = 3),
        twilight(dates, longitude, latitude, rise = FALSE, zenith = 96, iters = 3)
      )

      twilight <- unique(sort(twilight))
      rise <- hour(twilight) < mean(hour(twilight))
      while(rise[1]){
        twilight <- twilight[-1]
        rise <- rise[-1]
      }
      while(!rise[length(rise)]){
        twilight <- twilight[-length(twilight)]
        rise <- rise[-length(rise)]
      }
      nights <- data.frame(set = twilight[!rise],
                           rise = twilight[rise])
      # add days & nights
      rect_nights <- list()
      for(k in 1:nrow(nights)){
        rect_nights[[k]] <- list(type = "rect",
                                 fillcolor = "grey",
                                 line = list(color = "grey"),
                                 x0 = nights$set[k],
                                 x1 = nights$rise[k],
                                 y0 = 0,
                                 y1 = nrow(dep)+1,
                                 opacity = 0.2,
                                 text = paste("Night", nights$set[k], nights$rise[k])
        )
      }
      fig <- plot_ly()
      for(i in 1:nrow(dep)){
        fig <- add_trace(fig,
                         x = c(dep$start[i], dep$end[i]),
                         y = c(i, i),
                         mode = "lines",
                         line = list(color = pal[1], width = 5),
                         showlegend = F,
                         hoverinfo = "text",
                         type = "scatter",
                         opacity = 0.25
        )
      }
      for(j in 1:nrow(trip)){
        i = which((trip$bird[j] == dep$bird) & grepl("GPS$", dep$logger_type))

        fig <- add_trace(fig,
                         x = c(trip$start[j], trip$end[j]),
                         y = c(i, i),
                         mode = "lines",
                         line = list(color = pal[1], width = 5),
                         hoverinfo = "text",
                         type = "scatter",
                         opacity = 1,
                         text = paste("Trip: ", trip$id[j])
        )

      }
      fig <- layout(fig, shapes = rect_nights)
      fig
    })

    output$schedule_map <- renderLeaflet({
  
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>%
        setView(lng = mean(fieldwork()$lon),
                lat = mean(fieldwork()$lat),
                zoom = 3) %>%
        addMarkers(lng = fieldwork()$lon,
                   lat = fieldwork()$lat,
                   icon = makeIcon("./www/home.png", iconWidth = 18, iconHeight = 18))
      
    })
  })
  # # modifying dynamically GANTT
  observe({
    if(!is.null(input$schedule_anim)){
      plotlyProxy("schedule_gantt", session) %>%
        plotlyProxyInvoke("deleteTraces", list(-1)) %>%
        plotlyProxyInvoke("addTraces", list(mode = 'lines',
                                            name = 'time',
                                            x = c(input$schedule_anim, input$schedule_anim),                                                    xend = input$fieldwork_anim,
                                            y = c(0, nrow(deployment_schedule()) + 1 ),
                                            line = list(color = "red"),
                                            showlegend = F))
    }
  })
  # # modifying dynamically MAP
  observe({
    if(!is.null(input$schedule_anim)){
      gps <- dbGetQuery(con,
                        paste0(
                          "SELECT
                            gps.datetime,
                            gps.lon,
                            gps.lat,
                            gps.trip,
                            bird.id as bird
                          FROM gps
                          INNER JOIN trip ON gps.trip = trip.id
                          INNER JOIN bird ON bird.id = trip.bird",
                          " WHERE datetime <= '", word(input$schedule_anim, 1, 2), "'
                           AND datetime >= '", word(input$schedule_anim - minutes(2), 1, 2), "'",
                          " ORDER BY trip, datetime ASC;"))


      if(nrow(gps)>0){
        leafletProxy("schedule_map", data = gps) %>%
          clearShapes() %>%
          addCircles(~lon, ~lat, popup = ~trip,
                     color = pal[1]
          )

      } else {
        leafletProxy("schedule_map") %>% clearShapes()
      }
    }
  })

  output$schedule_statistics <- renderUI({
    valueBox(nrow(deployment_schedule()), "Number of Deployments", icon = icon("crow"), color = "aqua")
  })

  ### Bird  ---------------------------------------------------------
  ## Trip Reactive ##

  output$trip_select_bird <- renderUI({
    selectInput("trip_select_bird_", "Bird :", unique(bird()$id))
  })
  
  output$trip_select_trip <- renderUI({
    selectInput("trip_select_trip_", "Trip :", unique(trip()$id[trip()$bird == input$trip_select_bird_]))
  })
  
  output$trip_action <- renderUI({
    actionButton("trip_action_", "Plot")
  })
  
  ## Trip Plot ##
  
  dive <- eventReactive( input$trip_action_, {
    if(!is.na(trip()$file_tdr[trip()$id == input$trip_select_trip_])){
      d <- dbGetQuery(con,
                      paste0("SELECT
                          	datetime, pressure, lon, lat
                              FROM
                              	(SELECT
                              		tdr.datetime as datetime,
                              	 	tdr.pressure
                              	 FROM tdr
                              	WHERE tdr.trip = '", input$trip_select_trip_, "') a
                              LEFT JOIN
                              	(SELECT
                              	 	gps.datetime as gps_dt,
                              	 	gps.lon,
                              		gps.lat
                              	 FROM gps
                              	WHERE gps.trip = '", input$trip_select_trip_, "') b
                              ON a.datetime = b.gps_dt
                              ORDER BY datetime ASC;")
      )
      d <- subset(d, !is.na(lon))
    } else {
      d <- dbGetQuery(con,
                      paste0("SELECT
                                 	datetime, lon, lat
                                 FROM gps
                                 WHERE gps.trip = '", input$trip_select_trip_,"'
                                 ORDER BY datetime ASC;") )
    }
    d$datetime <- as.POSIXct(d$datetime)
    trip_statitics(d)
  })
  
  output$trip_variable_x1 <- renderUI({
    selectInput("trip_variable_x1_", "X axis", colnames(dive()),
                selected = "step_direction")
  })
  
  output$trip_variable_y1 <- renderUI({
    selectInput("trip_variable_y1_", "Y axis", colnames(dive()),
                selected = "step_speed")
  })
  
  output$trip_variable_x2 <- renderUI({
    selectInput("trip_variable_x2_", "X axis", colnames(dive()),
                selected = "datetime")
  })
  
  output$trip_variable_y2 <- renderUI({
    selectInput("trip_variable_y2_", "Y axis", colnames(dive()),
                selected = "step_speed")
  })
  
  eqs <- SharedData$new(dive)
  
  output$trip_plot_1 <- renderPlotly({
    p1 <- plot_ly(eqs, x = eqs$data()[,input$trip_variable_x1_],
                  y = eqs$data()[,input$trip_variable_y1_])  %>%
      add_markers(alpha = 0.5) %>%
      layout(xaxis = list(title = input$trip_variable_x1_),
             yaxis = list(title = input$trip_variable_y1_)) %>%
      highlight("plotly_selected", color = '#ff7f0e', opacity = 1)
  })
  
  output$trip_plot_2 <- renderPlotly({
    p1 <- plot_ly(eqs, x = eqs$data()[,input$trip_variable_x2_],
                  y = eqs$data()[,input$trip_variable_y2_])  %>%
      add_markers(alpha = 0.5) %>%
      layout(xaxis = list(title = input$trip_variable_x2_),
             yaxis = list(title = input$trip_variable_y2_)) %>%
      highlight("plotly_selected", color = '#ff7f0e', opacity = 1)
  })
  
  output$trip_map <- renderPlotly({
    map <-  plot_ly(eqs,  lat = ~lat,
                    lon = ~lon,
                    type = 'scattermapbox')
    map <- map %>%
      layout(
        mapbox = list(
          style = 'open-street-map',
          zoom =8,
          center = list(lon = ~mean(lon),
                        lat = ~mean(lat)))) %>%
      highlight("plotly_selected", color = '#ff7f0e', opacity = 1)
  })
  
  output$trip_statistics_1 <- renderUI({
    valueBox(paste(round(sum(dive()$step_length/1000, na.rm = TRUE)), "km"), 
                         "Total Distance", icon("route"),
             width = 12, color = 'olive')
    
  })
  
  output$trip_statistics_2 <- renderUI({
    valueBox(paste(round(sum(dive()$step_duration/60, na.rm = TRUE)), "min"),
             "Total Duration", icon("tachometer-alt"),
             width = 12, color = 'teal')
  })
  

  ### Fish  ---------------------------------------------------------
  
  fish_data <- eventReactive( input$map_fish_action, {
    dbGetQuery(con,
               paste0(
                 "SELECT DISTINCT
                            fish.lon,
                            fish.lat,
                          	fish.nasc
                      FROM fish
                   WHERE fish.place = '", input$menu_place, "'",
                 " AND fish.date >= '", input$map_fish_date[1], "'",
                 " AND fish.date <= '", input$map_fish_date[2], "';")
    )})
  
  # modifying dynamically MAP
  observeEvent(input$map_fish_action, {
    if(nrow(fish_data())>0){
        pal <- colorNumeric("RdBu", range(log(1+fish_data()$nasc)), reverse=TRUE, na.color = "transparent")
        
        leafletProxy("map_leaflet", data = fish_data()) %>%
          addCircles(lng = ~lon,
                     lat = ~lat,
                     color = pal(log(1+fish_data()$nasc)),
                     group = "fish")
      
    } else {
      leafletProxy("map_leaflet") %>%
        clearGroup("fish")
    }
  })
  
  observeEvent(input$map_fish_clear, {
    leafletProxy("map_leaflet") %>%
      clearGroup("fish")
  })


  # 
  # ### Satellite  ---------------------------------------------------------    
  output$map_satellite_select <- renderUI({
    selectInput("map_satellite_fw", "Fieldwork",
                choices = fieldwork()$id)
  })
  
  satellite <- eventReactive( input$map_satellite_action, {
                  dbGetQuery(con,
                             paste0("SELECT * from satellite WHERE format!='L2'",
                                    " AND fieldwork = '", input$map_satellite_fw, "'")
                  )
                })

  # modifying dynamically MAP
  observeEvent(input$map_satellite_action, {
    try({
      path = "./data"
      if(input$map_satellite_var == "chlorophyll"){
        # --------------------
        # GlobColour
        file = paste(path, "GlobColour", satellite()$file[satellite()$product =="GlobColour"], sep ="/")
        nc <- ncdf4::nc_open(file)
        time <- ncdf4::ncvar_get(nc, "time")
        origin = as.POSIXct("1900-01-01 00:00:00")
        time = as.Date(origin + lubridate::days(time))
        
        r <- raster(file, band = which(time == input$map_satellite_date), varname = "CHL")
        pal <- colorNumeric("Greens", values(r), na.color = "transparent")
        
        leafletProxy("map_leaflet") %>% 
          clearGroup("sst") %>%
          clearGroup("wind") %>%
          addRasterImage(r,  project = FALSE, colors = pal, opacity = 0.5, group = "chlorophyll") %>%
          addLegend(values = values(r), pal = pal, title = "Chlorophyll (mg/m3)", group = "chlorophyll")
      }
      
      if(input$map_satellite_var == "sst"){
        # --------------------
        # MODIS
        file = paste(path, "MODIS", satellite()$file[satellite()$product =="MODIS"], sep ="/")
        nc <- ncdf4::nc_open(file)
        time <- ncdf4::ncvar_get(nc, "time")
        origin = as.POSIXct("2002-01-01 00:00:00")
        time = as.Date(origin + lubridate::days(time))
        
        r <- raster(file, band = which(time == input$map_satellite_date), varname = "sst")
        pal <- colorNumeric("RdBu", values(r), reverse = TRUE, na.color = "transparent")
        
        leafletProxy("map_leaflet") %>% 
          clearGroup("chlorophyll") %>%
          clearGroup("wind") %>%
          addRasterImage(r,  project = FALSE, colors = pal, opacity = 0.5, group = "sst") %>% 
          addLegend(values = values(r), pal = pal, title = "SST (°C)", group = "sst")
      }
      
      if(input$map_satellite_var == "wind"){
        # --------------------
        # ASCAT
        file = paste(path, "ASCAT", satellite()$file[satellite()$product =="ASCAT"], sep ="/")[1]
        
        nc <- ncdf4::nc_open(file)
        time <- ncdf4::ncvar_get(nc, varid="time")
        time <- as.POSIXct(time, origin = "1990-01-01 00:00:00")
        lat <- ncdf4::ncvar_get(nc, varid="lat")
        lon <- ncdf4::ncvar_get(nc, varid="lon")
        speed <- ncdf4::ncvar_get(nc, varid="wind_speed") # wind speed
        dir <- ncdf4::ncvar_get(nc, varid="wind_to_dir") # wind direction
        
        b = which(as.Date(time) == input$map_satellite_date)
        speed <- speed[,,b]
        dir <- dir[,,b]
        
        df <- data.frame(lon = lon[base::row(dir)], lat = lat[base::col(dir)], angle = c(dir))
        df <- subset(df, !is.na(angle))
        
        r <- raster(file, band = b, varname = 'wind_speed')
        pal <- colorNumeric("RdBu", values(r), reverse=TRUE, na.color = "transparent")
     
        leafletProxy("map_leaflet") %>%      
          clearGroup("chlorophyll") %>%
          clearGroup("sst") %>%
          registerPlugin(plugin = rotatedMarker ) %>% 
          addRasterImage(r,  project = FALSE, colors = pal, opacity = 0.5, group = "wind") %>% 
          addLegend(values = values(r), pal = pal, title = "Wind (m/s)", group = "wind") %>% 
          addMarkers(      lng = ~lon,
                           lat = ~lat,
                           icon = wind.icon,
                           options = markerOptions( rotationAngle = ~ 180+ angle),
                           data = df,
                           group = "wind")
        
      }
   
      if(input$map_satellite_var == "bathymetry"){
        leafletProxy("map_leaflet") %>%      
          clearGroup("chlorophyll") %>%
          clearGroup("sst") %>%
          clearGroup("wind")
      }
      
    })
  })
  
  # output$satellite_map <- renderLeaflet({
  #   

  #     
  #     map <-leaflet() %>% addTiles()

  #   }
  #   
  #   map
  # })
  ### End Tab items  ---------------------------------------------------------    
})

### Run Shiny App
shinyApp(ui = ui, server = server)

### Deployment
# rsconnect::setAccountInfo(name='amdroy',
#                           token='56471F99313DBEF23B2E2ABF6677E785',
#                           secret='9BW41JbxnNvkbDpMe5HXIofFGpH7mDhGAqyA2uDm')
# rsconnect::deployApp('/home/amdroy/MEGA/Website/SeabirdMap/')