library(shiny)
library(shinyWidgets)
library(htmltools)
library(zip)

library(reactable)

library(readxl)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(glue)
library(lubridate)
library(tidyr)
library(ggplot2)
library(scales)

library(sf)
library(leaflet)
library(leaflet.extras2)



# UI ----
ui <- fluidPage(
  
  ## Title ----
  titlePanel("qPCR Data Manager 3"),

  mainPanel(
    tabsetPanel(
      
      ## Data Upload Tab ----
      tabPanel("Data Upload", 
               sidebarLayout(
                 sidebarPanel(
                   fileInput(inputId = "qPCRfiles", 
                             label = HTML("<h4>Upload excel sheets output from StepOne Software</h4>
<p>To upload more than one file hold Crtl (Windows) or Command (Mac) and select files.</p>"), 
                             multiple = TRUE,
                             accept = c(".xls", ".xlsx")
                            ),
                  fileInput(inputId = "latLongFile", 
                            label = "latLongFile as .csv (Optional)",
                            multiple = FALSE,
                            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                            )
                 ),
               mainPanel(verbatimTextOutput("uploadStatus"))
               )),
      
      
      ## Combined Data Tab ----
      tabPanel("Combined Data",
               sidebarLayout(
                 sidebarPanel(
                   downloadButton("downloadData", "Download Data")
                 )
                 ,
               mainPanel(reactableOutput("combinedTable"))
               )),
      
      # ## Sample Summary Tab ----
      # tabPanel("Sample Summary", 
      #          sidebarLayout(
      #            sidebarPanel(
      #              downloadButton("downloadSampleData", "Download Data")
      #            )
      #            ,
      #            mainPanel(reactableOutput("sampleSummary"))
      #          )),
      
      # ## Site Summary Tab ----
      # tabPanel("Overall Site Summary", reactableOutput("siteSummary")),
      
      ## Negatives Summary Tab ----
      tabPanel("Negatives Summary",
               sidebarLayout(
                 sidebarPanel(
                   downloadButton("downloadNegativeData", "Download Data")
                 )
                 ,
                 mainPanel(reactableOutput("negativesSummary"))
               )),
      
      ## Standards Summary Tab ----
      tabPanel("Standards Summary",
               sidebarLayout(
                 sidebarPanel(
                   downloadButton("downloadStandardsData", "Download Data")
                 )
                 ,
                 mainPanel(reactableOutput("standardsSummary"))
               )),
  
    
      
      ## Detection Summary Tab ----
      tabPanel("Detection Summary", 
               sidebarLayout(
                 sidebarPanel(
                   downloadButton("downloadDetData", "Download Data (.csv)"),
                   downloadButton("downloadDetDataDoc", "Download Table (.docx)")
                 )
                 ,
                 mainPanel(reactableOutput("detSummary"))
               )),
      
      ## Only 1 amplification Summary Tab ----
      tabPanel("Only 1 Amplification", reactableOutput("only1AmpSummary")),

      ## Detection Map Tab ----
      tabPanel("Maps", 
               sidebarLayout(
                 sidebarPanel(
                   # Target (Species)
                   uiOutput('mapDetTarget'),

                   # Sample No.
                   uiOutput('mapDetSampleNo'),

                   downloadButton("downloadMapShapefile", "Download Shapefile"),
                   downloadButton("downloadMapRds", "Download .rds file (sf)"),
                 ),
                 mainPanel(
                  leafletOutput("mapDetections"), 
                  br(),
                  fluidRow(
                    column(4, 
                      h4("Sites in qPCR Data But Not on Map"),
                      reactableOutput("sitesInDataNotOnMap")
                          ),
                    column(8,
                      h4("Sites in Site Key But Not in qPCR Data"),
                      reactableOutput("sitesInKeyNotOnMap")
                    )
                  )
                 )
               )),

    ## Florescence Tab ----
    tabPanel("Florescence Plot", 
             sidebarLayout(
               sidebarPanel(
                 uiOutput('fileNameFlor'),
                 selectInput("plotLog", "Select y-axis transformation", 
                             choices = c("Log10", "Linear"))
             ),
             mainPanel(
               plotOutput("florPlot"),
               checkboxGroupButtons("plotWell", "Select Well",
                                    choiceNames = expand.grid(LETTERS[1:8], 1:12) %>%
                                      mutate_all(as.character) %>%
                                      mutate(Var2 = ifelse(str_length(Var2)==1, paste0(Var2, " "), Var2)) %>%
                                      arrange(Var1) %>% 
                                      transmute(x = paste0(Var1, Var2)) %>%
                                      pull(),
                                    choiceValues = expand.grid(LETTERS[1:8], 1:12) %>%
                                      mutate_all(as.character) %>%
                                      arrange(Var1) %>% 
                                      transmute(x = paste0(Var1, Var2)) %>%
                                      pull(),
                                    size = "sm", 
                                    individual = TRUE,
                                    width = '525px')
             )
             ))

    )
  )
)

# Server ----
server <- function(input, output) {
  
  ## * Upload Status ----
  output$uploadStatus <- renderPrint({
    files <- input$qPCRfiles
    if (is.null(files)) {
      return(NULL) #No file(s) uploaded
    }else{
      filenames= map2(files$name, 1:length(files$name), ~glue("{.y}) {.x}")) %>% 
                      unlist() %>% paste(collapse = "\n") 
      print(glue("You have uploaded {nrow(files)} file(s).\nTheir name(s) are:\n{filenames}"))
    }
    
    latlon <- input$latLongFile
    if (is.null(latlon)) {
      return(NULL) #No file(s) uploaded
    }else{
      print(glue("\nFor lat/lon data you have uploaded:\n    {latlon$name}"))
    }    

  })
  
  
  # -------------------------------------------------------------------------- #
  
  ## * Import data ----
  
  ### * Fun to combine all qpcr data uploads ----
  all_data <- reactive({
    files <- input$qPCRfiles
    if (is.null(files)) {
      return(NULL)
    }
    
    ## Get sheet number
    for(i in 1:length(read_excel(files$datapath[1], skip = 7))){
      temp = read_excel(files$datapath[1], skip = 7, sheet = i)
      if("NOAMP" %in% colnames(temp)){
        sheetNo = i
        break
      }
    }
    
    ## all sheets to one df
    map2_dfr(files$datapath, files$name, 
             ~read_excel(.x, skip = 7, sheet = sheetNo) %>% 
              mutate(file_name = basename(.y),
                     date_qpcr = lubridate::ymd(str_extract(.y, "^[0-9_-]+"))) # , .before = "Well"
    ) %>% 
      select(-EXPFAIL, -Comments, -`Automatic Baseline`, -`Baseline Start`, -`Baseline End`) %>% 
      mutate(`Sample Name` = str_trim(`Sample Name`)) %>% 
      mutate(`Site Code` = str_extract(`Sample Name`, "^[A-z0-9]+(?=\\s)")) %>% 
      mutate(`Sample No.` = case_when(
        `Task` == "STANDARD" ~ NA_character_,
        `Sample Name` == "" ~ NA_character_,
        `Task` == "NTC" ~ NA_character_,
        TRUE ~ map_chr(str_split(`Sample Name`, "\\s+"), ~.x[2])) 
      ) %>% 
      mutate(`Sample Date` = 
               `Sample Name` %>%
               str_replace_all("\\.", "-") %>% 
               str_extract("[0-9_-]+$") %>% 
               mdy()
      ) %>% 
      # mutate_if(is.numeric, ~round(.x, 4)) %>% 
      mutate(detection = if_else(`NOAMP` == "N", 1, 0)) 
  })
  
  
  ### * Fun to get floresence data ----
  florData = reactive({
    files <- input$qPCRfiles
    if (is.null(files)) {
      return(NULL)
    }
    
    ## find what file we're working with
    fileName = files$datapath[files$name == input$fileNameFlor]
    
    ## import
    df = read_excel(fileName, skip = 7, sheet = "Amplification Data") 
    
  })
  
  ### * Fun to compile latlong data ----
  latLongData = reactive({
    
    file <- input$latLongFile
    if (is.null(file)) {
      return(NULL)
    }
    
    ## read in file
    df = read.csv(file$datapath)
    
    ## Get lat and long, site code, name columns & rename
    for(col in colnames(df)){
      if(str_detect(col, regex("LAT\\.*|LATITUDE", ignore_case = T))){
        latCol = df[col] %>% pull()
      }
      if(str_detect(col, regex("LONG*\\.*|LONGITUDE", ignore_case = T))){
        lonCol = df[col] %>% pull()
      }
      if(str_detect(col, regex("Sites*\\.*Codes*", ignore_case = T))){
        siteCodeCol = df[col] %>% pull()
      }
      if(str_detect(col, regex("Sites*\\.*Names*", ignore_case = T))){
        siteNameCol = df[col] %>% pull()
      }      
    }
    
    ## If one missing return error
    if(!exists("latCol") |
       !exists("lonCol") |
       !exists("siteCodeCol") |
       !exists("siteNameCol")
       ){
      return("Could not find lat/lon or site code/name columns.")
    }
    
    ## to new df
    df2 = tibble(`Site Code` = siteCodeCol, 
                 `Site Name` = siteNameCol,
                 # `Route` = Route,
                 lat = latCol, lon = lonCol) %>% 
      filter(!is.na(lat) | !is.na(lon))
    
    
    ## output df
    df_sf = st_as_sf(df2, coords=c("lon","lat"), crs=4326) 
    df_sf
    
  })
  
  
  # -------------------------------------------------------------------------- #
  
  ## * Output Tabs ----
  
  ### * Combined DataTables Tab ----
  output$combinedTable <- renderReactable({
    req(all_data())
    reactable(all_data(), 
              # searchable = TRUE, 
              filterable = TRUE,
              pageSize = 10,
              style = list(
                css = list(
                  ".rt-tbody" = list(
                    "max-width" = "100%",
                    "overflow-x" = "auto"
                    # "overflow" = "auto",
                    # "height" = "auto"
                  )
                )
              )
    )
  })
  
  
  ### * Sample Summary Tab ----
  sampleSum = function(){
    all_data() %>%
      group_by(`Sample Name`, Task) %>%
      summarise(`Detection` = if_else(sum(detection) > 0, "Y", "N"), 
                `# Detections/# Tech reps` = glue("{sum(detection)} / {n()}"),
                .groups = 'drop')
  }
  
  output$sampleSummary <- renderReactable({
    req(all_data())
    summary_data <- sampleSum()
    reactable(summary_data,
              pagination = TRUE,
              defaultPageSize = 10
    )
  })
  
  ### * Site Summary Tab ----
  output$siteSummary <- renderReactable({
    req(all_data())
    summary_data <- all_data() %>%
      group_by(`Site Code`, Task) %>%
      summarise(`Detection` = if_else(sum(detection) > 0, "Y", "N"), 
                `# Detections/# Tech reps` = glue("{sum(detection)} / {n()}"),
                .groups = 'drop')
    
    reactable(summary_data,
              pagination = TRUE,
              defaultPageSize = 10
    )
  })  
  
  
  ### * Negative Summary Tab ----
  negSum = function(){
    all_data() %>%
      group_by(`Site Code`, `Sample No.`) %>%
      filter(`Sample No.` == "N" | `Site Code` %in% c("ENC", "FNC")) %>% 
      summarise(`Detection` = if_else(sum(detection) > 0, "Y", "N"), 
                `# Detections/# Tech reps` = glue("{sum(detection)} / {n()}"),
                .groups = 'drop')
  }
  
  output$negativesSummary <- renderReactable({
    req(all_data())
    summary_data <- negSum()
    reactable(summary_data,
              pagination = TRUE,
              defaultPageSize = 10
    )
  })
  
  ### * Standards Summary Tab ----
  standardsSum = function(){
    all_data() %>%
      group_by(Task, `Sample Name`) %>%
      # filter(`Sample No.` == "N" | `Site Code` %in% c("ENC", "FNC")) %>% 
      filter(`Task` == "STANDARD") %>% 
      summarise(`Detection` = if_else(sum(detection) > 0, "Y", "N"), 
                `# Detections/# Tech reps` = glue("{sum(detection)} / {n()}"),
                .groups = 'drop')
  }
  
  output$standardsSummary <- renderReactable({
    req(all_data())
    summary_data <- standardsSum()
    reactable(summary_data,
              pagination = TRUE,
              defaultPageSize = 10
    )
  })
  

  ### * Detection Summary Tab ----
  detSum = function(){
    all_data() %>%
      group_by(`Site Code`, `Sample No.`) %>%
      filter(`Sample No.` != "N", !(`Site Code` %in% c("ENC", "FNC"))) %>%
      filter(`Task` != "STANDARD") %>% 
      summarise(`Detection` = if_else(sum(detection) > 0, "Y", "N"), 
                `# Detections/# Tech reps` = glue("{sum(detection)} / {n()}"),
                .groups = 'drop')
  }
  output$detSummary <- renderReactable({
    req(all_data())
    summary_data <- detSum()
    reactable(summary_data,
              pagination = TRUE,
              defaultPageSize = 10
    )
  })   
  
  ### * only1AmpSummary Tab ----
  only1AmpSum = function(){
    all_data() %>%
      group_by(`Site Code`, `Sample No.`) %>%
      filter(`Sample No.` != "N", !(`Site Code` %in% c("ENC", "FNC"))) %>% 
      filter(sum(detection) == 1) %>% 
      ungroup() %>% 
      filter(detection == 1)
  }
  
  output$only1AmpSummary <- renderReactable({
    req(all_data())
    summary_data <- only1AmpSum()
    reactable(summary_data,
              pagination = TRUE,
              defaultPageSize = 6
    )
  })
  
  
  ### * Map Detections Tab ----
  
  #### * Fun to combine qpcr and latlon data ----
  detToSf = function(){
    
    # get detections
    det = all_data() %>%
      filter(
              # `Sample No.` != "N", 
             !(`Site Code` %in% c("ENC", "FNC")),
               `Task` != "STANDARD"
             ) %>% 
      group_by(`Target Name`, `Site Code`, `Sample No.`) %>% # 
      summarise(`Detection` = if_else(sum(detection) > 0, "Y", "N"), 
                N = sum(detection),
                N_tr = n(),
                # `# Detections/# Tech reps` = glue("{sum(detection)} / {n()}"),
                .groups = 'drop') %>% 
      mutate(n_det = case_when(N == 0 ~ "0 positive technical replicates",
                           N == 1 ~ "1",
                           N == 2 ~ "2",
                           N >= 3 ~ "3+"))
    
    # get latlong sites
    df = latLongData()
    
    # join
    detections = det %>% 
      left_join(df, by = "Site Code") %>% 
      st_as_sf()
    

    return(detections)
  }
  
  #### * filter Map based on user input
  userDetMap = function(input){
    
    # get data
    detections = detToSf() 
    
    # if filters from user input
    if(length(input$mapDetSampleNo) == 1){
      detections = 
        detections %>% 
        filter(`Sample No.` %in% input$mapDetSampleNo,
               `Target Name` == input$mapDetTarget)
      
    }else if(length(input$mapDetSampleNo) > 1){
      detections = 
        detections %>% 
        filter(`Sample No.` %in% input$mapDetSampleNo,
               `Target Name` == input$mapDetTarget) %>% 
        group_by(`Target Name`, `Site Code`, `Site Name`) %>% 
        summarise(
          `No. Sampling Events` = n(),
          `Per Sample No.` = 
            paste0(glue("Sample No. {`Sample No.`} = {N}/{N_tr}"), collapse = ", "),
          N = sum(N),
          .groups = 'drop') %>% 
        mutate(n_det = case_when(N == 0 ~ "0 positive technical replicates",
                                 N == 1 ~ "1",
                                 N == 2 ~ "2",
                                 N >= 3 ~ "3+"))
    }else{
      return(NULL)
    }
  }
  
  
  
  #### * Map ----
  output$mapDetections <- renderLeaflet({  
    req(all_data())
    req(latLongData())
    
    # get det data
    detections = userDetMap(input)
    
    # mapview
    # m = mapview(detections, 
    #             zcol="n_det", 
    #             col.regions = c("0 positive technical replicates" = "grey", 
    #                             "1" = "yellow",
    #                             "2" = "orange",
    #                             "3+" = "red")
    # )
    # 
    # m@map %>% 
    #   # https://search.r-project.org/CRAN/refmans/leaflet.extras2/html/easyprintOptions.html
    #   addEasyprint(options = easyprintOptions(
    #     title = 'Save Screenshot',
    #     sizeModes = list("CurrentSize"),
    #     position = 'bottomleft',
    #     # hidden = FALSE,
    #     exportOnly = TRUE))
    
    pal <- colorFactor(c("grey", "yellow", "orange", "red"), 
                        domain = c("0 positive technical replicates", "1", "2", "3+"))
    detections %>% 
      leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircleMarkers(
        radius = 6,
        color = ~pal(n_det),
        stroke = FALSE, fillOpacity = 0.7,
        popup = ~htmlEscape(`Site Code`), label = ~htmlEscape(`Site Name`)
      ) %>% 
      addLegend("topright", pal = pal, values = ~n_det,
                title = "Detections",
                # labFormat = labelFormat(prefix = "$"),
                opacity = 1
      ) %>% 
      # https://search.r-project.org/CRAN/refmans/leaflet.extras2/html/easyprintOptions.html
      addEasyprint(options = easyprintOptions(
        title = 'Save Screenshot',
        sizeModes = list("CurrentSize"),
        position = 'bottomleft',
        # hidden = FALSE,
        exportOnly = TRUE))
      
  })
  
  
  
  #### * Site(s) in Data but not on Map ----
  output$sitesInDataNotOnMap <- renderReactable({
    req(all_data())
    req(latLongData())
    
    detections = userDetMap(input)  %>% 
      filter(st_is_empty(.)) %>%
      st_drop_geometry()
    
    reactable(detections,
              pagination = TRUE,
              defaultPageSize = 6
    )
  })
  
  #### * Sites in Key but not in data/map ----
  output$sitesInKeyNotOnMap <- renderReactable({
    
    req(all_data())
    req(latLongData())
    
    # get what is on map
    detections = userDetMap(input)
    
    # get latlong sites
    key = latLongData()
    
    # Get rows not in map
    keyNotInMap = key %>% 
      filter(!`Site Code` %in% detections$`Site Code`) %>% 
      st_drop_geometry()
    
    reactable(keyNotInMap,
              pagination = TRUE,
              defaultPageSize = 6
    )
    
  })
  
  
   
  ### *  Florescence plot ----
  output$florPlot <- renderPlot({
    req(all_data())
    req(florData())
    
    # Get Flor data
    df = florData()

    # Get threshold
    data = all_data() %>% filter(file_name == input$fileNameFlor)
    ct_threshold = data$`Ct Threshold`[1]

    
    # Plot data
    p1 =  df %>% 
      select(-Rn) %>% 
      mutate(Row = str_extract(Well, "^[A-Z]")) %>% 
      filter(Well %in% input$plotWell) %>% #
      pivot_longer(c(-Well, -Row, -Cycle, -`Target Name`)) %>% 
      ggplot(aes(x = Cycle, y = value, group = Well, color = Row)) + 
      geom_line() + 
      geom_hline(yintercept = ct_threshold, alpha = 0.7, color = "lightblue", size = 1.5) + 
      geom_text(aes(0, ct_threshold ,label = round(ct_threshold, 6), vjust = -1), color = "red") +
      scale_color_brewer(type = 'qual', palette = 2) + 
      labs(y = "ΔRn") + 
      theme_minimal()
    
    if(input$plotLog == "Log10"){
      p1 = p1 +     
        scale_y_continuous(trans = "log10",  
                           breaks = log_breaks(9, base = 10), 
                           label=comma) 
    }

    p1
    
  })
  
  
  # -------------------------------------------------------------------------- #
  
  ## * User Input Vals (derived from data uploads) ----
  
  ### * Map Sample No ----
  ##... need to filter for only ones with mapped sites
  output$mapDetSampleNo <- renderUI({
    
    req(all_data())
    
    sample_nos = 
      all_data() %>%
      filter(
       !(`Site Code` %in% c("ENC", "FNC")),
       `Task` != "STANDARD",
       !is.na(`Sample No.`)
       ) %>% 
      count(`Sample No.`) %>% 
      arrange(`Sample No.`) %>% 
      pull(`Sample No.`)
    
    checkboxGroupInput(
      inputId = 'mapDetSampleNo', 
      label = 'Select Sample Number(s)', 
      choices = sample_nos, selected = sample_nos[1])
  })

  ### * Map Target Name (Species) ----
  output$mapDetTarget <- renderUI({
    
    req(all_data())
    
    taget_name = all_data() %>%
      filter(
        !(`Site Code` %in% c("ENC", "FNC")),
        `Task` != "STANDARD"
      ) %>% 
      count(`Target Name`, sort = TRUE) %>% 
      pull(`Target Name`)

    selectInput("mapDetTarget", "Select Target Name (Species)", taget_name)
  })
  
  
  ### * Floresence File Name ----
  output$fileNameFlor <- renderUI({
    req(all_data())
    data = all_data() 
    fileNames = distinct(data, file_name)
    selectInput("fileNameFlor", "Select File to View", fileNames)
  })


  ### * Floresence Well Choices ----
  # output$plotWell <- renderUI({
  #   
  #   wellChoices =            
  #     expand.grid(LETTERS[1:8], 1:12) %>%
  #     mutate_all(as.character) %>%
  #     transmute(x = paste0(Var1, Var2)) %>%
  #     pull()
  #   
  #   start = seq(1,(8*12), by = 8)
  #   end = seq(8,(8*12), by = 8)
  #   
  #   tagList(
  #     for(i in 1:12){
  #       {
  #         div(
  #           style = "grid-gap: 1em;display:flex;align-items: center;",
  #           tags$label(glue("test")),
  #           shiny::checkboxGroupInput(
  #             inputId = glue("plotWell"),
  #             label =  wellChoices[start[i]:end[i]],
  #             choices = wellChoices[start[i]:end[i]],
  #             inline = TRUE
  #           )
  #         )
  #       }
  #     }
  # 
  #   )
    
    # tagList(
    #   map(
    #     wellChoices,
    #     ~ {
    #       div(
    #         style = "grid-gap: 1em;display:flex;align-items: center;",
    #         tags$label(glue("{.}")),
    #         shiny::checkboxGroupInput(
    #           inputId = glue("plotWell"),
    #           label = .,
    #           choices = .,
    #           inline = TRUE
    #         )
    #       )
    #     }
    #   )
    # )
  # })

  
  # -------------------------------------------------------------------------- #
  ## * Download Handlers ----
  
  ### * Combined Data ----
  output$downloadData <- downloadHandler(
    filename = function() {
      "combinedData.csv"
    },
    content = function(file) {
      req(all_data())
      write_csv(all_data(), file)
    }
  )
  
  ### * Sample Data ----
  output$downloadSampleData <- downloadHandler(
    filename = function() {
      "sampleData.csv"
    },
    content = function(file) {
      req(all_data())
      write_csv(sampleSum(), file)
    }
  )  
  
  ### * Detection Data ----
  output$downloadDetData <- downloadHandler(
    filename = function() {
      "detData.csv"
    },
    content = function(file) {
      req(all_data())
      write_csv(detSum(), file)
    }
  )  
  
  output$downloadDetDataDoc <- downloadHandler(
    filename = function() {
      "detData.docx"
    },
    content = function(file) {
      req(all_data())
      temp = 
        detSum() %>% 
        flextable() %>% 
        bold(part = "header")
      save_as_docx(temp, path = file)
    }
  )  
  
  ### * Map Data ----
  
  #### * Shapefile ----
  output$downloadMapShapefile <- downloadHandler(
    filename = "detection_shapefile.zip",
    content = function(file){
      req(all_data())
      req(latLongData())
      data = userDetMap(input)
      
      withProgress(message = "Exporting Data", {
        
        incProgress(0.5)
        tmp.path <- dirname(file)
        
        name.base <- file.path(tmp.path, "det")
        name.glob <- paste0(name.base, ".*")
        name.shp  <- paste0(name.base, ".shp")
        name.zip  <- paste0(name.base, ".zip")
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        sf::st_write(data, dsn = name.shp, ## layer = "shpExport",
                     driver = "ESRI Shapefile", quiet = TRUE)
        
        zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
        req(file.copy(name.zip, file))
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        
        incProgress(0.5)
      })
    }
  )
  
  #### * rds file ----
  output$downloadMapRds <- downloadHandler(
    filename = "detection_spatial.rds",
    content = function(file){
      req(all_data())
      req(latLongData())
      data = userDetMap(input)
      saveRDS(data, file = file)
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
