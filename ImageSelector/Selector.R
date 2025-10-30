################################################################################
# Libraries
pacman::p_load(shiny, shinyFiles, shinybusy, leaflet, leaflet.extras, exifr, sf,
               geojsonsf,dplyr,stringr, lubridate)
################################################################################
# UI 
ui <- fluidPage(
  titlePanel("Image Preselector"),
  add_busy_bar(color = "#FF0000"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Input Settings"),
      
      
      ########################################################################
      textInput(
        "input_folder", "Input Folder",
        value = "", # REMOVE!!!
        placeholder = "Select input folder path",
        width = "100%"
      ),
      br(),

      # Horizontal layout for input parameters
      div(
        style = "padding: 0px; margin-bottom: -45px;",
        div(
          style = "display: flex; gap: 5px; align-items: center; justify-content: space-between;",
          div(
            style = "flex: 1; min-width: 0;", # Added min-width: 0 to prevent overflow
            textInput(
              "prefix", "Add a Prefix",
              value = "", # REMOVE!!!
              placeholder = "DJI_",
              width = "100%"
            )
          ),
          div(
            style = "flex: 1; min-width: 0;",
            textInput(
              "sufix", "Add a Sufix",
              value = "",
              placeholder = "_W",
              width = "100%"
            )
          ),
          div(
            style = "flex: 1; min-width: 0;",
            selectInput("file_type", "Type",
                        choices = c("jpg", "png", "tif"),
                        selected = "jpg",
                        width = "100%")
          ),
          div(
            style = "flex: 1; min-width: 0; margin-top: 10px;",
            actionButton(
              "read_images", "Read Images",
              class = "btn-primary",
              style = "width: 100%;"
            )
          )
        )
      ),
      
      br(),
      br(),
      hr(style = "border-top: 1px solid #000000;"),
      ########################################################################
      # Horizontal layout for shape parameters
      div(
        style = "padding: 0px; margin-bottom: -45px;",
        div(
          style = "display: flex; gap: 5px; align-items: center;",
          textInput("gpkg_file", "Import shape or draw on map", value = "", placeholder = "Select an AOI file (GPKG)",
                    width = "70%"),
          div(
            style = "flex: 0 0 30%; margin-top: 10px;",
            actionButton("importgpkg", "Import", class = "btn-primary", style = "width: 100%;"),
          )
        )
      ),
      
      br(),
      br(),
      hr(style = "border-top: 1px solid #000000;"),
      ########################################################################
      # Fixed time slider - using time only without date
      sliderInput("timeFilter", "Time Range:",
                  min = 0, max = 24*3600, value = c(0*3600, 24*3600),
                  timeFormat = "%T", timezone = "+0000",
                  step = 1), 
      
      sliderInput( 
        "heightFilter", "Height filter", 
        min = 0, max = 120, 
        value = c(0, 120) ,
        step = 1
      ),
      hr(style = "border-top: 1px solid #000000;"),
      ########################################################################
      actionButton("intersect", "Intersect", class = "btn-warning", style = "width: 100%;"),
      hr(style = "border-top: 1px solid #000000;"),
      ########################################################################
      # Horizontal layout for output parameters
      div(
        style = "padding: 0px; margin-bottom: -45px;",
        div(
          style = "display: flex; gap: 5px; align-items: center;",
          textInput("output_folder", "Output Folder",
                    value = "",
                    placeholder = "Select output folder path",
                    width = "70%"),
          div(
            style = "flex: 0 0 30%; margin-top: 10px;",
            actionButton("start", "Copy to output", class = "btn-success", style = "width: 100%;"),
          )
        )
      ),
      br(),
      br(),
      hr(style = "border-top: 1px solid #000000;"),
      ########################################################################
      # Clear button
      fluidRow(
        div(style = "padding: 19px; margin-bottom: -45px;",
            div(style = "display: flex; gap: 5px;",
                actionButton("clear_intersection", "Clear Intersection", class = "btn-danger", style = "width: 100%;"),
                actionButton("clear", "Clear All", class = "btn-danger", style = "width: 100%;")
            )
        )
      ),
      
      br(), br(),
      
      # Status output
      div(style = "padding: 4px; margin-bottom: 12px;", verbatimTextOutput("status"))
    ),
    
    ########################################################################
    mainPanel(
      width = 8,
      leafletOutput("map", height = "600px"),
      br(),
      h4("Selected Images:"),
      tableOutput("selected_images")
    )
  )
)

################################################################################
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    images_data = NULL,
    selected_images = NULL,
    drawn_shape = NULL,
    map_proxy = NULL,
    filtered_images = NULL
  )
  
  # Clear all fields and reset map
  observeEvent(input$clear, {
    # Reset reactive values
    values$images_data <- NULL
    values$selected_images <- NULL
    values$drawn_shape <- NULL
    values$filtered_images <- NULL
    
    # Clear input fields
    updateTextInput(session, "input_folder", value = "")
    updateTextInput(session, "output_folder", value = "")
    updateTextInput(session, "gpkg_file", value = "")
    updateSelectInput(session, "file_type", selected = "jpg")
    
    # Reset sliders to default
    updateSliderInput(session, "timeFilter",
                      value = c(8*3600, 16*3600))
    updateSliderInput(session, "heightFilter",
                      value = c(35, 65))
    
    # Reset map
    if (!is.null(values$map_proxy)) {
      values$map_proxy %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearGeoJSON() %>% 
        clearGroup("draw") %>% 
        setView(lng = 0, lat = 0, zoom = 2)
    }
    
    showNotification("All fields cleared and map reset", type = "message")
  })
  
  observeEvent(input$clear_intersection, {
    # Reset reactive values
    values$selected_images <- NULL
    values$drawn_shape <- NULL
    
    # Clear input fields
    updateTextInput(session, "gpkg_file", value = "")
    
    # Reset map with current filtered images
    updateMapMarkers()
    
    showNotification("Intersection cleared", type = "message")
  })
  
  
  # Read images and extract EXIF data
  observeEvent(input$read_images, {
    req(input$input_folder)
    req(input$file_type)
    
    # Validate folder exists
    if (!dir.exists(input$input_folder)) {
      showNotification("Input folder does not exist!", type = "error")
      return()
    }
    
    # Show progress
    showNotification("Reading images...", type = "message")
    
    tryCatch({
      # Get all image files of specified type
      pattern <- paste0("^", input$prefix, ".*", input$sufix, "\\.", input$file_type, "$")
      print(pattern)
      image_files <- list.files(input$input_folder,
                                pattern = pattern,
                                full.names = TRUE,
                                ignore.case = TRUE)
      
      if (length(image_files) == 0) {
        showNotification("No images found in the folder!", type = "warning")
        return()
      }
      
      # list of fields to be read on exifr
      tags = c("FileName", "GPSLatitude", "GPSLongitude","GPSAltitude", "DateTimeOriginal")
      
      # Check if .csv file with EXIF data exists, if so read it and if not read images and save file
      if(file.exists(file.path(input$input_folder, "exif_data.csv"))){
        
        exif_data <- read.csv(file.path(input$input_folder, "exif_data.csv"), stringsAsFactors = FALSE)
        
        # Show mesage of found file
        showNotification("Metadata file found...", type = "message")
        
        # Check for missing images
        existing_files <- exif_data$FileName
        new_files <- setdiff(basename(image_files), existing_files)
        
        if(length(new_files) > 0){
          new_exif <- read_exif(file.path(input$input_folder, new_files),
                                tags = tags)
          exif_data <- rbind(exif_data, new_exif)
          write.csv(exif_data, file.path(input$input_folder, "exif_data.csv"), row.names = FALSE)
        }
        
      } else{
        exif_data <- read_exif(image_files,
                               tags = tags)
        # Save EXIF data to CSV for future use
        write.csv(exif_data, file.path(input$input_folder, "exif_data.csv"), row.names = FALSE)
      }
      
      # Filter out images without GPS data
      valid_images <- exif_data %>%
        filter(!is.na(GPSLatitude), !is.na(GPSLongitude)) %>%
        mutate(
          FileName = as.character(FileName),
          GPSLatitude = as.numeric(GPSLatitude),
          GPSLongitude = as.numeric(GPSLongitude),
          GPSAltitude = as.numeric(GPSAltitude),
          DateTimeOriginal = as.POSIXct(DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = "UTC"),
          # Extract time only for filtering
          TimeOnly = as.numeric(hms(format(DateTimeOriginal, "%H:%M:%S")))
        )
      
      ########################################################################
      # update time and height filters based on data
      if(nrow(valid_images) > 0){
        
        # Convert times to seconds since midnight for slider
        min_time_sec <- as.numeric(hms(format(min(valid_images$DateTimeOriginal), "%H:%M:%S")))
        max_time_sec <- as.numeric(hms(format(max(valid_images$DateTimeOriginal), "%H:%M:%S")))
        
        updateSliderInput(session, "timeFilter",
                          min = min_time_sec,
                          max = max_time_sec,
                          value = c(min_time_sec, max_time_sec))
        ########################################################################
        updateSliderInput(session, "heightFilter",
                          min = floor(min(valid_images$GPSAltitude, na.rm = TRUE)),
                          max = ceiling(max(valid_images$GPSAltitude, na.rm = TRUE)),
                          value = c(floor(min(valid_images$GPSAltitude, na.rm = TRUE)), 
                                    ceiling(max(valid_images$GPSAltitude, na.rm = TRUE)))
        )
      }
      ########################################################################
      if (nrow(valid_images) == 0) {
        showNotification("No geotagged images found!", type = "warning")
        return()
      }
      
      values$images_data <- valid_images
      values$selected_images <- NULL
      values$filtered_images <- valid_images
      
      # Update map with all images initially
      updateMapMarkers()
      
      showNotification(paste("Found", nrow(valid_images), "geotagged images"),
                       type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error reading images:", e$message), type = "error")
    })
  })
  
  # Function to update map markers based on current filters
  updateMapMarkers <- function() {
    req(values$images_data)
    req(values$map_proxy)
    
    # Apply time and height filters
    time_range <- input$timeFilter
    height_range <- input$heightFilter
    
    filtered <- values$images_data %>%
      filter(TimeOnly >= time_range[1] & TimeOnly <= time_range[2] &
               GPSAltitude >= height_range[1] & GPSAltitude <= height_range[2])
    
    values$filtered_images <- filtered
    
    # Calculate bounding box for filtered data
    if (nrow(filtered) > 0) {
      bbox <- list(
        lng1 = min(filtered$GPSLongitude),
        lat1 = min(filtered$GPSLatitude),
        lng2 = max(filtered$GPSLongitude),
        lat2 = max(filtered$GPSLatitude)
      )
      
      # Add some padding to the bounding box
      padding <- 0.0001
      bbox_padded <- list(
        lng1 = bbox$lng1 - padding,
        lat1 = bbox$lat1 - padding,
        lng2 = bbox$lng2 + padding,
        lat2 = bbox$lat2 + padding
      )
    }
    
    # Update map
    values$map_proxy %>%
      clearMarkers() %>%
      {if (nrow(filtered) > 0) {
        fitBounds(., 
                  lng1 = bbox_padded$lng1,
                  lat1 = bbox_padded$lat1,
                  lng2 = bbox_padded$lng2,
                  lat2 = bbox_padded$lat2
        )
      } else {.}} %>%
      # Add filtered markers in green
      addCircleMarkers(
        data = filtered,
        lng = ~GPSLongitude,
        lat = ~GPSLatitude,
        radius = 6,
        color = "green",
        fillColor = "green",
        fillOpacity = 0.7,
        popup = ~paste0(FileName, "<br>Time: ", format(DateTimeOriginal, "%H:%M:%S"), 
                        "<br>Height: ", round(GPSAltitude, 1), "m"),
        layerId = ~FileName
      ) %>%
      # Add non-filtered markers in light gray
      addCircleMarkers(
        data = values$images_data %>% 
          filter(!FileName %in% filtered$FileName),
        lng = ~GPSLongitude,
        lat = ~GPSLatitude,
        radius = 4,
        color = "gray15",
        fillColor = "gray15",
        fillOpacity = 0.3,
        popup = ~paste0(FileName, "<br>Time: ", format(DateTimeOriginal, "%H:%M:%S"), 
                        "<br>Height: ", round(GPSAltitude, 1), "m"),
        layerId = ~paste0("filtered_", FileName)
      )
  }
  
  # Observe time and height filter changes
  observeEvent(c(input$timeFilter, input$heightFilter), {
    if (!is.null(values$images_data) && !is.null(values$map_proxy)) {
      updateMapMarkers()
    }
  })
  
  # Render map with RGB basemap
  output$map <- renderLeaflet({
    leaflet() %>%
      # Add RGB basemap (Esri World Imagery)
      addTiles(
        urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
        attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community',
        group = "Satellite"
      ) %>%
      # Add regular OpenStreetMap as alternative
      addProviderTiles(
        "OpenStreetMap",
        group = "Street Map"
      ) %>%
      # Add layer control to switch between basemaps
      addLayersControl(
        baseGroups = c("Satellite", "Street Map"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addDrawToolbar(
        targetGroup = "draw",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        polygonOptions = drawPolygonOptions(    # Enable polygon tool
          shapeOptions = drawShapeOptions(
            fillOpacity = 0.1,
            color = 'red',
            weight = 2
          )
        ),
        markerOptions = FALSE,
        circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        rectangleOptions = FALSE
      ) %>%
      addScaleBar() %>%
      # Set initial view to a reasonable default
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  # Store map proxy
  observe({
    values$map_proxy <- leafletProxy("map")
  })
  
  # Handle drawn shapes
  observeEvent(input$map_draw_new_feature, {
    values$drawn_shape <- input$map_draw_new_feature
  })
  
  observeEvent(input$gpkg_file, {
    values$gpkg_file <- input$gpkg_file
  })
  
  # Intersect operation
  observeEvent(input$intersect, {
    req(values$filtered_images)
    
    tryCatch({
      
      if(!is.null(values$gpkg_file) && values$gpkg_file != ""){
        rectangle_sf <- st_transform(st_read(values$gpkg_file), st_crs(4326))
        
        # Reset map
        if (!is.null(values$map_proxy)) {
          values$map_proxy %>%
            clearShapes() %>% 
            clearGroup("draw") %>% 
            addPolygons(data = rectangle_sf)
        }
        
      } else if (!is.null(values$drawn_shape)){
        
        # Extract rectangle coordinates
        coords <- values$drawn_shape$geometry$coordinates[[1]]
        
        rectangle <-  list(matrix(unlist(lapply(coords, function(x){
          c(x[[1]], x[[2]])
        })), ncol = 2, byrow = TRUE))
        
        rectangle_sf <- st_polygon(rectangle) %>% st_sfc(crs = 4326)
      } else {
        showNotification("Please draw a shape or import a GPKG file first!", type = "warning")
        return()
      }
      
      # Convert filtered images to spatial points
      points_sf <- st_as_sf(values$filtered_images,
                            coords = c("GPSLongitude", "GPSLatitude"),
                            crs = 4326,
                            remove = FALSE)
      
      # Find points inside rectangle
      inside <- st_intersects(points_sf, rectangle_sf, sparse = FALSE)
      selected <- values$filtered_images[inside, ]
      values$selected_images <- selected
      
      # Update map to highlight selected images
      if (!is.null(values$map_proxy) && nrow(selected) > 0) {
        values$map_proxy %>%
          clearMarkers() %>%
          # Add non-selected filtered markers in green
          addCircleMarkers(
            data = values$filtered_images %>% 
              filter(!FileName %in% selected$FileName),
            lng = ~GPSLongitude,
            lat = ~GPSLatitude,
            radius = 6,
            color = "green",
            fillColor = "green",
            fillOpacity = 0.7,
            popup = ~paste0(FileName, "<br>Time: ", format(DateTimeOriginal, "%H:%M:%S"), 
                            "<br>Height: ", round(GPSAltitude, 1), "m"),
            layerId = ~FileName
          ) %>%
          # Add selected markers in red
          addCircleMarkers(
            data = selected,
            lng = ~GPSLongitude,
            lat = ~GPSLatitude,
            radius = 8,
            color = "red",
            fillColor = "red",
            fillOpacity = 0.9,
            popup = ~paste0(FileName, "<br>Time: ", format(DateTimeOriginal, "%H:%M:%S"), 
                            "<br>Height: ", round(GPSAltitude, 1), "m"),
            layerId = ~paste0("selected_", FileName)
          ) %>%
          # Add non-filtered markers in  gray
          addCircleMarkers(
            data = values$images_data %>% 
              filter(!FileName %in% values$filtered_images$FileName),
            lng = ~GPSLongitude,
            lat = ~GPSLatitude,
            radius = 4,
            color = "gray15",
            fillColor = "gray15",
            fillOpacity = 0.3,
            popup = ~paste0(FileName, "<br>Time: ", format(DateTimeOriginal, "%H:%M:%S"), 
                            "<br>Height: ", round(GPSAltitude, 1), "m"),
            layerId = ~paste0("filtered_", FileName)
          )
      }
      
      showNotification(paste("Selected", nrow(selected), "images inside the rectangle"),
                       type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error in intersection:", e$message), type = "error")
    })
  })
  
  # Display selected images table
  output$selected_images <- renderTable({
    req(values$selected_images)
    values$selected_images %>%
      select(FileName, DateTimeOriginal, GPSLongitude, GPSLatitude, GPSAltitude) %>%
      mutate(DateTimeOriginal = format(DateTimeOriginal, "%Y-%m-%d %H:%M:%S"))
  })
  
  # Start processing - copy selected images
  observeEvent(input$start, {
    req(values$selected_images)
    req(input$output_folder)
    
    # Validate output folder
    if (!dir.exists(input$output_folder)) {
      showNotification("Output folder does not exist!", type = "error")
      return()
    }
    
    if (nrow(values$selected_images) == 0) {
      showNotification("No images selected to copy!", type = "warning")
      return()
    }
    
    tryCatch({
      # Copy selected images to output folder
      for (i in 1:nrow(values$selected_images)) {
        img_file <- values$selected_images$FileName[i]
        source_path <- file.path(input$input_folder, img_file)
        dest_path <- file.path(input$output_folder, img_file)
        
        if (file.exists(source_path)) {
          file.copy(source_path, dest_path, overwrite = TRUE)
        }
      }
      
      showNotification(paste("Successfully copied", nrow(values$selected_images),
                             "images to output folder"), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error copying files:", e$message), type = "error")
    })
  })
  
  # Status output
  output$status <- renderText({
    status_text <- ""
    
    if (!is.null(values$images_data)) {
      status_text <- paste0(status_text,
                            "Total images: ", nrow(values$images_data), "\n")
    }
    
    if (!is.null(values$filtered_images)) {
      status_text <- paste0(status_text,
                            "Filtered images: ", nrow(values$filtered_images), "\n")
    }
    
    if (!is.null(values$selected_images)) {
      status_text <- paste0(status_text,
                            "Selected images: ", nrow(values$selected_images), "\n")
    }
    
    if (is.null(status_text) || status_text == "") {
      status_text <- "Ready"
    }
    
    status_text
  })
}

################################################################################
# Run app
shinyApp(ui, server)