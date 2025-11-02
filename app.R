library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(sf)
library(leaflet)
library(ggplot2)
library(terra)
library(leaflet.extras)

city_bounds <- list(
  
  Ahmedabad = list(lat_min = 22.80, lat_max = 23.30, lon_min = 72.30, lon_max = 72.85),
  Mumbai = list(lat_min = 18.80, lat_max = 19.35, lon_min = 72.70, lon_max = 73.10),
  Pune = list(lat_min = 18.40, lat_max = 18.75, lon_min = 73.70, lon_max = 74.20),
  Surat = list(lat_min = 21.10, lat_max = 21.35, lon_min = 72.75, lon_max = 73.00),
  
  Bengaluru = list(lat_min = 12.80, lat_max = 13.15, lon_min = 77.40, lon_max = 77.85),
  Chennai = list(lat_min = 12.90, lat_max = 13.25, lon_min = 80.10, lon_max = 80.40),
  Hyderabad = list(lat_min = 17.30, lat_max = 17.55, lon_min = 78.30, lon_max = 78.60),
  
  Delhi = list(lat_min = 28.35, lat_max = 28.95, lon_min = 76.80, lon_max = 77.45),
  Kolkata = list(lat_min = 22.45, lat_max = 22.70, lon_min = 88.25, lon_max = 88.55),
  Jaipur = list(lat_min = 26.80, lat_max = 27.05, lon_min = 75.70, lon_max = 76.00),
  Lucknow = list(lat_min = 26.75, lat_max = 27.00, lon_min = 80.85, lon_max = 81.10)
)

carrier_map <- tribble(
  ~MNC, ~CarrierName,
  872, "Airtel",
  870, "Airtel",
  871, "Airtel",
  873, "Airtel",
  874, "Airtel",
  861, "Airtel",
  860, "Airtel", 
  865, "Airtel",
  866, "Airtel",
  868, "Vodafone Idea",
  862, "Vodafone Idea",
  869, "Vodafone Idea",
  863, "Vodafone Idea",
  864, "Vodafone Idea", 
  867, "Vodafone Idea",
  858, "Reliance Jio",
  888, "BSNL",
  999, "Other/Unknown" 
)

raw_full_data <- read_csv("Network Features Data.csv")  #Stores the data as tibble

initial_data <- raw_full_data %>% 
  rename(
    RXLEV_dBm = `RXLEV (dBm)`, 
    SNR_dB = `SNR (dB)`, 
    DL_Speed_kbps = `DL Speed (kbps)`,
    BAND_MHz = BAND
  ) %>%
  #Reorders and selects
  select(City, MNC, RXLEV_dBm, SNR_dB, DL_Speed_kbps, Latitude, Longitude, BAND_MHz, everything()) #Selects all other columns that are not explicitly mentioned

is_valid_geospatial <- function(lat, lon, city) {
  
  bounds <- city_bounds[[city]]
  
  if (!is.null(bounds)) {
    return(lat >= bounds$lat_min & lat <= bounds$lat_max &
             lon >= bounds$lon_min & lon <= bounds$lon_max)
  }
  return(FALSE) 
}

cleaned_data <- initial_data %>%
  
  #Geospatial filtering
  rowwise() %>% 
  mutate(                      #add or modify a col
    is_valid = is_valid_geospatial(Latitude, Longitude, City)  
  ) %>%
  ungroup() %>%                #reverts the data frame
  filter(is_valid == TRUE) %>% # Keep only the valid rows
  select(-is_valid) %>%        #drop col is_valid
  
  #Carrier mapping
  left_join(carrier_map, by = "MNC") %>%  #joins carrier_map with main data with MNC as common key
  
  mutate(
    Carrier = if_else(is.na(CarrierName), "Other/Unknown", CarrierName)
  ) %>%
  select(-CarrierName) %>% 
  
  #Feature engineering
  mutate(
    Signal_Strength_Score = cut(RXLEV_dBm,                  #creates a categorical variable from a numeric value
                                breaks = c(-Inf, -100, -90, -80, -70, -50, Inf),
                                labels = c("1 (Poor)", "2 (Fair)", "3 (Good)", "4 (Very Good)", "5 (Excellent)", "1 (Poor)"),
                                right = FALSE,                #Right interval excluded
                                ordered_result = TRUE),       #Makes ordinal
    
    SNR_Quality = cut(SNR_dB,
                      breaks = c(-Inf, 5, 13, 20, Inf), 
                      labels = c("Poor", "Fair", "Good", "Excellent"),
                      right = FALSE,
                      ordered_result = TRUE)
  )
final_data_ready <- cleaned_data %>%
  mutate( 
    City = as.factor(City),       #factor-Represent categorical variables
    Carrier = as.factor(Carrier),
    BAND_MHz = str_trim(BAND_MHz),
    BAND_MHz = as.factor(BAND_MHz)
  )

city_choices <- sort(unique(final_data_ready$City))
carrier_choices <- sort(unique(final_data_ready$Carrier))
band_choices <- sort(unique(final_data_ready$BAND_MHz))

clean_data =cleaned_data
tif_files <- list.files(path = "C:\\Users\\Ritha\\OneDrive\\Desktop\\AuraNet\\population_ind_pak_general", pattern = "^population_.*_general.*\\.tif$",full.names = TRUE)

if (length(tif_files) == 0) {
  stop("No .tif files found in the specified directory!")
}

print(tif_files)
pop_raster <- terra::vrt(tif_files)


# -----------------------------------------------------------------
# STEP 3: CONVERT POINTS AND EXTRACT DATA
# -----------------------------------------------------------------

# 1. Convert your *clean* data frame to a spatial 'sf' object
sf_points <- st_as_sf(clean_data, 
                      coords = c("Longitude", "Latitude"), 
                      crs = 4326) # 4326 is the standard GPS code

# 2. Extract the population value for each point
pop_values <- terra::extract(pop_raster, sf_points)

# 3. Add the new population data back to your clean data frame
# The data is in column 2 of the pop_values, we rename it for clarity
final_data_ready <- clean_data %>%
  mutate(Population_Density = pop_values[, 2])

ui <- fluidPage(
  titlePanel("AuraNet: Network Performance Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Data Filtering & Analysis"),
      
      selectInput("city_filter", "Select City:",
                  choices = city_choices,
                  selected = "Bengaluru",
                  multiple = FALSE),
 
      selectInput("carrier_filter", "Select Carrier:",
                  choices = c("All", carrier_choices), 
                  selected = "All",
                  multiple = TRUE),
      
      selectInput("quality_filter", "Minimum Signal Strength:",
                  choices = levels(final_data_ready$Signal_Strength_Score),
                  selected = "All",
                  multiple = FALSE),
      hr(),
      h5(strong("Statistical Model")),
      p("The Linear Regression model is applied based on the current filters."),
      p(em("Explore model coefficients in the 'Model Explorer' tab."))
    ),

    mainPanel(
      width = 9,
      # Tabset Panel for multiple views
      tabsetPanel(
        id = "main_tabs",
        
        # TAB 1: Geospatial Heatmap (Hexbin Concept Simulation)
        tabPanel("Geospatial Heatmap", icon = icon("map-marker-alt"),
                 h4("Cellular Signal Strength (RXLEV) by Location"),
                 p("The map simulates Hexagonal Binning by aggregating mean signal strength within the filtered area."),
                 leafletOutput("cityMap", height = "75vh")
        ),
        
        # TAB 2: Performance Insights (Charts)
        tabPanel("Performance Insights", icon = icon("chart-bar"),
                 fluidRow(
                   column(12, h4("Performance Comparison by Carrier and Quality")),
                   column(6, 
                          h5(strong("Chart 1: Avg. Download Speed (kbps) by Carrier")),
                          plotOutput("dl_speed_bar", height = "300px")
                   ),
                   column(6, 
                          h5(strong("Chart 2: Signal Quality (SNR) Distribution")),
                          plotOutput("snr_box", height = "300px")
                   )
                 ),
                 fluidRow(
                   column(12, 
                          h5(strong("Chart 3: Signal Strength vs. Download Speed")),
                          plotOutput("dl_vs_rxlev_scatter", height = "350px")
                   )
                 )
        ),
        
        # TAB 3: Model Explorer (Linear Regression Output)
        tabPanel("Model Explorer", icon = icon("flask"),
                  h4("Predict Download Speed (DL Speed)"),
                  p("Use the controls below to predict the DL Speed (kbps) for the selected city (",
                    span(textOutput("model_city_label", inline = TRUE), style = "font-weight: bold;"),
                    ") based on the Linear Regression model."),
                 
                 fluidRow(
                   column(4, 
                          numericInput("pred_rxlev", "RXLEV (dBm):", value = -85, min = -150, max = -50, step = 1),
                          p(em("Closer to 0 is better signal."))
                   ),
                   column(4, 
                          numericInput("pred_snr", "SNR (dB):", value = 15, min = 0, max = 30, step = 1),
                          p(em("Higher SNR is better signal quality."))
                   ),
                   column(4, 
                          numericInput("pred_density", "Population Density:", value = 5000, min = 10, max = 50000, step = 100),
                          p(em("Mock environmental variable."))
                   )
                 ),
                 
                 fluidRow(
                   column(6, 
                          selectInput("pred_band", "BAND (MHz):", choices = band_choices, selected = band_choices[1], multiple = FALSE)
                   ),
                   column(6, 
                          actionButton("predict_button", "Predict DL Speed (kbps)", class = "btn-primary mt-4", style = "margin-top: 30px;")
                   )
                 ),
                 
                 hr(),
                 h5(strong("Prediction Result:")), 
                 uiOutput("predictionResult")
        )
      )
    )
  )
)

# 2. Server Definition
server <- function(input, output, session) {
  
  output$model_city_label <- renderText({
    input$city_filter
  })
  #Reactive filtering
  
  output$cityMap <- renderLeaflet({
    
    # 1. Create a leaflet object
    leaflet() %>%
      
      # 2. Add the background map (the light-grey map tiles)
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # 3. Set the starting view (centered on India)
      setView(lng = 78.96, lat = 20.59, zoom = 4)
  })
  
  filtered_data <- reactive({
    data <- cleaned_data
    
    selected_cities <- input$city_filter
    data <- data %>% filter(City %in% selected_cities) 
    
    selected_carriers <- input$carrier_filter
    if (!"All" %in% selected_carriers) {
      data <- data %>% filter(Carrier %in% selected_carriers)
    }
    
    selected_score <- input$quality_filter
    data <- data %>% filter(Signal_Strength_Score %in% selected_score)
    data
  })
  observe({
    data_for_map <- filtered_data() # Gets the new data from the "brain"
    
    leafletProxy("cityMap", data = data_for_map) %>%
      clearHeatmap() %>%    # Clears the old heatmap
      addHeatmap(           # Draws the new one
        lng = ~Longitude,
        lat = ~Latitude,
        intensity = ~RXLEV_dBm, # Colors it based on signal strength
        blur=20,
        max = -50,
        radius =15 
      )
  })
  # --- TAB 2: Performance Insights ---
  
  # Chart 1: Download Speed Bar Plot
  output$dl_speed_bar <- renderPlot({
    
    # Don't run if no data is filtered
    req(nrow(filtered_data()) > 0)
    
    filtered_data() %>%
      group_by(Carrier) %>%
      summarise(Avg_DL_Speed = mean(DL_Speed_kbps, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(Carrier, -Avg_DL_Speed), y = Avg_DL_Speed, fill = Carrier)) +
      geom_bar(stat = "identity") +
      labs(x = "Carrier", y = "Avg. Download Speed (kbps)") +
      theme_minimal() +
      theme(legend.position = "none") # Hide legend since X-axis is clear
  })
  output$snr_box <- renderPlot({
    
    # Don't run if no data is filtered
    req(nrow(filtered_data()) > 0)
    
    ggplot(filtered_data(), aes(x = Carrier, y = SNR_dB, fill = Carrier)) +
      geom_boxplot() +
      labs(x = "Carrier", y = "Signal Quality (SNR dB)") +
      theme_minimal() +
      theme(legend.position = "none") # Hide legend
  })
  output$dl_vs_rxlev_scatter <- renderPlot({
    
    # Don't run if no data is filtered
    req(nrow(filtered_data()) > 0)
    
    ggplot(filtered_data(), aes(x = RXLEV_dBm, y = DL_Speed_kbps, color = SNR_Quality)) +
      geom_point(alpha = 0.7) + # Use semi-transparent points
      geom_smooth(method = "lm", se = FALSE, color = "blue") + # Add a trend line
      labs(x = "Signal Strength (RXLEV dBm)", 
           y = "Download Speed (kbps)", 
           color = "SNR Quality") +
      theme_minimal()
  })
 
  lm_model <- reactive({
    data <- final_data_ready %>%
      filter(City == input$city_filter) %>%
      filter(!is.na(DL_Speed_kbps),
             !is.na(RXLEV_dBm),
             !is.na(SNR_dB),
             !is.na(Population_Density))
    
    # Ensure BAND_MHz has all levels from full dataset
    data$BAND_MHz <- factor(data$BAND_MHz, levels = unique(final_data_ready$BAND_MHz))
    
    # Drop constant-factor columns to avoid contrasts error
    factors_with_single_level <- names(which(sapply(data, function(x) is.factor(x) && length(unique(x)) < 2)))
    if (length(factors_with_single_level) > 0) {
      formula_str <- paste(
        "DL_Speed_kbps ~", 
        paste(setdiff(c("RXLEV_dBm", "SNR_dB", "Population_Density", "BAND_MHz"), factors_with_single_level), collapse = " + ")
      )
    } else {
      formula_str <- "DL_Speed_kbps ~ RXLEV_dBm + SNR_dB + Population_Density + BAND_MHz"
    }
    
    cat("DEBUG: Model formula =", formula_str, "\n")
    lm(as.formula(formula_str), data = data)
  })
  

  # Observe the Predict button click and run the prediction
  # Observe the Predict button click and run the prediction
  observeEvent(input$predict_button, {
    cat("DEBUG: Predict button clicked\n")
    model <- lm_model()
    
    if (is.null(model) || length(model$residuals) < 15) { # Added check for minimum observations
      output$predictionResult <- renderUI({
        div(class = "alert alert-warning", role = "alert",
            paste0("Model Error: Insufficient data points (N < 15) or model failed to run for ", input$city_filter))
      })
      return()
    }
    
    # Extract variables actually used in model
    model_vars <- all.vars(formula(model))
    
    # --- FIX IS HERE: Use levels from the MODEL's data structure ---
    model_band_levels <- levels(model$model$BAND_MHz)
    
    # Build new_data only with variables used in the model
    new_data <- data.frame(
      RXLEV_dBm = input$pred_rxlev,
      SNR_dB = input$pred_snr,
      Population_Density = input$pred_density,
      # Re-factor the input with the levels *actually used by the model*
      BAND_MHz = factor(input$pred_band, levels = model_band_levels) 
    )[model_vars[model_vars != "DL_Speed_kbps"]] 
    
    # Check if the selected band is actually a level in the model
    if (!input$pred_band %in% model_band_levels) {
      output$predictionResult <- renderUI({
        div(class = "alert alert-warning", role = "alert",
            paste0("Prediction Warning: The selected BAND (", input$pred_band, 
                   ") was not present in the data for ", input$city_filter, ". Prediction may be unreliable or fail."))
      })
      return()
    }
    
    # Safe prediction
    prediction <- tryCatch({
      predict(model, newdata = new_data)
    }, error = function(e) {
      cat("DEBUG: Prediction error -", e$message, "\n")
      return(NULL)
    })
    
    # ... (Rest of your prediction result rendering code) ...
    if (!is.null(prediction)) {
      pred_value <- round(as.numeric(prediction), 0)
      
      output$predictionResult <- renderUI({
        color_class <- case_when(
          pred_value < 1000 ~ "text-danger",
          pred_value < 2500 ~ "text-warning",
          TRUE ~ "text-success"
        )
        
        tagList(
          p(style = "font-size: 1.2em;",  
            "Based on your inputs, the predicted Download Speed in ",  
            strong(input$city_filter), " is:"
          ),
          h1(class = color_class, paste(format(pred_value, big.mark = ","), "kbps")),
          p(style = "font-style: italic; margin-top: 15px;",
            paste0("(Note: Prediction based on model trained for ", input$city_filter, ")"))
        )
      })
    } else {
      output$predictionResult <- renderUI({
        div(class = "alert alert-danger", role = "alert",
            "Prediction Failed. Please check your inputs or the model structure (e.g., band not in city data).")
      })
    }
  })

}
#options(shiny.error = recover)

# 3. Run the App
shinyApp(ui = ui, server = server)
