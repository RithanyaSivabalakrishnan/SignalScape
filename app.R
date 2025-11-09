library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(sf)
library(leaflet)
library(leaflet.extras) # For the heatmap
library(ggplot2)
library(terra)
library(cluster)        # For K-Means Clustering


# 2. SETUP: BOUNDING BOXES & CARRIER MAP

city_bounds <- list(
  Ahmedabad = list(lat_min = 22.80, lat_max = 23.30, lon_min = 72.30, lon_max = 72.85),
  Mumbai = list(lat_min = 18.80, lat_max = 19.35, lon_min = 72.70, lon_max = 73.10),
  Pune = list(lat_min = 18.40, lat_max = 18.75, lon_min = 73.70, lon_max = 74.20),
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
  872, "Airtel", 870, "Airtel", 871, "Airtel", 873, "Airtel", 874, "Airtel",
  861, "Airtel", 860, "Airtel", 865, "Airtel", 866, "Airtel",
  868, "Vodafone Idea", 862, "Vodafone Idea", 869, "Vodafone Idea",
  863, "Vodafone Idea", 864, "Vodafone Idea", 867, "Vodafone Idea",
  858, "Reliance Jio",
  888, "BSNL",
  999, "Other/Unknown"
)

# 3. DATA PREPARATION (Adding all features)

# --- Step 3a: Load and Rename ---

raw_full_data <- read_csv("Network Features Data.csv")

initial_data <- raw_full_data %>%
  rename(
    RXLEV_dBm = `RXLEV (dBm)`,
    SNR_dB = `SNR (dB)`,
    DL_Speed_kbps = `DL Speed (kbps)`,
    BAND_MHz = BAND,
    RXQUAL = RXQUAL,
    Physical_Speed = `Speed (m/s)`,
    Height_m = `Height (m)`
  )

# --- Step 3b: Geospatial Cleaning ---
is_valid_geospatial <- function(lat, lon, city) {
  bounds <- city_bounds[[city]]
  if (!is.null(bounds)) {
    return(lat >= bounds$lat_min & lat <= bounds$lat_max &
             lon >= bounds$lon_min & lon <= bounds$lon_max)
  }
  return(FALSE)
}

cleaned_data <- initial_data %>%
  rowwise() %>%
  mutate(is_valid = is_valid_geospatial(Latitude, Longitude, City)) %>%
  ungroup() %>%
  filter(is_valid == TRUE) %>%
  select(-is_valid)

# --- Step 3c: Population Join ---

tif_files <- list.files(path = "C:\\Users\\Ritha\\OneDrive\\Desktop\\SignalScape\\population_ind_pak_general", pattern = "^population_.*_general.*\\.tif$", full.names = TRUE)
if (length(tif_files) == 0) {
  stop("No .tif files found. Check the file path.")
}
pop_raster <- terra::vrt(tif_files)
sf_points <- st_as_sf(cleaned_data,
                      coords = c("Longitude", "Latitude"),
                      crs = 4326)
pop_values <- terra::extract(pop_raster, sf_points)

final_data <- cleaned_data %>%
  mutate(Population_Density = pop_values[, 2]) %>%
  mutate(Population_Density = ifelse(is.na(Population_Density), 0, Population_Density)) # Fix NAs

# --- Step 3d: Final Feature Engineering ---

final_data_ready <- final_data %>%
  left_join(carrier_map, by = "MNC") %>%
  mutate(Carrier = if_else(is.na(CarrierName), "Other/Unknown", CarrierName)) %>%
  select(-CarrierName) %>%
  mutate(
    Signal_Strength_Score = cut(RXLEV_dBm,
                                breaks = c(-Inf, -100, -90, -80, -70, -50, Inf),
                                labels = c("1 (Poor)", "2 (Fair)", "3 (Good)", "4 (Very Good)", "5 (Excellent)", "1 (Poor)"),
                                right = FALSE,
                                ordered_result = TRUE),
    SNR_Quality = cut(SNR_dB,
                      breaks = c(-Inf, 5, 13, 20, Inf),
                      labels = c("Poor", "Fair", "Good", "Excellent"),
                      right = FALSE,
                      ordered_result = TRUE)
  ) %>%
  mutate(
    City = as.factor(City),
    Carrier = as.factor(Carrier),
    BAND_MHz = as.factor(str_trim(BAND_MHz)),
    SNR_Quality = as.factor(SNR_Quality)
  )

# --- Step 3e: Create Choice Lists for UI ---
city_choices <- c("All", sort(unique(as.character(final_data_ready$City))))
carrier_choices <- c("All", sort(unique(as.character(final_data_ready$Carrier))))
band_choices <- c("All", sort(unique(as.character(final_data_ready$BAND_MHz))))
quality_choices <- c("All", levels(final_data_ready$Signal_Strength_Score))
# List of numeric variables for clustering
cluster_vars <- c("RXLEV_dBm", "SNR_dB", "DL_Speed_kbps")


print("Data preparation complete. Starting app...")

# 4. SHINY UI (USER INTERFACE)

ui <- fluidPage(
  titlePanel("SignalScape: Network Performance Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Data Filtering & Analysis"),
      
      selectInput("city_filter", "Select City:",
                  choices = city_choices,
                  selected = "All",
                  multiple = FALSE),
      
      selectInput("band_filter", "Filter by Technology Band:",
                  choices = band_choices,
                  selected = "All",
                  multiple = FALSE),
      
      selectInput("quality_filter", "Filter by Signal Strength:",
                  choices = quality_choices,
                  selected = "All",
                  multiple = FALSE)
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        
        # --- TAB 1: Map ---
        tabPanel("Geospatial Heatmap", icon = icon("map-marker-alt"),
                 h4("Cellular Signal Strength (RXLEV) by Location"),
                 p("The map shows the density and strength of signal readings."),
                 leafletOutput("cityMap", height = "75vh")
        ),
        
        # --- TAB 2: Plots ---
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
        
        # --- TAB 3: CLUSTER ANALYSIS TAB ---
        tabPanel("Cluster Analysis", icon = icon("project-diagram"),
                 h4("Find Hidden Groups in Your Data (K-Means Clustering)"),
                 p("This tool finds natural 'clumps' in your data based on the two features you select. It can help you find groups like 'Good Cells', 'Weak Cells', and 'Interference Cells'."),
                 
                 fluidRow(
                   column(4,
                          sliderInput("k_clusters", "Select Number of Clusters:",
                                      min = 2, max = 8, value = 3)
                   ),
                   column(4,
                          selectInput("x_var", "Plot X-Axis:", 
                                      choices = cluster_vars, selected = "RXLEV_dBm")
                   ),
                   column(4,
                          selectInput("y_var", "Plot Y-Axis:", 
                                      choices = cluster_vars, selected = "SNR_dB")
                   )
                 ),
                 
                 hr(),
                 plotOutput("cluster_plot", height = "500px")
        ),
        tabPanel("Model Explorer", icon = icon("flask"),
                 h4("Linear Regression Model Summary: DL Speed Drivers"),
                 p(strong("Dependent Variable:"), " DL Speed (kbps)"),
                 p(strong("Predictors:"), " RXLEV (dBm), SNR (dB), Population Density, and BAND (MHz)"),
                 verbatimTextOutput("modelSummary")
        )
      )
    )
  )
)

# 5. SHINY SERVER 

server <- function(input, output, session) {
  
  # --- Reactive Filter (for Tabs 1, 2, & 3) ---
  filtered_data <- reactive({
    data <- final_data_ready
    
    if (input$city_filter != "All") {
      data <- data %>% filter(City == input$city_filter)
    }
    if (input$band_filter != "All") {
      data <- data %>% filter(BAND_MHz == input$band_filter)
    }
    if (input$quality_filter != "All") {
      data <- data %>%
        filter(Signal_Strength_Score == input$quality_filter)
    }
    data
  })
  
  # --- TAB 1: Map Logic ---
  output$cityMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 78.96, lat = 20.59, zoom = 4)
  })
  
  observe({
    data_for_map <- filtered_data()
    proxy <- leafletProxy("cityMap", data = data_for_map) %>% clearHeatmap()
    if (nrow(data_for_map) > 0) {
      proxy %>% addHeatmap(lng = ~Longitude, lat = ~Latitude, intensity = ~RXLEV_dBm, blur = 20, max = -50, radius = 15)
    }
  })
  
  observeEvent(input$city_filter, {
    if (input$city_filter != "All") {
      req(nrow(filtered_data()) > 0)
      data_for_map <- filtered_data()
      center_lon <- mean(data_for_map$Longitude, na.rm = TRUE)
      center_lat <- mean(data_for_map$Latitude, na.rm = TRUE)
      leafletProxy("cityMap") %>% flyTo(lng = center_lon, lat = center_lat, zoom = 12)
    } else {
      leafletProxy("cityMap") %>% flyTo(lng = 78.96, lat = 20.59, zoom = 4)
    }
  })
  
  # --- TAB 2: Plot Logic ---
  output$dl_speed_bar <- renderPlot({
    req(nrow(filtered_data()) > 0)
    filtered_data() %>%
      group_by(Carrier) %>%
      summarise(Avg_DL_Speed = mean(DL_Speed_kbps, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(Carrier, -Avg_DL_Speed), y = Avg_DL_Speed, fill = Carrier)) +
      geom_bar(stat = "identity") +
      labs(x = "Carrier", y = "Avg. Download Speed (kbps)") +
      theme_minimal() + theme(legend.position = "none")
  })
  
  output$snr_box <- renderPlot({
    req(nrow(filtered_data()) > 0)
    ggplot(filtered_data(), aes(x = Carrier, y = SNR_dB, fill = Carrier)) +
      geom_boxplot() +
      labs(x = "Carrier", y = "Signal Quality (SNR dB)") +
      theme_minimal() + theme(legend.position = "none")
  })
  
  output$dl_vs_rxlev_scatter <- renderPlot({
    req(nrow(filtered_data()) > 0)
    ggplot(filtered_data(), aes(x = RXLEV_dBm, y = DL_Speed_kbps, color = SNR_Quality)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(x = "Signal Strength (RXLEV dBm)", y = "Download Speed (kbps)", color = "SNR Quality") +
      theme_minimal()
  })
  
  # --- TAB 3: Cluster Logic (INTERACTIVE VERSION) ---
  
  # 1. Create a reactive dataset for clustering
  # THIS IS THE FIX: We only select the two variables
  # that the user is currently plotting.
  cluster_data <- reactive({
    
    # Get *only* the two features the user is plotting
    data <- filtered_data() %>%
      select(all_of(c(input$x_var, input$y_var))) %>%
      na.omit()
    
    # Scale those two features
    data_scaled <- as.data.frame(scale(data))
    
    return(data_scaled)
  })
  
  # 2. Run the K-Means algorithm
  kmeans_result <- reactive({
    req(nrow(cluster_data()) > input$k_clusters) # Need more data than clusters
    
    # "Lock" the random starting point so results are stable
    set.seed(13) 
    
    # Run the K-Means algorithm
    kmeans(cluster_data(), centers = input$k_clusters, nstart = 25)
  })
  
  # 3. Create the cluster plot
  output$cluster_plot <- renderPlot({
    
    # Get the original (un-scaled) filtered data, 
    # but ONLY the two columns we are plotting
    data_to_plot <- filtered_data() %>%
      select(all_of(c(input$x_var, input$y_var))) %>%
      na.omit()
    
    # Add the cluster results as a new column
    data_to_plot$Cluster <- as.factor(kmeans_result()$cluster)
    
    # Plot the results using the user's X and Y choices
    ggplot(data_to_plot, aes(x = .data[[input$x_var]], 
                             y = .data[[input$y_var]], 
                             color = Cluster)) +
      geom_point(alpha = 0.8, size = 3) +
      labs(x = input$x_var, y = input$y_var, color = "Found Cluster") +
      theme_minimal(base_size = 14) +
      guides(color = guide_legend(override.aes = list(size = 5)))
  })
  
  #Tab:4 Model explorer
  lm_model <- reactive({
    
    data <- final_data_ready
    
    if (input$city_filter != "All") {
      data <- data %>% filter(City == input$city_filter)
    }
    if (input$band_filter != "All") {
      data <- data %>% filter(BAND_MHz == input$band_filter)
    }
    if (input$quality_filter != "All") {
      data <- data %>%
        filter(Signal_Strength_Score == input$quality_filter)
    }
    
    # Number of predictors (p) = 6 (Intercept, RXLEV, SNR, PopDensity, 2 BAND factors)
    if (nrow(data) < 15) { 
      return("Insufficient data points to run a meaningful regression model (N < 15). Please select a city with more data.")
    }
    
    # MODEL: Predict DL Speed using all specified predictors
    # Formula: DL_Speed_kbps ~ RXLEV_dBm + SNR_dB + Population_Density + BAND_MHz
    # Note: Carrier is excluded here to focus the model on technical and environmental drivers within the city.
    model <- lm(DL_Speed_kbps ~ RXLEV_dBm + SNR_dB + Population_Density + BAND_MHz, data = data)
    
    return(summary(model))
  })
  
  output$modelSummary <- renderPrint({
    model_output <- lm_model()
    
    # Check if it's the error message string
    if (is.character(model_output)) {
      cat(model_output)
    } else {
      cat("Linear Model Summary for ", input$city_filter, "\n\n")
      print(model_output)
    }
  })
  
} # End server

# 6. RUN THE APP
shinyApp(ui = ui, server = server)
