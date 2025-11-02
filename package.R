library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(sf)
library(leaflet)
library(ggplot2)
library(terra)

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

raw_full_data <- read_csv("Network Features Data.csv")

initial_data <- raw_full_data %>% 
  rename(
    RXLEV_dBm = `RXLEV (dBm)`, 
    SNR_dB = `SNR (dB)`, 
    DL_Speed_kbps = `DL Speed (kbps)`,
    BAND_MHz = BAND
  ) %>%
  select(City, MNC, RXLEV_dBm, SNR_dB, DL_Speed_kbps, Latitude, Longitude, BAND_MHz, everything())

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
  mutate(
    is_valid = is_valid_geospatial(Latitude, Longitude, City)
  ) %>%
  ungroup() %>%
  filter(is_valid == TRUE) %>%
  select(-is_valid) %>%
  left_join(carrier_map, by = "MNC") %>%
  mutate(
    Carrier = if_else(is.na(CarrierName), "Other/Unknown", CarrierName)
  ) %>%
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
  )

final_data_ready <- cleaned_data %>%
  mutate(
    City = as.factor(City),
    Carrier = as.factor(Carrier),
    BAND_MHz = as.factor(BAND_MHz)
  )
clean_data =cleaned_data
tif_files <- list.files(path = "C:\\Users\\kavin\\Downloads\\population_ind_pak_general\\population_ind_pak_general", pattern = "^population_.*_general.*\\.tif$",full.names = TRUE)

print("Found these population files:")
print(tif_files)

# 2. Load all .tif files into a single "virtual" raster map
pop_raster <- vrt(tif_files)


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
final_data <- clean_data %>%
  mutate(Population_Density = pop_values[, 2])

# 4. Check your new, combined data!
print("Data with Population Density Added:")
print(head(final_data))

# ----------------------------------------
# UI CODE COMMENTED OUT
# ----------------------------------------

ui <- fluidPage(
  titlePanel("AuraNet: Network Performance Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Data Filtering & Analysis"),
      
      selectInput("city_filter", "Select City:",
                  choices = c("All",sort(unique(as.character((final_data_ready$City))))),
                  selected = "All",
                  multiple = FALSE),
      
      selectInput("carrier_filter", "Select Carrier:",
                  choices = c("All", sort(unique(as.character(final_data_ready$Carrier)))), 
                  selected = "All",
                  multiple = TRUE),
      
      selectInput("band_filter", "Filter by Technology Band:",
                  choices = sort(unique(final_data_ready$BAND_MHz)),
                  selected = unique(final_data_ready$BAND_MHz),
                  multiple = TRUE),
      
      selectInput("quality_filter", "Minimum Signal Strength:",
                  choices = c("All",levels(final_data_ready$Signal_Strength_Score)),
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
                 h4("Linear Regression Model Summary: DL Speed Drivers"),
                 p(strong("Dependent Variable:"), " DL Speed (kbps)"),
                 p(strong("Predictors:"), " RXLEV (dBm), SNR (dB), Population Density, and BAND (MHz)"),
                 verbatimTextOutput("modelSummary")
        )
      )
    )
  )
)

# 2. Server Definition
server <- function(input, output, session) {
  
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
    if (!"All" %in% selected_cities){
      data <- data %>% filter(City %in% selected_cities) 
    }
    selected_carriers <- input$carrier_filter
    if (!"All" %in% selected_carriers) {
      data <- data %>% filter(Carrier %in% selected_carriers)
    }
    
    data <- data %>% filter(BAND_MHz %in% input$band_filter)
    
    if (input$quality_filter != "All") {
      data <- data %>%
        filter(Signal_Strength_Score == input$quality_filter)
    }
    else
    {
      min_level <- which(levels(final_data_ready$Signal_Strength_Score) == input$quality_filter)
      data %>%
        filter(as.integer(Signal_Strength_Score) >= 1)
    }
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
  
}

# 3. Run the App
shinyApp(ui = ui, server = server)

