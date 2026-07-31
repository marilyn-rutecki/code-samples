library(shiny)
library(tidyverse)
library(lubridate)
library(sf)
library(leaflet)
library(treemapify)
library(scales)
library(readr)
library(tidycensus)
library(htmltools)
library(bslib)
library(plotly)
library(ggalluvial)
library(rsconnect)

# custom color palette
# Yale Blue, Inferno, Harvest Orange, Soft Peach, Tropical Teal
custom_palette <- c("#004777", "#A30000", "#FF7700", "#EFD28D", "#00AFB5")

# function to extend palette for categorical data with many levels
get_extended_palette <- function(n) {
  colorRampPalette(custom_palette)(n)
}

# light grey background color for the dashboard
bg_color <- "#f4f6f9"

# theme
my_theme <- bs_theme(
  version = 5,
  bg = bg_color,
  fg = "#333333",
  primary = "#004777",       # Yale Blue
  secondary = "#00AFB5",     # Tropical Teal
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

# load data

# 1. Permits Data
permits <- read_csv("Building_Permits_20251202.csv", show_col_types = FALSE)

# 2. Land Use Data (used for both Land Use Map and Treemap)
landuse_raw <- read_csv("San_Francisco_Land_Use_-_2023_20250922.csv", show_col_types = FALSE)

# 3. HPI Data
hpi_df <- read_csv("ATNHPIUS41884Q.csv", show_col_types = FALSE)

# 4. Census Data (rviz_5)
census_data <- tryCatch({
  get_acs(
    variables = c(
      "median_rent"   = "B25064_001",
      "median_income" = "B19013_001",
      "pop_total"     = "B03002_001",
      "pop_white"     = "B03002_003"
    ),
    geography = "tract",
    state = "California",
    county = c("Alameda", "Contra Costa", "Marin",
               "Napa", "San Francisco", "San Mateo",
               "Santa Clara", "Solano", "Sonoma"),
    year   = 2023,
    survey = "acs5",
    geometry = TRUE
  ) %>%
  select(-moe) %>%
  pivot_wider(names_from = "variable", values_from = "estimate")
}, error = function(e) {
  message("Error fetching census data: ", e$message)
  return(NULL)
})


# --- data pre-processing ---

# 1. Permits
permits_clean <- permits %>%
  filter(!is.na(`Issued Date`)) %>%
  mutate(Year = year(ymd_hms(`Issued Date`))) %>%
  filter(!is.na(Year)) %>%
  mutate(`Permit Type Definition` = str_to_title(`Permit Type Definition`)) # Capitalize globally

min_year <- min(permits_clean$Year, na.rm = TRUE)
max_year <- max(permits_clean$Year, na.rm = TRUE)
all_permit_types <- unique(permits_clean$`Permit Type Definition`)
top_permit_types <- permits_clean %>%
  count(`Permit Type Definition`, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(`Permit Type Definition`)

# 2. Land Use (Map)
all_landuse_types <- unique(landuse_raw$landuse)

# 3. Land Use (Bar Graph - rviz_3)
sf_restype_summary <- landuse_raw %>%
  mutate(restype = str_trim(restype)) %>%
  filter(!is.na(restype), restype != "", restype != " ") %>%
  count(restype, name = "parcel_count") %>%
  mutate(label_text = paste0(restype, "\n(", parcel_count, ")"))

# 4. HPI (rviz_2)
hpi_clean <- hpi_df %>%
  mutate(observation_date = as.Date(observation_date))

# 5. Process Data for New Visualizations

# A. Violin Plot Data (Processing Time)
permits_wait_time <- permits_clean %>%
  filter(!is.na(`Filed Date`), !is.na(`Issued Date`)) %>%
  mutate(WaitTime = as.numeric(difftime(ymd_hms(`Issued Date`), ymd_hms(`Filed Date`), units = "days"))) %>%
  filter(WaitTime >= 0, WaitTime < 1000) # filter outliers/errors

# B. Lollipop Chart Data (Neighborhood Activity)
# Moved to server for reactivity

# C. Sankey Diagram Data (Change of Use)
change_of_use <- permits_clean %>%
  filter(!is.na(`Existing Use`), !is.na(`Proposed Use`)) %>%
  filter(`Existing Use` != `Proposed Use`) %>%
  count(`Existing Use`, `Proposed Use`, sort = TRUE) %>%
  slice_head(n = 15) 


# --- UI ---
ui <- page_navbar(
  title = "Visualizing Housing in the San Francisco Bay Area",
  theme = my_theme,
  bg = "#004777", # Yale Blue background for navbar
  inverse = TRUE, # White text for navbar
  
  # custom CSS to adjust tab font size and ensure single line
  header = tags$head(
    tags$style(HTML("
      .nav-underline {
        background-color: #004777; /* Yale Blue background for the whole tab bar */
        padding-top: 10px;
        padding-bottom: 0px;
      }
      .nav-link {
        font-size: 0.9rem !important;
        white-space: nowrap !important;
        padding-left: 15px !important;
        padding-right: 15px !important;
        color: rgba(255, 255, 255, 0.8) !important; /* Light white for inactive tabs */
        border-bottom: 3px solid transparent !important;
        border-radius: 0 !important;
      }
      .nav-link:hover {
        color: white !important;
      }
      .nav-link.active {
        color: white !important;
        font-weight: bold;
        background-color: transparent !important;
        border-bottom: 3px solid #00AFB5 !important; /* Teal underline for active tab */
      }
      .navbar-brand {
        font-size: 1.2rem !important;
      }
      .card {
        border: none;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      /* Custom styles for About Tab */
      .about-header {
        text-align: center;
        margin-bottom: 30px;
        color: #004777;
      }
      .about-card-purple {
        background-color: #6A4C93; /* Purple similar to reference */
        color: white;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
      }
      .border-left-blue { border-left: 5px solid #004777; }
      .border-left-orange { border-left: 5px solid #FF7700; }
      .border-left-teal { border-left: 5px solid #00AFB5; }
      .border-left-red { border-left: 5px solid #A30000; }
      .feature-card {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        height: 100%;
      }
      .footer-section {
        background-color: #2C3E50;
        color: white;
        padding: 40px;
        text-align: center;
        margin-top: 40px;
        border-radius: 8px;
      }
    "))
  ),
  
  # Tab 1: Housing Types (Bar Graph)
  nav_panel("Housing Types",
    card(
      full_screen = TRUE,
      # card_header removed
      card_body(
        p("Proportion of housing parcels by residential type"),
        plotOutput("barPlot", height = "600px"),
        br(),
        p("SRO: Single Occupancy Room", style = "font-style: italic; color: #666;")
      )
    )
  ),
  
  # Tab 2: Permits Trend (Interactive) - With Local Sidebar
  nav_panel("Permits Trend",
    layout_sidebar(
      sidebar = sidebar(
        title = "Permit Filters",
        sliderInput("yearRange", "Select Year Range:",
                    min = min_year, max = max_year,
                    value = c(2000, max_year), sep = ""),
        checkboxGroupInput("permitTypes", "Select Permit Types:",
                           choices = sort(all_permit_types),
                           selected = top_permit_types)
      ),
      card(
        full_screen = TRUE,
        # card_header removed
        card_body(
          plotOutput("permitsPlot", height = "600px"),
          br(),
          p("Source: SF Building Permits Data")
        )
      )
    )
  ),
  
  # Tab 3: Change of Use (Sankey Diagram)
  nav_panel("Change of Use",
    card(
      full_screen = TRUE,
      # card_header removed
      card_body(
        plotOutput("sankeyPlot", height = "600px")
      )
    )
  ),
  
  # Tab 4: Processing Time (Violin Plot)
  nav_panel("Processing Time",
    card(
      full_screen = TRUE,
      # card_header removed
      card_body(
        p("Distribution of days from filing to issuance (capped at 1000 days)"),
        plotOutput("violinPlot", height = "600px")
      )
    )
  ),
  
  # Tab 5: Neighborhood Activity (Lollipop Chart)
  nav_panel("Neighborhoods",
    layout_sidebar(
      sidebar = sidebar(
        title = "Filter by Permit Type",
        checkboxGroupInput("neighborhoodPermitType", "Select Permit Types:",
                           choices = sort(all_permit_types),
                           selected = all_permit_types)
      ),
      card(
        full_screen = TRUE,
        # card_header removed
        card_body(
          plotOutput("lollipopPlot", height = "600px")
        )
      )
    )
  ),
  
  # Tab 6: Land Use Map (Interactive) - With Local Sidebar
  nav_panel("Land Use Map",
    layout_sidebar(
      sidebar = sidebar(
        title = "Land Use Filters",
        selectInput("landUseType", "Select Land Use Type:", 
                    choices = sort(all_landuse_types), 
                    selected = "OPENSPACE"),
        p("Note: Showing up to 1000 parcels to ensure performance."),
        textOutput("parcelCount")
      ),
      card(
        full_screen = TRUE,
        # card_header removed
        card_body(
          div(style = "font-family: Arial; font-weight: bold; font-size: 16px; color: #004777; margin-bottom: 10px;", "Land Use Explorer"),
          leafletOutput("landUseLeaflet", width = "100%", height = "600px")
        )
      )
    )
  ),
  
  # Tab 7: House Price Index (HPI)
  nav_panel("House Price Index",
    card(
      full_screen = TRUE,
      # card_header removed
      card_body(
        plotlyOutput("hpiPlot", height = "600px")
      )
    )
  ),
  
  # Tab 8: Median Rent Map
  nav_panel("Median Rent Map",
    card(
      full_screen = TRUE,
      # card_header removed
      card_body(
        div(style = "font-family: Arial; font-weight: bold; font-size: 16px; color: #004777; margin-bottom: 10px;", "Median Rent Map (2023)"),
        if(is.null(census_data)) {
          div(
            class = "alert alert-danger",
            h4("Error: Could not fetch Census data."),
            p("Please ensure you have a valid Census API key installed (e.g. in .Renviron).")
          )
        } else {
          leafletOutput("rentMap", height = "600px")
        }
      )
    )
  ),
  
  # Tab 9: About
  nav_panel("About",
    fluidPage(
      # Header
      div(class = "about-header",
        h1("Visualizing Housing in the San Francisco Bay Area", style = "font-weight: bold;"),
        h4("An Interactive Data Visualization Dashboard", style = "color: #7f8c8d;")
      ),
      
      # Purple Intro Card
      div(class = "about-card-purple",
        h3("About This Dashboard", style = "margin-top: 0;"),
        p("This interactive dashboard provides comprehensive insights into housing trends, building permits, and land use patterns in the San Francisco Bay Area from 2000 to 2023. It enables users to explore, analyze, and understand the housing landscape through various interactive visualizations.")
      ),
      
      # Data Source & Definitions
      card(
        card_header("Data Sources & Definitions"),
        card_body(
          h5("Data Sources"),
          p("The data comes from publicly available sources including:"),
          tags$ul(
            tags$li(strong("SF Open Data"), ": Building Permits and Land Use data."),
            tags$li(strong("US Census Bureau"), ": American Community Survey (ACS) data for demographics and rent."),
            tags$li(strong("FRED"), ": Federal Reserve Economic Data for House Price Index.")
          ),
          hr(),
          h5("Key Definitions"),
          tags$ul(
            tags$li(strong("Land Use"), ": The classification of how a parcel of land is being utilized (e.g., Residential, Commercial, Industrial, Open Space)."),
            tags$li(strong("Parcel"), ": A defined piece of real estate, usually resulting from the division of a larger area of land. It is the basic unit of land administration."),
            tags$li(strong("Zoning"), ": Municipal or local laws or regulations that govern how real property can and cannot be used in certain geographic areas (e.g., height limits, density, allowed uses).")
          )
        )
      ),
      
      br(),
      
      # Dashboard Features Grid
      h3("Dashboard Features", style = "color: #004777; margin-bottom: 20px;"),
      layout_column_wrap(
        width = 1/2,
        div(class = "feature-card border-left-blue",
          h4("Housing Types"),
          p("Visualizes the distribution of residential land parcels, distinguishing between Single Family, Apartments, Condos, and more.")
        ),
        div(class = "feature-card border-left-orange",
          h4("Permits Trend"),
          p("Interactive line graph showing the volume of different building permit types issued over time.")
        ),
        div(class = "feature-card border-left-teal",
          h4("Change of Use"),
          p("Sankey diagram illustrating the flow of building use changes, from existing to proposed uses.")
        ),
        div(class = "feature-card border-left-red",
          h4("Processing Time"),
          p("Violin plots showing the distribution of processing times (filing to issuance) for various permit types.")
        ),
        div(class = "feature-card border-left-blue",
          h4("Neighborhood Activity"),
          p("Lollipop chart highlighting the top neighborhoods by total permit activity.")
        ),
        div(class = "feature-card border-left-orange",
          h4("Interactive Maps"),
          p("Explore Land Use and Median Rent across the Bay Area with interactive leaflet maps.")
        )
      ),
      
      # Footer
      div(class = "footer-section",
        h3("Credits & Acknowledgments"),
        p("Data provided by DataSF, US Census Bureau, and FRED."),
        br(),
        h5("Author: Marilyn Rutecki", style = "color: #EFD28D;"),
        p("Email: mr1970@georgetown.edu")
      )
    )
  )
)


# --- Server ---
server <- function(input, output, session) {
  
  # ggplot theme
  common_theme <- theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(family = "Arial", face = "bold", size = 16, color = "#004777"), # Standardized Title
      plot.subtitle = element_text(family = "Arial", size = 12, color = "#453f78"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank() # Remove gridlines
    )
  
  # 1. Bar Graph Logic
  output$barPlot <- renderPlot({
    # Order data by count
    plot_data <- sf_restype_summary %>%
      arrange(desc(parcel_count))
    
    # create palette: Top 5 get custom palette, rest get extended
    n_types <- nrow(plot_data)
    
    extended_cols <- colorRampPalette(c("grey70", "grey90"))(n_types - 5)
    final_colors <- c(custom_palette, extended_cols)
    
    if (n_types < 5) {
      final_colors <- custom_palette[1:n_types]
    } else {
      final_colors <- final_colors[1:n_types]
    }
    
    # map colors to the specific types (in order)
    names(final_colors) <- plot_data$restype
    
    ggplot(plot_data, aes(x = reorder(restype, -parcel_count), y = parcel_count, fill = restype)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = comma(parcel_count)), vjust = -0.5, size = 5, family = "Inter") +
      scale_fill_manual(values = final_colors) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
      labs(
        title = "San Francisco Land Parcels by Residential Type (2023)", 
        subtitle = "Proportion of housing parcels by residential type",
        x = "Residential Type",
        y = "Parcel Count"
      ) +
      common_theme +
      theme(
        legend.position = "none"
      )
  })
  
  # 2. Permits Logic
  filtered_permits <- reactive({
    req(input$permitTypes)
    permits_clean %>%
      filter(Year >= input$yearRange[1], Year <= input$yearRange[2]) %>%
      filter(`Permit Type Definition` %in% input$permitTypes) %>%
      count(Year, `Permit Type Definition`)
  })
  
  output$permitsPlot <- renderPlot({
    data <- filtered_permits()
    validate(need(nrow(data) > 0, "No data available for the selected filters."))
    
    n_selected <- length(unique(data$`Permit Type Definition`))
    permit_colors <- get_extended_palette(n_selected)
    
    ggplot(data, aes(x = Year, y = n, color = `Permit Type Definition`)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 2) +
      scale_color_manual(values = permit_colors) +
      labs(
        title = "Building Permits Issued by Type over Time",
        x = "Year",
        y = "Number of Permits",
        color = "Permit Type"
      ) +
      common_theme +
      theme(
        legend.position = "bottom",
        legend.direction = "vertical"
      )
  })
  
  # 3. Land Use Map Logic
  landuse_sf <- reactive({
    req(input$landUseType)
    df_filtered <- landuse_raw %>% filter(landuse == input$landUseType)
    if(nrow(df_filtered) > 1000) df_filtered <- df_filtered %>% slice_head(n = 1000)
    st_as_sf(df_filtered, wkt = "the_geom", crs = 4326)
  })
  
  output$parcelCount <- renderText({
    req(input$landUseType)
    count <- sum(landuse_raw$landuse == input$landUseType)
    paste("Total parcels:", comma(count))
  })
  
  # for map use custom palette for the legend/types
  landuse_colors <- colorFactor(
    palette = get_extended_palette(length(all_landuse_types)), 
    domain = all_landuse_types
  )
  
  output$landUseLeaflet <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -122.4194, lat = 37.7749, zoom = 12) 
    
  })
  
  observe({
    data <- landuse_sf()
    type <- input$landUseType
    poly_color <- landuse_colors(type)
    
    leafletProxy("landUseLeaflet", data = data) %>%
      clearShapes() %>%
      addPolygons(
        color = "#444444",
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 0.7,
        fillColor = poly_color, 
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste("Land Use:", landuse)
      )
  })
  
  # 4. HPI Logic
  output$hpiPlot <- renderPlotly({
    p <- ggplot(hpi_clean, aes(x = observation_date, y = ATNHPIUS41884Q, group = 1,
                               text = paste("Date:", format(observation_date, "%Y-%m-%d"), 
                                            "<br>Index:", comma(ATNHPIUS41884Q)))) +
      geom_line(color = custom_palette[3], linewidth = 1.5) + 
      scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
      scale_y_continuous(labels = comma_format()) +
      labs(
        title = "All-Transactions House Price Index: San Francisco, CA (MSA)",
        subtitle = "Index 1975 = 100, Not Seasonally Adjusted",
        x = NULL,
        y = "Index Value",
        caption = "Source: Federal Reserve Bank of St. Louis (FRED) • Series ID: ATNHPIUS41884Q"
      ) +
      common_theme +
      theme(
        axis.text.x = element_text(vjust = 0.5)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        title = list(
          text = "<b>All-Transactions House Price Index: San Francisco, CA (MSA)</b>",
          font = list(family = "Arial", size = 16, color = "#004777"),
          x = 0.05
        )
      )
  })
  
  # 5. Violin Plot Logic
  output$violinPlot <- renderPlot({
    # Data is already Title Cased globally
    plot_data <- permits_wait_time %>%
      mutate(
        `Permit Type Definition` = str_wrap(`Permit Type Definition`, width = 10) # Wrap for horizontal fit
      )
      
    ggplot(plot_data, aes(x = `Permit Type Definition`, y = WaitTime, fill = `Permit Type Definition`)) +
      geom_violin(trim = FALSE, alpha = 0.6) + 
      geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
      scale_fill_manual(values = get_extended_palette(length(unique(plot_data$`Permit Type Definition`)))) +
      labs(
        title = "Distribution of Permit Processing Times",
        x = "Permit Type",
        y = "Wait Time (Days)"
      ) +
      common_theme +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5), 
        panel.grid.major.y = element_line(color = "grey90") 
      )
  })
  
  # 6. Lollipop Chart Logic
  output$lollipopPlot <- renderPlot({
    req(input$neighborhoodPermitType)
    
    # Filter data based on selection
    filtered_neighborhoods <- permits_clean %>%
      filter(!is.na(neighborhoods_analysis_boundaries)) %>%
      filter(`Permit Type Definition` %in% input$neighborhoodPermitType) %>%
      count(neighborhoods_analysis_boundaries, sort = TRUE) %>%
      slice_head(n = 20)
      
    validate(need(nrow(filtered_neighborhoods) > 0, "No data available for the selected permit types."))

    ggplot(filtered_neighborhoods, aes(x = reorder(neighborhoods_analysis_boundaries, n), y = n)) +
      geom_segment(aes(x = reorder(neighborhoods_analysis_boundaries, n), xend = reorder(neighborhoods_analysis_boundaries, n), y = 0, yend = n), color = "grey") +
      geom_point(size = 4, color = custom_palette[3]) + 
      geom_text(aes(label = comma(n)), hjust = -0.5, size = 4, family = "Inter") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
      coord_flip() +
      labs(
        title = "Total Permits Issued by Neighborhood",
        x = NULL, 
        y = NULL  
      ) +
      common_theme +
      theme(
        panel.grid.major.y = element_blank() # Clean look
      )
  })
  
  # 7. Sankey Diagram Logic
  output$sankeyPlot <- renderPlot({
    sankey_data <- change_of_use %>%
      mutate(
        `Existing Use` = str_to_title(`Existing Use`),
        `Proposed Use` = str_to_title(`Proposed Use`)
      ) %>%
      mutate(
        `Existing Use` = str_replace_all(`Existing Use`, "Hndlng", "Handling"),
        `Proposed Use` = str_replace_all(`Proposed Use`, "Hndlng", "Handling")
      ) %>%
      mutate(
        `Existing Use` = str_replace_all(`Existing Use`, "Food/Beverage Handling", "Food/ Beverage"),
        `Proposed Use` = str_replace_all(`Proposed Use`, "Food/Beverage Handling", "Food/ Beverage")
      )
      
    ggplot(sankey_data, aes(y = n, axis1 = `Existing Use`, axis2 = `Proposed Use`)) +
      geom_alluvium(aes(fill = `Existing Use`), width = 1/12) +
      geom_stratum(width = 1/12, fill = "grey80", color = "grey") +
      geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 4, family = "Inter") +
      scale_fill_manual(values = get_extended_palette(length(unique(sankey_data$`Existing Use`)))) +
      labs(
        title = "Top Changes in Building Use",
        subtitle = NULL, 
        y = NULL 
      ) +
      common_theme +
      theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
      )
  })
  
  # 5. Rent Map Logic
  output$rentMap <- renderLeaflet({
    req(census_data)
    
    #  gradient from Yale Blue to Red
    # custom palette: Yale Blue (#004777) -> Light Blue (#72A1C6) -> Peach (#EFD28D) -> Orange (#FF7700) -> Red (#A30000)
    map_palette <- c("#004777", "#72A1C6", "#EFD28D", "#FF7700", "#A30000")
    
    pal <- colorNumeric(
      palette = map_palette,
      domain  = census_data$median_rent,
      na.color = "#cccccc"
    )
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
       Median rent: %s<br/>
       Median income: %s<br/>
       Total population: %s",
      census_data$NAME,
      dollar(census_data$median_rent),
      dollar(census_data$median_income),
      comma(census_data$pop_total)
    ) %>% lapply(HTML)
    
    leaflet(census_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor   = ~pal(median_rent),
        weight      = 0.3,
        color       = "#444444",
        opacity     = 0.6,
        fillOpacity = 0.8,
        smoothFactor = 0.2,
        label       = labels, 
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#000000",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        "bottomright",
        pal     = pal,
        values  = ~median_rent,
        title   = "Median Rent (2023)",
        labFormat = labelFormat(prefix = "$"),
        opacity = 0.7
      )
  })
}

# run app
shinyApp(ui = ui, server = server)
