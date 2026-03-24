library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(plotly)
library(sf)
library(munsell)
library(scales)

# Load pre-processed data
preprocessed_data <- readRDS("preprocessed_data_backup.rds")
data <- preprocessed_data$data
us_counties <- preprocessed_data$us_counties

# UI
ui <- fluidPage(
  titlePanel("Hurricane Strikes and Home Values Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h4("Counties with Hurricanes"),
      # This will be populated from the server
      selectInput("county_select", "Select a county:", choices = NULL, width = "100%"),
      br(),
      hr(),
      h5("Data Sources:"),
      tags$ul(
        tags$li(tags$a(href = "https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2", "FEMA Disaster Declarations Summaries")),
        tags$li(tags$a(href = "https://www.zillow.com/research/data/", "Zillow Home Value Index (ZHVI)")),
        tags$li(tags$a(href = "https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html", "US Counties Shapefile"))
      )
    ),
    mainPanel(
      h4("Select a County on the Map or From the List"),
      p("Click on a highlighted county or select from the dropdown to view home value trends."),
      HTML('<div>
              <span style="background-color: yellow; border: 1px solid black; padding: 0px 10px;">&nbsp;</span> With Hurricane
              &nbsp;&nbsp;
              <span style="background-color: lightgray; border: 1px solid black; padding: 0px 10px;">&nbsp;</span> Without Hurricane
            </div><br/>'),
      leafletOutput("map"),
      br(),
      plotlyOutput("timeSeries"),
      uiOutput("graphNote")
    )
  )
)

# Server
server <- function(input, output, session) {

  # Prepare and set the choices for the dropdown
  hurricane_county_list <- data %>%
    filter(hurricane_flag) %>%
    distinct(fips, RegionName, StateName) %>%
    mutate(fips = as.character(fips)) %>%
    arrange(StateName, RegionName) %>%
    mutate(display_name = paste0(RegionName, ", ", StateName))

  county_choices <- setNames(hurricane_county_list$fips, hurricane_county_list$display_name)

  updateSelectInput(session, "county_select", choices = c("Select a county" = "", county_choices))

  # Reactive value for selected county
  selected_county <- reactiveVal(NULL)

  # Observer for map clicks on hurricane counties
  observeEvent(input$map_shape_click, {
    # Only react to clicks on counties that are in our hurricane list
    req(input$map_shape_click$id %in% county_choices)
    selected_county(input$map_shape_click$id)
  })

  # Observer for dropdown selection
  observeEvent(input$county_select, {
    # Ignore the initial blank value
    req(input$county_select != "")
    selected_county(input$county_select)
  })

  # When the selected county changes, update the dropdown menu to keep it in sync
  observeEvent(selected_county(), {
    req(selected_county())
    updateSelectInput(session, "county_select", selected = selected_county())
  })

  # Map
  output$map <- renderLeaflet({
    leaflet(us_counties) %>%
      addTiles() %>%
      setView(lng = -98.57, lat = 39.82, zoom = 4) %>%
      addPolygons(
        fillColor = ~ifelse(!is.na(RegionName), "yellow", "lightgray"),
        fillOpacity = 0.5,
        color = "black",
        weight = 1,
        layerId = ~fips,
        popup = ~paste(RegionName, StateName, sep = ", ")
      )
  })

  # Time series plot
  output$timeSeries <- renderPlotly({
    req(selected_county())
    county_data <- data %>% filter(fips == selected_county())

    if (nrow(county_data) == 0) return(NULL)

    # Get hurricane periods
    hurricanes <- county_data %>%
      filter(hurricane_flag) %>%
      mutate(end_date = date %m+% years(1),
             ay_offset = -40)

    # Simple algorithm to repel overlapping labels
    if(nrow(hurricanes) > 1) {
      hurricanes <- hurricanes %>% arrange(date)
      for(i in 2:nrow(hurricanes)) {
        if(difftime(hurricanes$date[i], hurricanes$date[i-1], units = "days") < 365 * 2) { # If hurricanes are within 2 years
          # Offset the ay parameter (text position) instead of the arrow tip (zhvi_value)
          hurricanes$ay_offset[i] <- hurricanes$ay_offset[i-1] - 30
        }
      }
    }

    plot_ly(county_data, x = ~date, y = ~zhvi_value, type = 'scatter', mode = 'lines', name = "ZHVI",
            text = ~paste("Month:", format(date, "%Y-%m"),
                          "<br>ZHVI:", format(round(zhvi_value, 0), big.mark = ","),
                          ifelse(!is.na(hurricane_names), paste("<br>Hurricane:", hurricane_names), "<br>Hurricane: -")),
            hoverinfo = 'text') %>%
      layout(
        title = paste("Monthly Home Values in", unique(county_data$RegionName), unique(county_data$StateName)),
        xaxis = list(title = "Date"),
        yaxis = list(title = "ZHVI", tickformat = ","),
        showlegend = FALSE,
        shapes = lapply(1:nrow(hurricanes), function(i) {
          list(
            type = "rect",
            fillcolor = "yellow",
            line = list(width = 0),
            opacity = 0.2,
            x0 = hurricanes$date[i],
            x1 = hurricanes$end_date[i],
            y0 = 0,
            y1 = 1,
            yref = "paper",
            layer = "below",
            hoverinfo = "none"
          )
        }),
        annotations = lapply(1:nrow(hurricanes), function(i) {
          list(
            x = hurricanes$date[i],
            y = hurricanes$zhvi_value[i],
            text = hurricanes$hurricane_names[i],
            showarrow = TRUE,
            arrowhead = 4,
            standoff = 0,
            ax = 0,
            ay = hurricanes$ay_offset[i]
          )
        })
      )
  })

  output$graphNote <- renderUI({
    req(selected_county())
    p(HTML("<b>Note:</b> The yellow shaded area in the graph represents the 1-year period following a hurricane strike."))
  })
}

# Run the app
print(shinyApp(ui = ui, server = server))