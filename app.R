library(shiny)
library(leaflet)
library(sf)
library(DT)
library(dplyr)
library(RColorBrewer)
library(shinyjs)  # Add shinyjs library
library(rsconnect)


setwd("C:/Users/orenaike/OneDrive/02_JOBS/Others/FrancoiseMap/Web_Site/Interactive_Map/Interactive_map_github")

# Load the data
ssd_mt_admin_point_exp_N <- st_read("./data/Live_Plants.gpkg")
SSD_Admin <- st_read("./data/Box_Area.gpkg")
# Drop the Z dimension
SSD_Admin <- st_zm(SSD_Admin)
# Convert sf object to Spatial
SSD_Admin_sp <- as(SSD_Admin, "Spatial")

bbox <- st_bbox(SSD_Admin)

# Clean and categorize data
categories <- list(
  Trees = c("Tree", "Palm Tree", "Tree (Timber)", "Shrub/Tree", "Shrub-Tree", "Small Tree", "Shrub/Small Tree"),
  Shrubs = c("Shrub", "Scrambling Shrub", "Semi-Succulent Subshrub", "Suffrutescent Evergreen Shrub"),
  Subshrubs = c("Subshrub", "Subshrub (Epiphytic)", "Subshrub Perennial", "Subshrub/Shrub", 
                "Scrambling Subshrub", "Succulent Subshrub", "Succulent Sub-Shrub"),
  Vines = c("Vine", "Woody Vine", "Climber", "Climber Vine", "Scrambling Vine", "Vine/Young Tree"),
  Succulents = c("Cactus", "Succulent Perennial"),
  Geophytes = c("Bulbous Geophyte", "Climbing Tuberous Geophyte", "Tuberous Geophyte", 
                "Rhizomatous Geophyte", "Rhizomatous Geophyte (Fern)", "Scrambling Tuberous Geophyte"),
  Perennials = c("Herbaceous Perennial", "Herb Perennial", "Perennial", "Tall Perennial Grass", "Succulent Perennial"),
  Herbs = c("Herb"),
  Epiphytes = c("Orchid", "Orchideaceae", "Poroid Fungus Epiphyte"),
  Annuals = c("Annual", "Annual Climber", "Annual Herbaceous", "Annual Subshrub", "Annual/Biennial", 
              "Annual/Perennial?", "Annual/Subshrub")
)

categorize_plant <- function(plant_cate) {
  for (category in names(categories)) {
    if (plant_cate %in% categories[[category]]) {
      return(category)
    }
  }
  return("Other")
}

ssd_mt_admin_point_exp_N <- ssd_mt_admin_point_exp_N %>%
  mutate(Category = sapply(Plant_Cate, categorize_plant))

growth_form_minimal_categories <- list(
  Perennial = c("Tree", "Palm Tree", "Tree (Timber)", "Shrub/Tree", "Shrub-Tree", "Small Tree", 
                "Shrub/Small Tree", "Shrub", "Scrambling Shrub", "Semi-Succulent Subshrub", 
                "Suffrutescent Evergreen Shrub", "Subshrub", "Subshrub (Epiphytic)", 
                "Subshrub Perennial", "Subshrub/Shrub", "Scrambling Subshrub", "Succulent Subshrub", 
                "Succulent Sub-Shrub", "Vine", "Woody Vine", "Climber", "Climber Vine", 
                "Scrambling Vine", "Vine/Young Tree", "Cactus", "Succulent Perennial", 
                "Bulbous Geophyte", "Climbing Tuberous Geophyte", "Tuberous Geophyte", 
                "Rhizomatous Geophyte", "Rhizomatous Geophyte (Fern)", "Scrambling Tuberous Geophyte", 
                "Herbaceous Perennial", "Herb Perennial", "Perennial", "Tall Perennial Grass", 
                "Orchid", "Orchideaceae", "Poroid Fungus Epiphyte"),
  Annual = c("Annual", "Annual Climber", "Annual Herbaceous", "Annual Subshrub", "Annual/Biennial", 
             "Annual/Perennial?", "Annual/Subshrub")
)

categorize_growth_form_minimal <- function(plant_cate) {
  for (category in names(growth_form_minimal_categories)) {
    if (plant_cate %in% growth_form_minimal_categories[[category]]) {
      return(category)
    }
  }
  return("Other")
}

ssd_mt_admin_point_exp_N <- ssd_mt_admin_point_exp_N %>%
  mutate(Growth_Form = sapply(Plant_Cate, categorize_growth_form_minimal))

ssd_mt_admin_point_exp_N <- ssd_mt_admin_point_exp_N %>% 
  mutate(Matched_Ch = tolower(Matched_Ch)) %>%
  filter(Matched_Ch == "yes")

# Add a unique ID column for each plant
ssd_mt_admin_point_exp_N <- ssd_mt_admin_point_exp_N %>%
  mutate(id = row_number())

# Custom CSS
css <- "
input[type='number'] {
  max-width: 80%;
}

div.outer {
  position: fixed;
  top: 41px;
  left: 0;
  right: 0;
  bottom: 0;
  overflow: hidden;
  padding: 0;
}

/* Customize fonts */
body, label, input, button, select { 
  font-family: 'Helvetica Neue', Helvetica;
  font-weight: 200;
  background-color: #fdf6e8;
  color: #2c5444;
}

h1, h2, h3, h4 { 
  font-weight: 400;
  color: #2c5444;
}

#controls {
  /* Appearance */
  background-color: #fdf6e8;
  padding: 0 20px 20px 20px;
  cursor: move;
  /* Fade out while not hovering */
  opacity: 0.85;
  zoom: 0.9;
  transition: opacity 500ms 1s;
  border: 1px solid #2c5444;
}
#controls:hover {
  /* Fade in while hovering */
  opacity: 0.95;
  transition-delay: 0;
}

/* Position and style citation */
#cite {
  position: absolute;
  bottom: 10px;
  left: 10px;
  font-size: 12px;
}

/* If not using map tiles, show a white background */
.leaflet-container {
  background-color: #E6EFFB !important;
}

.green-popup .leaflet-popup-content-wrapper {
  background-color: #2c5444 !important;
  color: white;
}

/* Ensure the popup arrow is visible */
.green-popup .leaflet-popup-tip-container {
  visibility: visible;
}
"

# Generate a color palette for the Category column
categories <- unique(ssd_mt_admin_point_exp_N$Category)
# Use Paired palette for up to 12 categories
colors <- colorFactor(palette = brewer.pal(min(length(categories), 12), "Paired"), domain = categories)


# Define the UI
ui <- navbarPage("Yakumay Plant Base", id = "nav",
                 tabPanel("Interactive Map",
                          useShinyjs(),  # Initialize shinyjs
                          div(class = "outer",
                              tags$head(
                                tags$style(HTML(css))
                              ),
                              leafletOutput("map", width = "100%", height = "100%"),
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = FALSE, top = 160, left = 20, right = "auto", bottom = "auto",
                                            width = 330, height = "auto",
                                            h2("Filter Plants"),
                                            textInput("search", "Search Plant Name", value = ""),
                                            selectInput("life_cycle", "Life Cycle Category", 
                                                        choices = c("All" = "All", 
                                                                    setNames(unique(na.omit(ssd_mt_admin_point_exp_N$Growth_Form)), 
                                                                             unique(na.omit(ssd_mt_admin_point_exp_N$Growth_Form))))),
                                            selectInput("growth_form", "Growth Form Category", 
                                                        choices = c("All" = "All", 
                                                                    setNames(unique(na.omit(ssd_mt_admin_point_exp_N$Category)), 
                                                                             unique(na.omit(ssd_mt_admin_point_exp_N$Category))))),
                                            div(id = "plant_info", style = "margin-top: 20px;")
                              )
                          )
                 ),
                 tabPanel("Table",
                          DTOutput("table")
                 )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    data <- ssd_mt_admin_point_exp_N
    if (input$life_cycle != "All") {
      data <- data %>% filter(Growth_Form == input$life_cycle)
    }
    if (input$growth_form != "All") {
      data <- data %>% filter(Category == input$growth_form)
    }
    if (input$search != "") {
      data <- data %>% filter(grepl(input$search, New_Names, ignore.case = TRUE))
    }
    data
  })
  
  output$map <- renderLeaflet({
    valid_SSD_Admin <- SSD_Admin[!is.na(SSD_Admin$geometry), ]
    
    leaflet(options = leafletOptions(minZoom = 0, maxZoom = 30)) %>%
      addTiles(group = "OSM") %>%
      addProviderTiles("CartoDB.Positron", group = "Carto Basemap") %>%
      
      addPolygons(data = valid_SSD_Admin,
                  color = "grey",
                  weight = 2,
                  opacity = 0.05,  # Adjust this for border transparency
                  fillColor = "#ffffff",
                  fillOpacity = 0.2,  # Adjust this for fill transparency
                  highlightOptions = highlightOptions(color = "#2c5444", weight = 5,
                                                      bringToFront = FALSE),
                  group = "Polygon") %>%
      
      addCircleMarkers(data = filtered_data(),
                       layerId = ~id,  # Use the unique ID as the layer ID
                       color = ~colors(Category),
                       fillColor = ~colors(Category),
                       radius = 5,
                       stroke = TRUE,
                       fillOpacity = 0.8,
                       label = ~New_Names,
                       popup = ~paste('<span style="font-size: 20px;"><b>', toupper(New_Names), '</b></span>',
                                      "<hr>",
                                      "<b>BOTANICAL NAME: </b><i>", Botanical_, "</i><br>",
                                      "<b>ORIGIN: </b>", Area_of_Or),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", 
                                      "font-family" = "serif", padding = "3px 8px",
                                      "color" = "#2c5444",
                                      "box-shadow" = "3px 3px rgba(0,0,0,0.25)", 
                                      "border-color" = "rgba(0,0,0,0.5)"),
                         textsize = "12px",
                         direction = "auto"),
                       popupOptions = popupOptions(
                         closeButton = TRUE,
                         className = "green-popup"
                       ),
                       group = "Plants") %>%
      addLegend(position = "bottomleft",
                colors = colors(categories),
                labels = categories,
                opacity = 1, 
                title = "Plant Category") %>% 
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479") %>% 
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE) %>% 
      addLayersControl(baseGroups = c("OSM", "Carto Basemap"),
                       overlayGroups = c("Plants", "Polygon"),
                       options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    plant <- filtered_data() %>% filter(id == click$id)
    if (nrow(plant) == 1) {
      plant_info <- paste("<hr>",'<span style="font-size: 20px;"><b>', toupper(plant$New_Names), '</b></span>',
                          "<hr>",
                          "<b>BOTANICAL NAME: </b><i>", plant$Botanical_, "</i><br>",
                          "<b>MEDICINAL USE: </b>", plant$Medicinal_, "<br>",
                          "<b>PARTS USED MEDICALLY: </b>", plant$Parts_Used, "<br>",
                          "<b>ORIGIN: </b>", plant$Area_of_Or,"<br>",
                          "<b>ADDITIONAL NOTES: </b><i>", plant$Notes, "</i><br>")
      shinyjs::html("plant_info", plant_info)
    }
  })
  
  output$table <- renderDT({
    datatable(filtered_data())
  })
}

shinyApp(ui, server)
