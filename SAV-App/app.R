# SAV REVAMP

library(groundhog)
library(shiny)

# groundhog.day <- "2026-04-01"
# 
# groundhog.library(leaflet, groundhog.day)
# groundhog.library(sf, groundhog.day)
# groundhog.library(here, groundhog.day)
# groundhog.library(tidyverse, groundhog.day)
# groundhog.library(data.table, groundhog.day)
# groundhog.library(plotly, groundhog.day)
# groundhog.library(bslib, groundhog.day)
# groundhog.library(RODBC, groundhog.day)
# groundhog.library(keyring, groundhog.day)

#for now, try without groundhog just to see if it works
library(leaflet)
library(sf)
library(here)
library(tidyverse)
library(data.table)
library(plotly)
library(bslib)
library(DBI)
library(odbc)
library(keyring)

#############

#Temp: identifying posit's IP address for Becca

library(httr)

# Ask the internet what IP we are dialing out from
ip_check <- GET("https://checkip.amazonaws.com")
outbound_ip <- trimws(content(ip_check, "text"))

# Print it to your Posit deployment log
print(paste("URGENT FOR IT: The Posit Connect Outbound IP is:", outbound_ip))

############### DEPENDENCIES ##################################################

## Bring in segments for map

baysegments <- st_read(here( "Chesapeake_Bay_104_Segments"))
baysegments <- st_transform(baysegments, crs = "+proj=longlat +datum=WGS84") #transform to DD

SAVdata <- fread(here("SAV-App", "SAVdata.csv"))

#reformat SAV data

SAVdata_long <- melt(SAVdata, 
                     id.vars=c(1:5, 86),
                     measure.vars=c("1984", "1985", "1986", "1987", "1989", "1990", "1991", "1992", "1993",
                                   "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002",
                                   "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011",
                                   "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020",
                                   "2021", "2022", "2023", "2024"),
                     variable.name = "Year", value.name = "Coverage"
                    )

#couple of bits to format the selection menu
segment_mapping <- unique(SAVdata_long[, c("Segment_Name", "CBPSEG")])
dynamic_choices <- setNames(segment_mapping$CBPSEG, segment_mapping$Segment_Name)
final_segment_choices <- c("Select a segment..." = "", dynamic_choices)

################## UI ##########################################################
ui <- fluidPage(
  
  # # Theme, title and DNR banner
  theme = bs_theme(preset = "flatly"),
  tags$div(
    style = "display: flex; justify-content: space-between; align-items: center; margin-top: 20px; margin-bottom: 20px;",
    tags$h2(("MD DNR's SAV Coverage"), style = "margin: 0;"),
    tags$img(src = "DNR_logo_final.png", height = "60px")
  ),
  hr(),

    fluidRow(
      column(width = 5,
        wellPanel(height = "500px",
                selectInput(inputId = "segment_selection",
                            label = "Select Bay Segment",
                            choices = final_segment_choices,
                            selected = ""
                ),
                actionButton(
                  inputId = "recenter", 
                  label = "Recenter Map"
                ),
                br(),
                br(),
                helpText(HTML(
                  "Welcome to MDDNR's SAV Visualizer tool. 
                  This tool visualizes data collected from <a href='https://www.vims.edu/research/units/programs/sav/methods/'>SAV arial surveys</a>
                  the Virtinia Institute of Marine Science
                  conducts yearly. To use this tool, select your segment of choice from the drop-down menu above. 
                  This will display the SAV coverage measured each year from 1984 to present, and display the segment
                  on the map to the left. You may select 'Recenter Map' to zoom back out, or use the map's buttons
                  to zoom in and out and navigate through the Chesapeake Bay."
                )),
                style = "padding-bottom: 145px;"
        )
      ),
      column(width = 7,
             card(p("Segment Map", align="center"),
                  leafletOutput("SegmentMap", height="500px"))
      )
    ),
  fluidRow(
    column(width = 12,
           plotlyOutput("SAVCoverage"))
  ),
  br(),
  br()
)

################ SERVER ########################################################
server <- function(input, output, session) {
  
  ####### leaflet map
  
  output$SegmentMap <- renderLeaflet({
    
    leaflet () %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = -76.2, lat = 38.3, zoom = 8) %>%
      addPolygons(data = baysegments,
                  popup = ~CBPSEG,
                  group = paste("Bay", "Segments", sep="<br>"),
                  opacity = 0.3,
                  weight = 1,
                  smoothFactor = 0.5,
                  color = "navy")
  })
  
  observeEvent(input$segment_selection, {
    
    if (is.null(input$segment_selection) || input$segment_selection == "") {
      leafletProxy("SegmentMap") %>%
        clearGroup("highlighted_polygon") %>%
        setView(lng = -76.2, lat = 38.3, zoom = 8)
      return() # Exit the observeEvent early
    }
    
    #filter for selected segment
    selected_polygon <- baysegments[baysegments$CBPSEG == input$segment_selection, ]
    
    # Calculate the bounding box to center the map (sf method)
    bbox <- sf::st_bbox(selected_polygon)
    
    # Use leafletProxy to modify the existing map
    leafletProxy("SegmentMap") %>%
      clearGroup("highlighted_polygon") %>%
      addPolygons(
        data = selected_polygon, 
        popup = ~CBPSEG,
        stroke = TRUE, 
        weight = 4, # Slightly thicker to stand out
        color = "goldenrod", 
        fillColor = "goldenrod",
        fillOpacity = 0.6,
        group = "highlighted_polygon"
      ) %>%
      # Fit the map view to the selected polygon's boundaries
      fitBounds(
        lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
        lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
      )
  })
  
  observeEvent(input$recenter, {
    # This resets the dropdown menu back to "Select a segment..."
    updateSelectInput(session, "segment_selection", selected = "")
    
    # Reset the map view and clear any existing highlights
    leafletProxy("SegmentMap") %>%
      clearGroup("highlighted_polygon") %>%
      setView(lng = -76.2, lat = 38.3, zoom = 8)
  })
  
  ######## SAV Bar Graph
  
  SAVreactive <- reactive ({
    req(input$segment_selection)
    SAVdata_long[CBPSEG == input$segment_selection, ]
  })
  
  output$SAVCoverage <- renderPlotly ({ 
    
  if(input$segment_selection == "") {

    SAVgg <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "Select a segment to display SAV coverage from 1984 to present.", size = 6, hjust = 0.5) +
      theme_classic() +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())

    SAV_plotly <- ggplotly(SAVgg)
    SAV_plotly

  } else {
    
    SAVforgraph <- SAVreactive()
    
    SAVgg <- ggplot(SAVforgraph, aes(x=Year, y=Coverage))+
      geom_bar(stat="identity", position="dodge", fill= "#6b8e23")+
      geom_hline(SAVforgraph, mapping=aes(yintercept=RestorationAcreageGoal), color="navy")+
      annotate("text", 
               x=5, 
               y=ifelse(unique(SAVforgraph$RestorationAcreageGoal) > 3, 
                               sum(unique(SAVforgraph$RestorationAcreageGoal)+unique(SAVforgraph$RestorationAcreageGoal)*0.1),
                               sum(unique(SAVforgraph$RestorationAcreageGoal)+0.2)), 
               label="Restoration Goal", color="navy")+
      ggtitle(paste(unique(SAVforgraph$Segment_Salinity)))+
      ylab("Coverage (Acres)")+
      xlab("Years")+
      theme_classic()+
      theme(text=element_text(size=15),
            axis.text.x = element_text(size=10, angle = 45, vjust = 0.5, hjust=1))
    
    SAV_plotly <- ggplotly(SAVgg)
    SAV_plotly
    
  }
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
