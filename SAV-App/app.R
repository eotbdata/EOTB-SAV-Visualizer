# SAV REVAMP

library(groundhog)
library(shiny)
library(leaflet)
library(leafgl)
library(sf)
library(here)
library(tidyverse)
library(data.table)
library(plotly)
library(bslib)

#run these before pushing to posit if you add more libraries
#library(rsconnect)
#writeManifest(appDir = here("SAV-App"))

############### DEPENDENCIES ##################################################

## Bring in segments for map

baysegments <- st_read(here( "Chesapeake_Bay_104_Segments"))
baysegments <- st_transform(baysegments, crs = "+proj=longlat +datum=WGS84") #transform to DD
#this little bit of code modifies the CBPTF segement names
baysegments <- baysegments %>%
  mutate(CBPSEG := case_when(CBSEG_104 == "CB1TF1" ~ "CB1TF1",
                             CBSEG_104 == "CB1TF2" ~ "CB1TF2",
                             TRUE ~ CBPSEG))

SAVdata <- fread(here("SAV-App", "SAVdata_alphabetized.csv"))

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

SAVdata_comments <- melt(SAVdata, 
                         id.vars=c(1:5, 86),
                         measure.vars=c("_1984", "_1985", "_1986", "_1987", "_1989", "_1990", "_1991", "_1992", "_1993",
                                        "_1994", "_1995", "_1996", "_1997", "_1998", "_1999", "_2000", "_2001", "_2002",
                                        "_2003", "_2004", "_2005", "_2006", "_2007", "_2008", "_2009", "_2010", "_2011",
                                        "_2012", "_2013", "_2014", "_2015", "_2016", "_2017", "2018_nd", "2019_nd", "2020_nd",
                                        "2021_nd", "2022_nd", "2023_nd", "2024_nd"),
                         variable.name = "Year", value.name = "Comments"
)
SAVdata_comments <- SAVdata_comments[, Year := gsub("_|n|d", "", Year)]
SAVdata_long <- merge(SAVdata_long, SAVdata_comments, by=c("CBPSEG", 
                                                           "Segment_Name", 
                                                           "Sort", 
                                                           "Salinity", 
                                                           "Segment_Salinity", 
                                                           "RestorationAcreageGoal", 
                                                           "Year"))
SAVdata_long$Comments <- as.character(SAVdata_long$Comments)

#couple of bits to format the selection menu
segment_mapping <- unique(SAVdata_long[, c("Segment_Name", "CBPSEG")])
dynamic_choices <- setNames(segment_mapping$CBPSEG, segment_mapping$Segment_Name)

#merge names of segment mapping back to bay segments
baysegments <- merge(baysegments, segment_mapping, by=c("CBPSEG"))

#bring in stuff for the sav map
#note: MW will try just doing 3 years of simplified polygons for speed

sav2024 <- st_read(here("sav2024_smpl"), promote_to_multi=FALSE)
sav2023 <- st_read(here("sav2023_smpl"), promote_to_multi=FALSE)
sav2022 <- st_read(here("sav2022_smpl"), promote_to_multi=FALSE)



################## UI ##########################################################
ui <- fluidPage(
  
  # # Theme, title and DNR banner
  theme = bs_theme(preset = "flatly"),
  tags$div(
    style = "display: flex; justify-content: space-between; align-items: center; margin-top: 20px; margin-bottom: 20px;",
    tags$h2(("MD DNR's Submerged Aquatic Vegetation (SAV) Coverage"), style = "margin: 0;"),
    tags$img(src = "DNR_logo_final.png", height = "60px")
  ),
  hr(),
  
  fluidRow(
    column(width = 5,
           wellPanel(selectInput(inputId = "segment_selection",
                                 label = "Select Bay Segment",
                                 choices = dynamic_choices,
                                 selected = ""
                     ),
                     br(),
                     actionButton(
                       inputId = "recenter", 
                       label = "Recenter Map to Maryland Chesapeake Bay",
                       class = "btn-primary"
                     ),
                     br(),
                     br(),
                     downloadButton(
                       outputId = "segment_download",
                       label = "ADA Data Download",
                       class = "btn-secondary"
                     ),
                     br(),
                     br(),
                     helpText(HTML(
                       "<p>Welcome to the Maryland Department of Natural Resources (MDDNR) Submerged Aquatic Vegetation (SAV) Visualizer.
                       This tool illustrates SAV coverage data collected during
                       <a href='https://www.vims.edu/research/units/programs/sav/methods/' target='_blank'>annual aerial surveys</a>
                       conducted by the Virginia Institute of Marine Science (VIMS).</p>
                       
                       <p>To begin, select a Bay segment from the drop-down menu above. 
                       The tool will display the SAV coverage measured from 1984 to the present 
                       in the chart below, and highlight that specific area on the map to the right. 
                       You can click 'Recenter Map' to reset the view, or use the map navigation buttons
                       to explore the Chesapeake Bay.</p>"
                     )),
                     style = "padding-bottom: 50px;"
           )
    ),
    column(width = 7,
           card(p("Segment Map", align="center"),
                leafletOutput("SegmentMap", height="500px"))
    )
   ),
  br(),
  fluidRow(
    column(width = 12,
           plotlyOutput("SAVCoverage"))
  ),
  br(),
  h4("VIMS SAV Aerial Survey Bed Locations (2019-2023)", style="text-align:center"),
  fluidRow(
    column(width=12,
           leafglOutput("CoverageMap", height = "800px")
    )
  ),
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
                  layerId = ~CBSEG_104, #this codes for when users click on the segment
                  popup = paste0(baysegments$Segment_Name, ", (", baysegments$CBPSEG, ")"),
                  group = paste("Bay", "Segments", sep="<br>"),
                  opacity = 0.3,
                  weight = 1,
                  smoothFactor = 0.5,
                  color = "navy")
  })
  
  #observe for when segment selection drop-down is selected
  
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
    bbox <- st_bbox(selected_polygon)
    
    # Use leafletProxy to modify the existing map
    leafletProxy("SegmentMap") %>%
      clearGroup("highlighted_polygon") %>%
      addPolygons(
        data = selected_polygon, 
        popup = paste0(selected_polygon$Segment_Name, " (", selected_polygon$CBPSEG, ")"),
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
  
  #observe for when map is clicked
  observeEvent(input$SegmentMap_shape_click, {
    click <- input$SegmentMap_shape_click
    #make sure the map only responds to clicks on actual polygons
    if (is.null(click)) return()
    #note: click ids need to be unique. Using CBSEG_104 as a unqiue proxy, then filter back to regular CBPSEG
    cbseg104_clicked_segment <- click$id
    cbpseg <- baysegments$CBPSEG[baysegments$CBSEG_104 == cbseg104_clicked_segment]
    # Update the dropdown menu to match the clicked segment, map will automatically zoom
    updateSelectInput(session, "segment_selection", selected = cbpseg)
  })
  
  ######## SAV Bar Graph
  
  SAVreactive <- reactive ({
    req(input$segment_selection)
    SAVdata_long[CBPSEG == input$segment_selection, ]
  })
  
  output$SAVCoverage <- renderPlotly ({ 
    
    if(input$segment_selection == "") {
      
      MDcap <- SAVdata_long[Segment_Name == "Maryland", which(Comments == "pd"), by=.(Year)]
      caption_text <- paste0("Partial data for ", paste(MDcap$Year, collapse = ", "), ".")
      
      SAVgg <- ggplot(SAVdata_long[Segment_Name == "Maryland"], aes(x=Year, 
                                       y=Coverage,
                                       text = paste0(
                                         "Year: ", Year, "<br>",
                                         "Abundance: ", round(Coverage, 1), " acres<br>"
                                       ))) +
        geom_bar(stat="identity", position="dodge", fill= "#6b8e23")+
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        geom_hline(mapping=aes(yintercept=79800), color="navy")+
        annotate("text", x=7, y=82000, label="Restoration Goal: 79800 acres", color="navy")+
        #ggtitle("Maryland Submerged Aquatic Vegetation (SAV) Abundance")+
        ylab("Abundance (Acres)")+
        xlab("Years")+
        theme_classic()+
        theme(text=element_text(size=15),
              axis.text.x = element_text(size=10, angle = 45, vjust = 0.5, hjust=1))
      
      SAV_plotly <- ggplotly(SAVgg, tooltip = "text") %>%
        layout(
          margin = list(t = 80), 
          title = list(
            text = "Maryland Submerged Aquatic Vegetation (SAV) Abundance",
            y = 0.90,      
            yanchor = "top"
          )
        )  
      
      SAV_plotly <- SAV_plotly %>%
        layout(
          margin = list(b = 100), 
          annotations = list(
            x = 0, y = -0.30,     
            text = caption_text,
            showarrow = FALSE,
            xref = 'paper', yref = 'paper',
            xanchor = 'left', yanchor = 'top',
            font = list(size = 12, color = "black")
          )
        )
      
      SAV_plotly
      
    } else {
      
      SAVforgraph <- SAVreactive()
      
      pd_years <- SAVforgraph$Year[which(SAVforgraph$Comments == "pd")]
      nd_years <- SAVforgraph$Year[which(SAVforgraph$Comments == "nd")]
      
      pd_text <- if(length(pd_years) > 0) paste("Partial data for", paste(pd_years, collapse = ", ")) else ""
      nd_text <- if(length(nd_years) > 0) paste("No data for", paste(nd_years, collapse = ", ")) else ""
      
      if (pd_text != "" & nd_text != "") {
        caption_text <- paste0(pd_text, ". ", nd_text, ".")
      } else if (pd_text != "") {
        caption_text <- paste0(pd_text, ".")
      } else if (nd_text != "") {
        caption_text <- paste0(nd_text, ".")
      } else {
        caption_text <- ""
      }
      
      SAVgg <- ggplot(SAVforgraph, aes(x=Year, 
                                       y=Coverage,
                                       text = paste0(
                                         "Year: ", Year, "<br>",
                                         "Abundance: ", round(Coverage, 1), " acres<br>"
                                       ))) +
        geom_bar(stat="identity", position="dodge", fill= "#6b8e23")+
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        geom_hline(SAVforgraph, mapping=aes(yintercept=RestorationAcreageGoal), color="navy")+
        annotate("text", 
                 x=7, 
                 y = {
                   goal <- unique(SAVforgraph$RestorationAcreageGoal)
                   if (is.na(goal) || goal == 0) {
                     0.1
                   } else if (goal >= 3) {
                     goal + (goal * 0.1)
                   } else {
                     goal + 10 #this condition only captures a single river's goal -- adjust as needed for 2025's data
                   }
                 },
                 label = {
                   goal <- unique(SAVforgraph$RestorationAcreageGoal)
                   if (is.na(goal)) "No Restoration Goal Set" else paste0("Restoration Goal: ", goal, " acres")
                 }, color="navy")+
        #ggtitle(paste(unique(SAVforgraph$Segment_Salinity)))+
        ylab("Abundance (Acres)")+
        xlab("Years")+
        theme_classic()+
        theme(text=element_text(size=15),
              axis.text.x = element_text(size=10, angle = 45, vjust = 0.5, hjust=1))
      
   SAV_plotly <- ggplotly(SAVgg, tooltip = "text") %>%
     layout(
       margin = list(t = 80), 
       title = list(
         text = paste(unique(SAVforgraph$Segment_Salinity)),
         y = 0.90,      
         yanchor = "top"
       )
     )  
      
      if (caption_text != "") {
        SAV_plotly <- SAV_plotly %>%
          layout(
            margin = list(b = 100), # Increases bottom margin to fit the text
            annotations = list(
              x = 0, y = -0.25,     # Adjust y negatively to push it further down
              text = caption_text,
              showarrow = FALSE,
              xref = 'paper', yref = 'paper',
              xanchor = 'left', yanchor = 'top',
              font = list(size = 12, color = "black")
            )
          )
      }
      
      SAV_plotly
      
    }
    
  })
  
  output$segment_download <- downloadHandler(
    filename = function() {
      nice_name <- segment_mapping$Segment_Name[segment_mapping$CBPSEG == input$segment_selection]
      paste0(nice_name, ".csv")
    },
    content = function(file) {
      write.csv(SAVreactive(), file, quote = FALSE, row.names = FALSE) 
    }
  )
  
  output$CoverageMap <- renderLeaflet({

    savmap <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = -76.3, lat = 38.7, zoom = 8) %>%
      addGlPolygons(data = sav2019, color = "#B9DCAC", weight = 2, opacity = 1,
                    popup = "SAV Coverage 2019",
                    group = "SAV Coverage 2019") %>%
      addGlPolygons(data = sav2020, color = "#9ECF8C", weight = 2, opacity = 1,
                    popup = "SAV Coverage 2020",
                    group = "SAV Coverage 2020") %>%
      addGlPolygons(data = sav2021, color = "#82C16C", weight = 2, opacity = 1,
                    popup = "SAV Coverage 2021",
                    group = "SAV Coverage 2021") %>%
      addGlPolygons(data = sav2022, color = "#67B44B", weight = 2, opacity = 1,
                    popup = "SAV Coverage 2022",
                    group = "SAV Coverage 2022") %>%
      addGlPolygons(data = sav2023, color = "#427330", weight = 2, opacity = 1,
                    popup = "SAV Coverage 2023",
                    group = "SAV Coverage 2023") %>%
      addLayersControl(
        overlayGroups = c("SAV Coverage 2019",
                          "SAV Coverage 2020",
                          "SAV Coverage 2021",
                          "SAV Coverage 2022",
                          "SAV Coverage 2023"
        ),
        options = layersControlOptions(collapsed = FALSE))
    savmap

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)