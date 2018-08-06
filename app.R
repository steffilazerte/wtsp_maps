#rsconnect::deployApp(appDir = "maps")


library(shiny)
library(leaflet)
library(htmltools)
library(dplyr)
library(tidyr)
library(readr)

           
               
xc <- read_csv("http://www.whitethroatsong.ca/wp-content/uploads/wtsp_songs_xc.csv",
               col_names = c("XC", "year", "season", "prov", "strophe.type", "lat", "lon", "recordist", "active", "date"), skip = 1) %>% 
  select(-date, -season, -prov) %>%
  #mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  #arrange(date) %>%
  mutate(recordist = replace(recordist, 
                             recordist %in% c("Robin Sommerfeld", "Ken Otter", "Veronica Mesias"), 
                             "WTSP project"),
         active = active == "Yes")
  
songs <- read_csv("http://www.whitethroatsong.ca/wp-content/uploads/wtsp_songs_project.csv") %>%
  select(-season, -prov, -ID, -XC, - date) %>%
  #filter(is.na(XC)) %>%
  full_join(xc) %>%
  filter((lat > 16.301420 & lat < 70.171038) & (lon < -37.532618 & lon > -168.313862)) %>%
  distinct(.) %>%
  mutate(lat = jitter(lat, factor = 50),
         lon = jitter(lon, factor = 50),
         strophe.type = replace(strophe.type, strophe.type == "", "Unknown")) %>%
  arrange(lat, lon)

pal <- colorFactor(c("yellow", "red", "black"), domain = c("Doublet", "Triplet","Mono", "Unknown"))

pal_doublet <- colorNumeric(
  palette = colorRamp(c("#FFFFFF", "#FFFF00"), interpolate = "spline"),
  domain = songs$year
)

pal_triplet <- colorNumeric(
  palette = colorRamp(c("#FFFFFF", "#CC0000"), interpolate = "spline"),
  domain = songs$year
)

pal_unknown <- colorNumeric(
  palette = colorRamp(c("#FFFFFF", "#000000"), interpolate = "spline"),
  domain = songs$year
)

html_legend <- paste0(
  "<div style = 'margin-bottom: 5px; text-align:center;'><strong>Legend</strong></div>
   <div>
     <div class = grad id = doublet></div>Doublet<br>
     <div class = grad id = triplet></div>Triplet<br>
     <div class = grad id = unknown></div>Unknown<br>
     <div class = grad style = 'float:none; height: 5px; border-left: solid; border-right: solid; border-width: 1px;'></div>
   <div style = 'height: 15px;'>   
     <div style = 'position: absolute; left: 5px; bottom: 0px;'>", min(songs$year), "</div>
     <div style = 'position: absolute; left: 80px; bottom: 0px;'>", max(songs$year), "</div> 
   </div>
   </div>")



#previewColors(pal_doublet, sort(unique(songs$year)))
#previewColors(pal_triplet, sort(unique(songs$year)))

ui <- fluidPage(theme = "style.css",
                leafletOutput("map", height = 600),
                absolutePanel(class = "info legend leaflet-control",
                              top = 345, right = 25,
                              width = "100px",
                              radioButtons("xc_link", label = h4("Xeno-canto"), choices = list("All" = "All", "On XC" = "xc"), selected = "All")),
                absolutePanel(class = "info legend leaflet-control",
                              top = 450, right = 25,
                              width = "200px",
                              sliderInput("years", 
                                          label = h4("Years"), 
                                          min = min(songs$year), max = max(songs$year), 
                                          value = c(min(songs$year), max(songs$year)), 
                                          step = 1,
                                          ticks = TRUE, sep = "", animate = TRUE)
                )
)
    

server <- function(input, output, session) {
  
  s <- reactive({
    req(input$years, input$xc_link)
    temp <- songs[songs$year >= input$years[1] & songs$year <= input$years[2], ]
    if(input$xc_link != "All") temp <- temp[!is.na(temp$XC),]
    return(temp)
  })
    
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Satelite") %>%
      addProviderTiles("Stamen.TonerHybrid") %>%
      fitBounds(-130, 40, -50, 55) %>%
      # addLegend("topright", colors = c("#FFFF00", "#CC0000", "#000000"),
      #           labels = c("Doublets", "Triplets", "Unknown"), title = "Strophe type", opacity = 1) %>%
      # addLegend("topright", pal = pal_legend,
      #           values = unique(songs$year),
      #           title = "Years",
      #           opacity = 1, bins = 5,
      #           labFormat = labelFormat(big.mark = "")) %>%
      addControl(html = html_legend, position = "topright") %>%
      addCircleMarkers(data = songs[songs$strophe.type == "Unknown",],
                       lat = ~lat, lng = ~lon,
                       radius = 6,
                       color = ~pal_unknown(year),
                       fillOpacity = 0.8,
                       popup = ~ifelse(!is.na(XC), paste0(year, " - ", "<a href = 'http://xeno-canto.org/", XC, "' target=\"_blank\">XC", XC, "</a>"), paste0(year, " - WTSP Project Recording")),
                       stroke = FALSE,
                       group = "Unknown") %>%
      addCircleMarkers(data = songs[songs$strophe.type == "Doublet",],
                       lat = ~lat, lng = ~lon,
                       radius = 6,
                       color = ~pal_doublet(year),
                       fillOpacity = 0.8,
                       popup = ~ifelse(!is.na(XC), paste0(year, " - ", "<a href = 'http://xeno-canto.org/", XC, "' target=\"_blank\">XC", XC, "</a>"), paste0(year, " - WTSP Project Recording")),
                       stroke = FALSE,
                       group = "Doublets") %>%
      addCircleMarkers(data = songs[songs$strophe.type == "Triplet",],
                       lat = ~lat, lng = ~lon,
                       radius = 6,
                       color = ~pal_triplet(year),
                       fillOpacity = 0.8,
                       popup = ~ifelse(!is.na(XC), paste0(year, " - ", "<a href = 'http://xeno-canto.org/", XC, "' target=\"_blank\">XC", XC, "</a>"), paste0(year, " - WTSP Project Recording")),
                       stroke = FALSE,
                       group = "Triplets") %>%
      addLayersControl(overlayGroups = c("Doublets", "Triplets", "Unknown"),options = layersControlOptions(collapsed = FALSE))
  })
  
  observeEvent(s(), {
    ## Change markers depending on year
    if(nrow(s()) > 0){
      leafletProxy("map", data = s()) %>%
        clearMarkers() %>%
        addCircleMarkers(data = s()[s()$strophe.type == "Unknown",],
                         lat = ~lat, lng = ~lon,
                         radius = 4,
                         color = "black",
                         fillOpacity = 1,
                         popup = ~ifelse(!is.na(XC), paste0(year, " - ", "<a href = 'http://xeno-canto.org/", XC, "' target=\"_blank\">XC", XC, "</a>"), paste0(year, " - WTSP Project Recording")),
                         stroke = FALSE) %>%
        addCircleMarkers(data = s()[s()$strophe.type == "Doublet",],
                         lat = ~lat, lng = ~lon,
                         radius = 6,
                         color = ~pal_doublet(year),
                         fillOpacity = 0.8,
                         popup = ~ifelse(!is.na(XC), paste0(year, " - ", "<a href = 'http://xeno-canto.org/", XC, "' target=\"_blank\">XC", XC, "</a>"), paste0(year, " - WTSP Project Recording")),
                         stroke = FALSE) %>%
        addCircleMarkers(data = s()[s()$strophe.type == "Triplet",],
                         lat = ~lat, lng = ~lon,
                         radius = 6,
                         color = ~pal_triplet(year),
                         fillOpacity = 0.8,
                         popup = ~ifelse(!is.na(XC), paste0(year, " - ", "<a href = 'http://xeno-canto.org/", XC, "' target=\"_blank\">XC", XC, "</a>"), paste0(year, " - WTSP Project Recording")),
                         stroke = FALSE)               
      ## Remove markers if no data to plot
    } else if(nrow(s()) == 0){
      suppressMessages(
        leafletProxy("map") %>%
          clearMarkers()
      )
    }
  })
}

shinyApp(ui = ui, server = server)
