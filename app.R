library(shiny)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(sf)

############ DATA IMPORT

# death data
data <- readRDS("base_data_w_NA.rds")

# wealth categorization of neighborhoods
wealth <- readRDS("wealth.rds")

# only important variables
data <- select(data, year, wijk, cause, majority_vote, DR)

# wide data format into long format
data <- data %>% 
  pivot_wider(id_cols = c("year", "wijk", "majority_vote"),
              values_from = "DR",
              names_from = "cause") 
data$total <- as.numeric(as.character(data$total))
data$pneum <- as.numeric(as.character(data$pneum))
data$diarrh <- as.numeric(as.character(data$diarrh))
data$resp_tb <- as.numeric(as.character(data$resp_tb))
data$measles <- as.numeric(as.character(data$measles))
data$diphth <- as.numeric(as.character(data$diphth))
data$whooping <- as.numeric(as.character(data$whooping))


# import the map in shapefile format

shape <- st_read("Neighbh1900_reconstr_nw.shp")

# inspect neighborhood codes
unique(shape$buurt1897)

# recode some neighborhoods so that the shapefile can be merged with the death data
shape$buurt1897[shape$buurt1897=="I"] <- "J"
shape$buurt1897[shape$buurt1897=="II"] <- "JJ"

shape$buurt1897[shape$buurt1897=="p"] <- "P"

# mergin map with death data

# data for animated maps
spatial_data <- merge(shape, data, by.x="buurt1897", by.y="wijk", all.x=F, all.y=T, duplicateGeoms=T)

# data for the map on the documentation page
spatial_data2 <- merge(shape, wealth, by.x="buurt1897", by.y="wijk", all.x=T, all.y=T)


######################################## UI


ui <- fluidPage(
  
  titlePanel(h1(strong("Spatial analysis of mortality due to infectious diseases in Amsterdam, 1856-1898"))),
  
  sidebarLayout(
    sidebarPanel(
      
      conditionalPanel(condition = "input.tab_selected==1", h4("Introduction to app features")),
      conditionalPanel(style = "font-size: 16px", condition = "input.tab_selected==2 | input.tab_selected==3 | input.tab_selected==4 | input.tab_selected==5 | input.tab_selected==6 | input.tab_selected==7 |input.tab_selected==8 ",
                       sliderInput("year2", "Observe over time", min = 1856, max = 1898, step = 1, value = 1856, animate = TRUE, sep = "")),
     ),
    
    mainPanel(
      tabsetPanel(
        type="tabs",
        id="tab_selected",
        selected=1,
        tabPanel(title="Documentation", value = 1,
                 h2("Introduction"),
                 p("The aim of this web application is to explore the spatial distribution of mortality in Amsterdam in the period between 1856 and 1898. The app is developed as part of the research project",
                   a(em("Lifting the burden of disease. The modernisation of health in the Netherlands: Amsterdam 1854-1940."), href="https://www.ru.nl/rich/our-research/research-groups/radboud-group-historical-demography-family-history/current-research-projects/current-projects/lifting-burden-disease/"),
                   "Information on death is obtained from the Amsterdam Cause-of-death Database (ACD), population data are from the decennial censuses available",
                   a(em("here."), href="http://www.volkstellingen.nl/nl/index.html"),
                   
                   style = "font-size: 110%"
                 ),
                 h2("How to use the app?"),
                   p("The tabs explore all-cause mortality and mortality due to certain infectious diseases such as measles, respiratory tuberculosis,
                     pneumonia, whooping cough, diphtheria and diarrheal causes including cholera. Low death rates are indicated with shades of orange, high death rates are indicated with shades of red in the maps. 
                     Playing the slider animation, you can explore changes over time.
                     If you want to inspect a certain year in more detail, use the start and stop button of the slider. The maps are interactive: each neighborhood is assigned with an 
                     info box which you can observe by placing the cursor over the area of the selected unit. The information box includes the year,
                     cause of death, neighborhood id, wealth and the death rate due to the selected cause by 10,000. The sliders across tabs are interconnected. It means that once you play the slider on a certain
                     tab, all animations are played. Thus, if you pause the animation of a certain cause at a certain year, you can switch between tabs and observe the death rate due to other causes in the same year.",
                   style = "font-size: 110%"),
                 h2("Neighborhoods and wealth classification"),
                 p("One of the peculiarities of this app is that you can analyze death rate by administrative unit. Amsterdam had 50 neighborhoods in the second half of the nineteenth century which were labelled with letters such as A, AA, B, BB etc.
                   In 1896, a new neighborhood named ONA was added to the city, which is missing from our analysis. The reason is that this new neighborhood 
                   appears only in the 1899 census and we cannot execute population interpolation based on a single year. Another peculiarity is that we use three sources 
                   (rental values from 1832, tax information from 1897/98 and a report by a contemporary medical doctor) to be able to categorize the neighborhoods into four wealth groups: poor, rather poor,
                    rather wealthy and wealthy. The color of the border in the maps indicates which wealth class a neighborhood belongs to.  Thus, this app allows to observe if the poor died from certain causes with higher probability than the rich. More information on these sources and the wealth classification method can be obtained from the author of this app.
                   The map below shows the location of neighborhoods including ONA (which lacks wealth classification) and the distribution of wealth categories (shades of green) around the city.", 
                   style = "font-size: 110%" 
                 ),
                 leafletOutput("wealth_map", height = "50vh", width = "50%"),
                 h2("Missing data"),
                 p("The death counts are missing for the following years: 1892, 1893, 1894. Grey color in the maps indicate missing information.", style = "font-size: 110%"),
                 h2("Additional information"),
                 
                 p("The app was designed and developed by", a(em("Katalin Buzasi."), href="http://katalinbuzasi.weebly.com/"), style="font-size: 110%"),
                 p("The historical shapefile of Amsterdam was obtained from", a(em("AdamLink"), href="https://adamlink.nl/geo/districts"), "and refined by", a(em("Sanne Muurling."), href="https://www.ru.nl/personen/muurling-s/") , style="font-size: 110%"),
                 
                 h2("Useful links"),
                 a(em("App 1 - Mortality by age group"), href="https://deathinamsterdam.shinyapps.io/mortality_by_age_group/", style="font-size: 110%"),
                 br(),
                 a(em("App 2 - Causes of death in time"), href="https://deathinamsterdam.shinyapps.io/causes_of_death_in_Amsterdam/", style="font-size: 110%"),
                 br(),
                 a(em("Dood in Amsterdam"), href="https://doodinamsterdam.nl/", style="font-size: 110%"),
                 br(),
                 a(em("RG Citizen Science for the History of Health"), href="https://www.ru.nl/rich/our-research/research-groups/radboud-group-historical-demography-family-history/current-research-projects/current-projects/rg-citizen-science-history-health/", style="font-size: 110%"),
                 icon = icon("question-circle")),
        tabPanel("All-cause", leafletOutput("all_cause", height="80vh", width = "90%"), value=2),
        tabPanel("Measles", leafletOutput("measles", height="80vh", width = "90%"), value=3),
        tabPanel("Resp. tuberc.", leafletOutput("resp_tb", height="80vh", width = "90%"), value=4),
        tabPanel("Pneumonia", leafletOutput("pneum", height="80vh", width = "90%"), value=5),
        tabPanel("Whooping cough", leafletOutput("whooping", height="80vh", width = "90%"), value=6),
        tabPanel("Diphtheria", leafletOutput("diphth", height="80vh", width = "90%"), value=7),
        tabPanel("Diarrheal", leafletOutput("diarrh", height="80vh", width = "90%"), value=8),
      )
    )
  ))


######################################## SERVER

server <- function(input, output){
  
  output$wealth_map <- renderLeaflet({
    pal_wealth <- colorFactor(palette = "Greens", domain = spatial_data2$majority_vote, na.color = "grey")
    
    labels_wealth <- sprintf(
      "<strong>Neighborhood: </strong>%s",
      spatial_data2$buurt1897) %>% 
      lapply(htmltools::HTML)
    

    
      spatial_data2 %>% 
      st_transform(4326) %>% 
      leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(label = labels_wealth,
                  stroke = TRUE,
                  #smoothFactor = 0.7,
                  weight = 2,
                  color = "black",
                  opacity = 0.5,
                  fillOpacity = 0.6,
                  fillColor = ~pal_wealth(majority_vote),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "white",
                                                      opacity = 1, 
                                                      bringToFront = TRUE)) %>% 
      addLegend("bottomright",
                pal = pal_wealth,
                values = ~majority_vote,
                title = "wealth",
                opacity = 0.7)

    
  })
  

  selected_year <- reactive({
    y <- spatial_data %>% filter(year == input$year2)
    
    return(y)
  })
  
  ### all-cause tab  
  output$all_cause <- renderLeaflet({
    # color palette
    
    pal_total <- colorBin(palette = "OrRd", bins = c(70, 165, 206, 242, 279, 332, 430, 723), domain = spatial_data$total, na.color = "grey")
    pal_border <- colorFactor(palette = "Greens", domain = unique(spatial_data$majority_vote))
    
    # labels
    labels_total <- sprintf(
      "<strong>Year: </strong>%g<br><strong>Cause: </strong>%s<br><strong>Neighborhood: </strong>%s<br><strong>Wealth: </strong>%s<br><strong>DR: </strong>%g",
      selected_year()$year, "all-cause", selected_year()$buurt1897, selected_year()$majority_vote, round(selected_year()$total, 0)) %>% 
      lapply(htmltools::HTML)
    
    # map
    selected_year() %>% 
      st_transform(4326) %>% 
      leaflet() %>% 

      addPolygons(label = labels_total,
                  stroke = TRUE,
                  #smoothFactor = 0.7,
                  weight = 3.5,
                  #color = "black",
                  color = ~pal_border(selected_year()$majority_vote),
                  opacity = 0.7,
                  fillOpacity = 0.6,
                  fillColor = ~pal_total(selected_year()$total),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "white",
                                                      opacity = 1, 
                                                      bringToFront = TRUE)) %>% 
      addLegend("bottomright",
                pal = pal_total,
                values = ~total,
                title = "DR by 10,000",
                opacity = 0.7) %>% 
      addLegend("bottomright",
                pal = pal_border,
                values = ~majority_vote,
                title = "wealth (border color)",
                opacity = 0.7)
    
    
    
    
  })
  
  ### measles tab  
  output$measles <- renderLeaflet({
    # color palette
    
    pal_measles <- colorBin(palette = "OrRd", bins = c(0, 3, 8, 15, 63, 107), domain = spatial_data$measles, na.color = "grey")
    pal_border <- colorFactor(palette = "Greens", domain = unique(spatial_data$majority_vote))
    
    # labels
    labels_measles <- sprintf(
      "<strong>Year: </strong>%g<br><strong>Cause: </strong>%s<br><strong>Neighborhood: </strong>%s<br><strong>Wealth: </strong>%s<br><strong>DR: </strong>%g",
      selected_year()$year, "measles", selected_year()$buurt1897, selected_year()$majority_vote, round(selected_year()$measles, 0)) %>% 
      lapply(htmltools::HTML)
    
    # map
    selected_year() %>% 
      st_transform(4326) %>% 
      leaflet() %>% 
      addPolygons(label = labels_measles,
                  stroke = TRUE,
                  #smoothFactor = 0.7,
                  weight = 3.5,
                  #color = "black",
                  color = ~pal_border(selected_year()$majority_vote),
                  opacity = 0.7,
                  fillOpacity = 0.6,
                  fillColor = ~pal_measles(selected_year()$measles),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "white",
                                                      opacity = 1, 
                                                      bringToFront = TRUE)) %>% 
      addLegend("bottomright",
                pal = pal_measles,
                values = ~measles,
                title = "DR by 10,000",
                opacity = 0.7) %>% 
      addLegend("bottomright",
                pal = pal_border,
                values = ~majority_vote,
                title = "wealth (border color)",
                opacity = 0.7)
    
  })
  
  
  ### resp_tb tab  
  output$resp_tb <- renderLeaflet({
    # color palette
    
    pal_resp_tb <- colorBin(palette = "OrRd", bins = c(0, 14, 28, 41, 55, 69), domain = spatial_data$resp_tb, na.color = "grey")
    pal_border <- colorFactor(palette = "Greens", domain = unique(spatial_data$majority_vote))
    
    # labels
    labels_resp_tb <- sprintf(
      "<strong>Year: </strong>%g<br><strong>Cause: </strong>%s<br><strong>Neighborhood: </strong>%s<br><strong>Wealth: </strong>%s<br><strong>DR: </strong>%g",
      selected_year()$year, "resp. tuberc.", selected_year()$buurt1897, selected_year()$majority_vote, round(selected_year()$resp_tb, 0)) %>% 
      lapply(htmltools::HTML)
    
    # map
    selected_year() %>% 
      st_transform(4326) %>% 
      leaflet() %>% 
      addPolygons(label = labels_resp_tb,
                  stroke = TRUE,
                  #smoothFactor = 0.7,
                  weight = 3.5,
                  #color = "black",
                  color = ~pal_border(selected_year()$majority_vote),
                  opacity = 0.7,
                  fillOpacity = 0.6,
                  fillColor = ~pal_resp_tb(selected_year()$resp_tb),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "white",
                                                      opacity = 1, 
                                                      bringToFront = TRUE)) %>% 
      addLegend("bottomright",
                pal = pal_resp_tb,
                values = ~resp_tb,
                title = "DR by 10,000",
                opacity = 0.7) %>% 
      addLegend("bottomright",
                pal = pal_border,
                values = ~majority_vote,
                title = "wealth (border color)",
                opacity = 0.7)
    
    
    
    
  })
  
  ### pneumonia tab  
  output$pneum <- renderLeaflet({
    # color palette
    
    pal_pneum <- colorBin(palette = "OrRd", bins = c(0, 19, 37, 55,74, 92), domain = spatial_data$pneum, na.color = "grey")
    pal_border <- colorFactor(palette = "Greens", domain = unique(spatial_data$majority_vote))
    
    # labels
    labels_pneum <- sprintf(
      "<strong>Year: </strong>%g<br><strong>Cause: </strong>%s<br><strong>Neighborhood: </strong>%s<br><strong>Wealth: </strong>%s<br><strong>DR: </strong>%g",
      selected_year()$year, "pneumonia", selected_year()$buurt1897, selected_year()$majority_vote, round(selected_year()$pneum, 0)) %>% 
      lapply(htmltools::HTML)
    
    # map
    selected_year() %>% 
      st_transform(4326) %>% 
      leaflet() %>% 
      addPolygons(label = labels_pneum,
                  stroke = TRUE,
                  #smoothFactor = 0.7,
                  weight = 3.5,
                  #color = "black",
                  color = ~pal_border(selected_year()$majority_vote),
                  opacity = 0.7,
                  fillOpacity = 0.6,
                  fillColor = ~pal_pneum(selected_year()$pneum),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "white",
                                                      opacity = 1, 
                                                      bringToFront = TRUE)) %>% 
      addLegend("bottomright",
                pal = pal_pneum,
                values = ~pneum,
                title = "DR by 10,000",
                opacity = 0.7) %>% 
      addLegend("bottomright",
                pal = pal_border,
                values = ~majority_vote,
                title = "wealth (border color)",
                opacity = 0.7)
    
  })
  
  
  ### whooping tab  
  output$whooping <- renderLeaflet({
    # color palette
    
    pal_whooping <- colorBin(palette = "OrRd", bins = c(0, 10, 19, 28, 37, 46), domain = spatial_data$whooping, na.color = "grey")
    pal_border <- colorFactor(palette = "Greens", domain = unique(spatial_data$majority_vote))
    
    # labels
    labels_whooping <- sprintf(
      "<strong>Year: </strong>%g<br><strong>Cause: </strong>%s<br><strong>Neighborhood: </strong>%s<br><strong>Wealth: </strong>%s<br><strong>DR: </strong>%g",
      selected_year()$year, "whooping cough", selected_year()$buurt1897, selected_year()$majority_vote, round(selected_year()$whooping, 0)) %>% 
      lapply(htmltools::HTML)
    
    # map
    selected_year() %>% 
      st_transform(4326) %>% 
      leaflet() %>% 
      addPolygons(label = labels_whooping,
                  stroke = TRUE,
                  #smoothFactor = 0.7,
                  weight = 3.5,
                  #color = "black",
                  color = ~pal_border(selected_year()$majority_vote),
                  opacity = 0.7,
                  fillOpacity = 0.6,
                  fillColor = ~pal_whooping(selected_year()$whooping),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "white",
                                                      opacity = 1, 
                                                      bringToFront = TRUE)) %>% 
      addLegend("bottomright",
                pal = pal_whooping,
                values = ~whooping,
                title = "DR by 10,000",
                opacity = 0.7) %>% 
      addLegend("bottomright",
                pal = pal_border,
                values = ~majority_vote,
                title = "wealth (border color)",
                opacity = 0.7)
    
  })
  
  ### diphtheria tab  
  output$diphth <- renderLeaflet({
    # color palette
    
    pal_diphth <- colorBin(palette = "OrRd", bins = c(0, 11, 21, 32, 42, 53), domain = spatial_data$diphth, na.color = "grey")
    pal_border <- colorFactor(palette = "Greens", domain = unique(spatial_data$majority_vote))
    
    # labels
    labels_diphth <- sprintf(
      "<strong>Year: </strong>%g<br><strong>Cause: </strong>%s<br><strong>Neighborhood: </strong>%s<br><strong>Wealth: </strong>%s<br><strong>DR: </strong>%g",
      selected_year()$year, "diphtheria", selected_year()$buurt1897, selected_year()$majority_vote, round(selected_year()$diphth, 0)) %>% 
      lapply(htmltools::HTML)
    
    # map
    selected_year() %>% 
      st_transform(4326) %>% 
      leaflet() %>% 
      addPolygons(label = labels_diphth,
                  stroke = TRUE,
                  #smoothFactor = 0.7,
                  weight = 3.5,
                  #color = "black",
                  color = ~pal_border(selected_year()$majority_vote),
                  opacity = 0.7,
                  fillOpacity = 0.6,
                  fillColor = ~pal_diphth(selected_year()$diphth),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "white",
                                                      opacity = 1, 
                                                      bringToFront = TRUE)) %>% 
      addLegend("bottomright",
                pal = pal_diphth,
                values = ~diphth,
                title = "DR by 10,000",
                opacity = 0.7) %>% 
      addLegend("bottomright",
                pal = pal_border,
                values = ~majority_vote,
                title = "wealth (border color)",
                opacity = 0.7)
    
  })
  
  ### diarrheal tab  
  output$diarrh <- renderLeaflet({
    # color palette
    
    pal_diarrh <- colorBin(palette = "OrRd", bins = c(0, 8, 14, 21, 29, 42, 77, 204), domain = spatial_data$diarrh, na.color = "grey")
    pal_border <- colorFactor(palette = "Greens", domain = unique(spatial_data$majority_vote))
    
    # labels
    labels_diarrh <- sprintf(
      "<strong>Year: </strong>%g<br><strong>Cause: </strong>%s<br><strong>Neighborhood: </strong>%s<br><strong>Wealth: </strong>%s<br><strong>DR: </strong>%g",
      selected_year()$year, "diarrheal", selected_year()$buurt1897, selected_year()$majority_vote, round(selected_year()$diarrh, 0)) %>% 
      lapply(htmltools::HTML)
    
    # map
    selected_year() %>% 
      st_transform(4326) %>% 
      leaflet() %>% 
      addPolygons(label = labels_diarrh,
                  stroke = TRUE,
                  #smoothFactor = 0.7,
                  weight = 3.5,
                  #color = "black",
                  color = ~pal_border(selected_year()$majority_vote),
                  opacity = 0.7,
                  fillOpacity = 0.6,
                  fillColor = ~pal_diarrh(selected_year()$diarrh),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "white",
                                                      opacity = 1, 
                                                      bringToFront = TRUE)) %>% 
      addLegend("bottomright",
                pal = pal_diarrh,
                values = ~diarrh,
                title = "DR by 10,000",
                opacity = 0.7) %>% 
      addLegend("bottomright",
                pal = pal_border,
                values = ~majority_vote,
                title = "wealth (border color)",
                opacity = 0.7)
    
  })
  
}

#################################### COMPILING APP

shinyApp(ui, server)