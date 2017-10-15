
# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
countrydata <- bigdata[sample.int(nrow(bigdata), 100),]

function(input, output, session) {

  ########################################################################################################
  #*******************************************INTERACTIVE MAP TAB****************************************#

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 70, lat = 25, zoom = 3)
  })
  
  # A reactive expression that returns the set of countries that are
  # in bounds right now
  countriesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(countrydata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(countrydata,
          latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    
    colorData <- countrydata[[colorBy]]
    pal <- colorBin("viridis", colorData, 7, pretty = FALSE)

    radius <- 200000
    
    leafletProxy("map", data = countrydata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~countrycode,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  # Show a popup at the given location
  showCountryPopup <- function(countrycode, lat, lng) {
    selectedCountry <- bigdata[bigdata$countrycode == countrycode,]
    content <- as.character(tagList(
      tags$strong(HTML(sprintf("%s, %s",
                               selectedCountry$countryname, selectedCountry$countrycode
      ))), tags$br(),
      sprintf("Percentage Population Affected, 2015: %s", selectedCountry$affectedpoppercent2015), tags$br(),
      sprintf("GDP (in millions of USD), 2015: %s", dollar(selectedCountry$gdp)), tags$br(),
      sprintf("CCPI, 2015: %s", selectedCountry$ccpi), tags$br(),
      sprintf("HAQI, 2015: %s", selectedCountry$haqi2015), tags$br(),
      sprintf("Gini Coefficient, 2013: %s", selectedCountry$haqi2015), tags$br(),
      sprintf("Adult Literacy Rate, 2015: %s", selectedCountry$literacy2015)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = countrycode)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showCountryPopup(event$id, event$lat, event$lng)
    })
  })
  
  ########################################################################################################
  #*******************************************ECONOMY TAB***************************************************#
  
  output$gdpIntensityMap <- renderGvis({gvisIntensityMap(gdpdata,locationvar = "countrycode", numvar=c("gdp","affectedpoppercent2015"),
                                                          options=list
                                                          (#colors="['#4682b4', '#0073CF','#0073CF']",
                                                            height = '100%',
                                                            width = '100%')
                                                          
  )})
  
  gdpdataframe <- reactive({
    df <- bigdata %>%
      select(countryname, gdp, affectedpoppercent2015,
             region,gini) %>%
      arrange(region)
  })
  
  output$gdpBubbleChart <- renderGvis({
    gvisBubbleChart(gdpdataframe(),
                    idvar = "countryname",
                    xvar = "gdp",
                    yvar = "affectedpoppercent2015",
                    colorvar = "region",
                    size = 'gini',
                    options = list(
                      height='500px',
                      #explorer={},
                      fontName = "Source Sans Pro",
                      fontSize = 13,
                      hAxis='{title: "Gross Domestic Product (millions of USD)"}',
                      hAxis.maxValue= max(bigdata$gdp+5), 
                      hAxis.minValue= min(bigdata$gdp-10),
                      vAxis ='{title: "Percentage of Population Affected (%)"}',
                      vAxis.maxValue= max(bigdata$affectedpoppercent2015+10), 
                      vAxis.minValue= min(bigdata$affectedpoppercent2015-10),
                      title = sprintf("Gross Domestic Product vs. Affected Population, 2015 (Sized by Gini Coefficient)"),
                      titleTextStyle = '{fontSize: 16}',
                      tooltip = '{textStyle: {fontSize: 12}}'
                    ),
                    chartid= "gdpBubbleChart"
    )
  })
  
  output$gdpPlot1 <-renderPlot(ggplot(GDP, aes(x = gdp2015, y = prevalence, color = factor(GDP$region))) + geom_point() + xlab('Gross Domestic Product (millions of USD)') + ylab("Percentage of Population Affected (%)") + ggtitle("Gross Domestic Product vs. Affected Population, 2015")+ labs(color="Region"))
  output$gdpPlot <- renderPlot(ggplot(GDP, aes(x = log(gdp2015), y = log(prevalence), color = factor(GDP$region))) + geom_point() + geom_smooth(method = 'lm', se = TRUE, color = "red") 
                               + xlab("log(Gross Domestic Product (millions of USD))") + ylab("log(Percentage of Population Affected (%))") + ggtitle("log(Gross Domestic Product) vs. log(Affected Population), 2015") + labs(color="Region"))
  
  output$giniPlot <- renderPlot(ggplot(Gini, aes(x = gini, y = prevalence, color = factor(Gini$Region), shape = factor(Gini$Cluster))) + geom_point()
                                + xlab("Gini Coefficient (%)") + ylab("Percentage of Affected Population (%)") + ggtitle("Gini Coefficient vs. Affected Population, 2015")+labs(color="Region")+labs(shape="K-means Clusters"))
  
  ########################################################################################################
  #*******************************************CLIMATE CHANGE TAB***************************************************#
  
  output$ccpiIntensityMap <- renderGvis({gvisIntensityMap(ccpidata,locationvar = "countrycode", numvar=c("ccpi","affectedpoppercent2015"),
                                                            options=list
                                                              (colors="['#4682b4', '#0073CF','#0073CF']",
                                                              height = '100%',
                                                              width = '100%')
                                                  
                                                            )})
  ccpidataframe <- reactive({
    df <- bigdata %>%
      select(countryname, ccpi, affectedpoppercent2015,
             region,gdp) %>%
      arrange(region)
  })
  
  output$ccpiBubbleChart <- renderGvis({
    gvisBubbleChart(ccpidataframe(),
                    idvar = "countryname",
                    xvar = "ccpi",
                    yvar = "affectedpoppercent2015",
                    colorvar = "region",
                    size = 'gdp',
                    options = list(
                      height='500px',
                      #explorer={},
                      fontName = "Source Sans Pro",
                      fontSize = 13,
                      hAxis='{title: "Climate Change Performance Index (no unit)"}',
                      hAxis.maxValue= max(bigdata$ccpi+5), 
                      hAxis.minValue= min(bigdata$ccpi-10),
                      vAxis ='{title: "Percentage of Population Affected (%)"}',
                      vAxis.maxValue= max(bigdata$affectedpoppercent2015+10), 
                      vAxis.minValue= min(bigdata$affectedpoppercent2015-10),
                      title = sprintf("Climate Change Performance Index vs. Affected Population, 2015"),
                      titleTextStyle = '{fontSize: 16}',
                      tooltip = '{textStyle: {fontSize: 12}}'
                    ),
                  chartid= "ccpiBubbleChart"
    )
  })

  ########################################################################################################
  #******************************************ADULT LITERACY TAB****************************************#
  
  output$lrIntensityMap <- renderGvis({gvisIntensityMap(lrdata,locationvar = "countrycode", numvar=c("literacy2015","affectedadultpoppercent2015"),
                                                          options=list
                                                          (colors="['#4682b4', '#0073CF','#0073CF']",
                                                            height = '100%',
                                                            width = '100%')
                                                          
  )})
  
  yearLiteracyData <- reactive({
  
    df <- literacydata %>%
      filter(year == input$lryear) %>%
      select(countryname, literacyrate, affectedadultpoppercent,
             region,haqi) %>%
      arrange(region)
  })

  output$lrBubbleChart <- renderGvis({
    gvisBubbleChart(yearLiteracyData(),
                    idvar = "countryname",
                    xvar = "literacyrate", 
                    yvar = "affectedadultpoppercent",
                    colorvar = "region",
                    sizevar = "haqi",
                    options = list(
  
                      height='500px',
                      #explorer={},
                      fontName = "Source Sans Pro",
                      fontSize = 13,
                      hAxis='{title: "Literacy Rate, Population 15+ Years (%)"}',
                      hAxis.maxValue= max(literacydata$literacyrate+5), 
                      hAxis.minValue= min(literacydata$literacyrate-5),
                      vAxis ='{title: "Percentage of Population (15+ Years) Affected (%)"}',
                      vAxis.maxValue= max(literacydata$affectedadultpoppercent+10), 
                      vAxis.minValue= min(literacydata$affectedadultpoppercent-10),
                      title = sprintf(
                        "Adult Literacy Rate vs. Percentage of Population Affected, Ages 15+, %s (Sized by HAQI)",
                        input$lryear),
                      titleTextStyle = '{fontSize: 16}',
                      tooltip = '{textStyle: {fontSize: 12}}'
                      #bubble = '{opacity: 0.4, stroke: "none"}'
                    )
    )
  })
  
   output$lrHistogram <- renderGvis({
     gvisHistogram(lrhistdata,
                   options = list(
                     width='500px',
                     height='650px',
                     hAxis='{title: "Adult Literacy Rate, Population 15+ Years (%)"}',
                     vAxis='{title: "Frequency"}',
                       title="Histogram of Adult Literacy Rate by Region, 2015",
                       legend="{ position: 'none' }",
                       colors="['#5C3292', '#1A8763', '#871B47']"
                   ),
                   chartid = "LiteracyRate"
     )
   })
   
   output$adultPopHistogram <- renderGvis({
     gvisHistogram(adultpophistdata,
                   options = list(
                     width='500px',
                     height='650px',
                     hAxis='{title: "Percentage of Affected Population 15+ Years (%)"}',
                     vAxis='{title: "Frequency"}',
                     title="Histogram of Percentage of Population 15+ Years Affected by Region, 2015",
                     legend="{ position: 'none' }",
                     colors="['#5C3292', '#1A8763', '#871B47']"
                   ),
                   chartid = "AdultPopulationAffected"
     )
   })
  
  output$lrPlot<-renderPlot(ggplot (lr2, aes(x = lr2$literacyrate, y = lr2$affectedadultpoppercent, color = lr2$region)) + geom_point() + labs (color = "Region")+ xlab ("Adult Literacy Rate (%)") + ylab ("HIV Prevalence (%)") + ggtitle("Adult (15+) Literacy Rate vs. HIV Prevalence in 94 Countries, 2015")+
                              geom_smooth(method = "auto", se = TRUE, color = "red"))
  
  output$lrPlot2<-renderPlot(ggplot (lr_truncated, aes(x = lr_truncated$'change in literacy', y = lr_truncated$'Change in HIV percent', color = lr_truncated$'Region')) + geom_point() + labs (color = "Region") + xlab ("Increase in Literacy Rate (%)") + ylab ("Increase in HIV Prevalence (%)") + ggtitle("Effects of Improvement in Literacy Rate on HIV Prevalence in 45 Countries")+
                               geom_smooth(method = "lm", se = TRUE, color = "red"))

########################################################################################################
#*******************************************HEALTHCARE TAB***************************************************#
  
  output$haqiIntensityMap <- renderGvis({gvisIntensityMap(haqidata,locationvar = "countrycode", numvar=c("haqi2015","affectedpoppercent2015"),
                                                         options=list
                                                         (#colors="['#4682b4', '#0073CF','#0073CF']",
                                                           height = '100%',
                                                           width = '100%')
                                                         
  )})
  
  haqidataframe <- reactive({
    df <- bigdata %>%
      select(countryname, haqi2015, affectedpoppercent2015,region,gini,gdp) %>%
      arrange(region)
  })
  
  output$haqiBubbleChart <- renderGvis({
    gvisBubbleChart(haqidataframe(),
                    idvar = "countryname",
                    xvar = "haqi2015",
                    yvar = "affectedpoppercent2015",
                    colorvar = "region",
                    sizevar = "gdp",
                    options = list(
                      #width='500px',
                      height='500px',
                      #explorer={},
                      fontName = "Source Sans Pro",
                      fontSize = 13,
                      #chartArea="{right: '0', left: 500}",
                      hAxis='{title: "Healthcare Access and Quality Index (no unit)"}',
                      #hAxis.maxValue= max(bigdata$haqi2015+5), 
                      #hAxis.minValue= min(bigdata$haqi2015-5),
                      vAxis ='{title: "Percentage of Population Affected (%)"}',
                      #vAxis.maxValue= max(bigdata$affectedpoppercent2015+10), 
                      #vAxis.minValue= min(bigdata$affectedpoppercent2015-10),
                      title = sprintf("Healthcare Access and Quality Index vs. Affected Population, 2015 (Sized by GDP)"),
                      titleTextStyle = '{fontSize: 16}',
                      tooltip = '{textStyle: {fontSize: 12}}'
                    )
    )
  })
  
  output$haqiPlot <-renderPlot(ggplot (haqi, aes(x = haqi$haqi2015, y = haqi$affectedpoppercent2015, color = haqi$region)) + geom_point() + labs (color = "Region") + xlab ("Healthcare Access and Quality Index (no units)") + ylab ("Percentage of Population Affected (%)") + ggtitle("Healthcare Access and Quality Index vs. Affected Population, 2015")+
                                 geom_smooth(method = "lm", se = TRUE, color = "red"))
  
  output$haqiPlot2 <-renderPlot(ggplot (haqi2, aes(x = haqi2$haqi2015, y = haqi2$affectedpoppercent2015, color = haqi2$gini)) + geom_point() + labs (color = "Gini Coefficient (%)") + xlab ("Healthcare Access and Quality Index (no units)") + ylab ("Percentage of Population Affected (%)") + ggtitle("Healthcare Access and Quality Index vs. Affected Population, 2015"))
  
}
