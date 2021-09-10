library(leaflet)
library(vinvSchema)
library(TreeSpeciesI18N)
library(h3jsr)
library(sf)

library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)


library(htmlwidgets)
library(shiny)
library(shinyjs)
library(geosphere)
library(ggplot2)
library(raster)
library(rgeos)

# https://github.com/obrl-soil/h3jsr
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example

function(input, output, session) {
    
    vinv <- reactiveValues()
    vinv$fixed <- FALSE
    vinv$hasData <- FALSE

    output$exampleFile <- renderUI({
      tags$a(href = "https://schema.vinv.io/src/0.0.1-alpha.0/examples/files/try_to_break.vinv", "... download example file", target="_blank",  download="try_to_break.vinv")
    })
    output$vinv <- renderUI({
      tags$a(href = "https://vinv.io", "about vinv.io", target="_blank")
    })
    
    getH3CellIndex <- function(longitude, latitude, h3res){
        bth <- sf::st_sfc(st_point(c(as.double(longitude), as.double(latitude))), crs = 4326)
        h3jsr::point_to_h3(bth, res = h3res  )
    }
    getCenterFromH3 <- function(h3_address){
      h3jsr::h3_to_point(h3_address = h3_address, simple = TRUE)
    }
    calRadius <- function(rad){
      pi * rad ^ 2
    }
    getPopup <- function(id, height, dbh, speciesName){
        paste(
            "Species:<b>", speciesName, "</b><br/>",
            "Height: ", height, " m", "</b><br/>",
            "Dbh: ", dbh, " cm"
        )
    }
    
    getPolygonPlotPopup <- function(e){
      
      ndf <- subset(vinv$data$inventory$tree_status, prevR::point.in.SpatialPolygons(vinv$data$inventory$tree_status$longitude, vinv$data$inventory$tree_status$latitude, e) )

      paste(
        "id: ", "<br/>",
        "Trees in plot: ", nrow(ndf), "<br/>",
        "Area: ", round(raster::area(e) * 100, 3), "ha", "<br/>",
        "max. Height: ", toString(max(ndf$height)), "m", "<br/>"
        
      )
    }
    getTreesOverPlot <- function(radius, targetLon, targetLat){

      df <- vinv$data$inventory$tree_status

      df$overPlot <- mapply(df$longitude, df$latitude, FUN = function(lon, lat){
        calDistanceTo(lon, lat, targetLon, targetLat)
      })
      ss <- subset(df, overPlot <= radius)

       return(ss)
    }
    getPlotPopup <- function(id, radius, targetLon, targetLat){
      # test <- as.data.frame(c(1,2))
      # ndf <- as.data.frame(c())
      # 
      # for (i in 1:length(targetLon)){ #tabs holds multiple tables as a list
      #   ndf <- append(ndf, getTreesOverPlot(radius[i], targetLon[i], targetLat[i]))
      # }
      # 
      # print('----')
      # print(typeof(test))
      # print(typeof(ndf))
      # print(typeof(radius))
      paste(
        "ID: ", id, "<br/>",
        # "Trees in plot: ", length(ndf$id), test, "<br/>",
        "Area: ", round(calRadius(radius)/10000, 3), "ha", "<br/>",
        "Longitude: ", targetLon, "<br/>",
        "Latitude: ", targetLat
      )
    }
    # Create the map
    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(doubleClickZoom = FALSE, zoomControl = FALSE)) %>%
            addTiles() %>%
            setView(lng = 13.9692496, lat = 47.3934771, zoom = 14) %>%
            onRender(
                "function(el,x){
                    /*this.on('mousemove', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        Shiny.onInputChange('hover_coordinates', [lat, lng])
                    });*/
                    this.on('dblclick', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        Shiny.onInputChange('fix_cursor_coordinates', [lat, lng])
                        e.preventDefault()
                        e.stopPropagation()
                        return false;
                    });
                    /*this.on('mouseout', function(e) {
                        Shiny.onInputChange('clicked', null)
                    })*/
                }"
            )
    })
    observe({
        coords <- input$hover_coordinates
        radius <- input$radius
        
        if(input$useH3 == TRUE){
          leafletProxy("map", data = df) %>%
            removeShape(layerId = "cursor")
          return()
        }
        
        if(!vinv$fixed){
            if(is.null(input$hover_coordinates)) {
                leafletProxy("map", data = df) %>%
                    removeShape(layerId = "cursor")
            } else if(input$useH3 == FALSE) {
                leafletProxy("map", data = df) %>%
                  addMapPane("hcCursor", zIndex = 200) %>%
                    #removeShape(layerId = "cursor") %>%
                    addCircles(lat = input$hover_coordinates[1], lng = input$hover_coordinates[2], radius = radius, weight = 2, color = "#50b68d",
                               fillColor = "#50b68d", fillOpacity = 0.5, layerId = "cursor",
                               options = pathOptions(pane = "hcCursor"),)
            }
        }else{
            leafletProxy("map", data = df) %>%
              addMapPane("hcCursor", zIndex = 200) %>%
                #removeShape(layerId = "cursor") %>%
                addCircles(lat = vinv$latitude, lng = vinv$longitude, radius = radius, weight = 2, color = "#50b68d",
                           fillColor = "#50b68d", fillOpacity = 0.5, layerId = "cursor",
                           options = pathOptions(pane = "hcCursor"))
        }
    })
    calDistanceTo = function(srcLongitude, srcLatitude, targetLongitude, targetLatitude){
      
      point_mat <- matrix(c(c(srcLongitude, targetLongitude), c(srcLatitude, targetLatitude)), ncol = 2)
      #point_mat <- matrix(c(c(srcLongitude, srcLatitude), c(targetLongitude, targetLatitude)), ncol = 2)
      
      #geosphere::distMeeus(c(srcLongitude, srcLatitude), c(targetLongitude, targetLatitude))
      geosphere::distMeeus(point_mat) 
      
    }
    calDistance = function(longitude, latitude){
      
      calDistanceTo(longitude, latitude, vinv$longitude, vinv$latitude)
        
       # point_mat <- matrix(c(c(longitude, coordinates$longitude), c(latitude, coordinates$latitude)), ncol = 2)
        
        # geospatial_dist <- distm(point_mat, fun = distGeo)  

       # distMeeus(point_mat) 
    }
    updateDistance <- function(){
        if(!is.null(vinv$data)){
            
            df <-  vinv$data$inventory$tree_status
            
            vinv$data$inventory$tree_status$distance <- mapply(df$longitude, df$latitude, FUN = function(lon, lat){
                calDistance(lon, lat)
            })
            vinv$data$inventory$tree_status$h3 <- mapply(df$latitude, df$longitude, FUN = function(lon, lat){
              getH3CellIndex(lon, lat, input$h3res)
            })
            
            
            df <-  vinv$data$areas$plot$circles
            
            vinv$data$areas$plot$circles$distance <- mapply(df$longitude, df$latitude, FUN = function(lon, lat){
              calDistance(lon, lat)
            })
            vinv$data$areas$plot$circles$h3 <- mapply(df$latitude, df$longitude, FUN = function(lon, lat){
              getH3CellIndex(lon, lat, input$h3res)
            })
        }
    }
    output$plotHeight <- renderPlot({
      par(mar = c(0,0,0,0))
      if(!is.null(vinv$filtered) && nrow(vinv$filtered) > 0){
          
        if(input$groupBySpecies){
          g <- ggplot(vinv$filtered, mapping = aes(y=height, x=speciesName, fill = paste('#', colorData)))
          g <- g + geom_boxplot(show.legend = FALSE)
          g <- g + labs(x = "Name of Species", y = "Height (meter)", title = "Tree heights by species")
          g + theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
        }else{
          g <- ggplot(vinv$filtered, mapping = aes(y=height, x=factor(0)))
          g <- g + geom_boxplot(show.legend = FALSE)
          g <- g + labs(y = "Height (meter)", title = "Tree heights")
          g + theme(axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank())
        }
      }
    })
    output$plotDBH <- renderPlot({
        if(!is.null(vinv$filtered) && nrow(vinv$filtered) > 0){

          par(mar = c(0,0,0,0))

          if(input$groupBySpecies){
            g <- ggplot(vinv$filtered, mapping = aes(y=dbh, x=speciesName, fill = paste('#', colorData)))
            g <- g + geom_boxplot(show.legend = FALSE)
            g <- g + labs(x = "Name of Species", y = "dbh (cm)", title = "Mean DBH by species")
            g + theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
          }else{
            g <- ggplot(vinv$filtered, mapping = aes(y=dbh, x=c("Trees")))
            g <-  g + geom_boxplot(show.legend = FALSE)
            g <- g + labs(x = "all trees", y = "dbh (cm)", title = "Mean DBH")
            g + theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())
          }
        }
    })
    calVol <- function(dbh, height){
      ((dbh/100)^2 * pi / 4) * height * 0.45
    }
    #5.65575034939545
    #8.011846664817371
    
    treesOnPlot <- function(){
      print('treesOnPlot')
      df <- data.frame()
      
      for(i in 1:ncol(vinv$filteredPlots)){

        subSet <- subset(vinv$filtered, vinv$filteredPlots$radius[i] >= calDistanceTo(longitude, latitude, vinv$filteredPlots$longitude[i], vinv$filteredPlots$latitude[i]))
        
        # multiply volume by hectar

        if(nrow(subSet) != 0){
          subSet$hectarVolume <- mapply(subSet$vol, i, FUN = function(x, y){
            print(y)
            x / vinv$filteredPlots$area[y]
          })
          
          df <- rbind(df, subSet)
        }
      }
      df
    }
    #http://www.fastossiach.at/images/pdf/Forsteinrichtung.pdf
    output$plotVol <- renderPlot({
      if(!is.null(vinv$filtered) && nrow(vinv$filtered) > 0 && !is.null(vinv$filteredPlots) && nrow(vinv$filteredPlots) > 0){
        
        par(mar = c(0,0,0,0))
        # area of Plot habe ich!!!
        meanDbh <- mean(vinv$filtered$dbh)
        meanHeight <- mean(vinv$filtered$height)
        selectionArea <- calRadius(input$radius) / 10000
        
        trees <- treesOnPlot()
        trees$selectionVolume <- mapply( trees$hectarVolume, FUN = function(x){
          x * selectionArea
        })
        
        if(input$groupBySpecies){
          g <- ggplot(trees, aes(y=selectionVolume, x=speciesName, fill = paste('#', colorData)))
          g <- g + geom_bar(stat = "identity", show.legend = FALSE)
          g <- g + labs(x = "Name of Species", y = "volume (m^3)", title = "Volume by species")
          g + theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
        }else{
          g <- ggplot(trees, aes(y=selectionVolume, x=c("Trees")))
          g <- g + geom_bar(stat = "identity", show.legend = FALSE)
          g <- g + labs(x = "all trees", y = "volume (m^3)", title = "Volume")
          g + theme(axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank())
        }
          
        
        
      }
    })
    getPlotsInCircle <- function(){
      
    }
    output$dataInRadius <- renderText({
        
        radius <- input$radius
        
        
        if(!is.null(vinv$data) && !is.null(vinv$longitude)){
            
            df <-  vinv$data$inventory$tree_status
            
            if(input$useH3 == TRUE){
              
              coords <- input$fix_cursor_coordinates
              h3_address <- getH3CellIndex(coords[1], coords[2], input$h3res)

              vinv$filtered <- df %>%
                filter(h3 == h3_address)
              
            }else{

              vinv$filtered <- df %>%
                filter(distance <= radius)
              
            }

            return ( paste(nrow(vinv$filtered), 'trees within the selection') )
        }
        "no data"
    })
    output$plotsInRadius <- renderText({
      
      cursoreRadius <- input$radius
      
      if(!is.null(vinv$data) && !is.null(vinv$longitude)){

        dfPlots <-  vinv$data$areas$plot$circles
        
        if(input$useH3 == TRUE){
          
          coords <- input$fix_cursor_coordinates
          h3_address <- getH3CellIndex(coords[1], coords[2], input$h3res)
          
          vinv$filteredPlots <- dfPlots %>%
            filter(h3 == h3_address)
          
        }else{
          
          vinv$filteredPlots <- dfPlots %>%
            filter(distance <= cursoreRadius)
          
        }
        
        return ( paste(nrow(vinv$filteredPlots), 'plots within the selection') )
      }
      "no plot"
    })
    # observe({
    #   plotCount <- vinv$filteredPlots
    #   print(plotCount)
    # })
    
    output$vinvHasData <- reactive({
      vinv$hasData
    })
    output$positionSelected <- reactive({
      !is.null(vinv$latitude)
    })
    outputOptions(output, "vinvHasData", suspendWhenHidden = FALSE)
    outputOptions(output, "positionSelected", suspendWhenHidden = FALSE)
    
    output$radiusInHa <- renderText({
      if(!vinv$hasData || is.null(vinv$latitude)){
        return("select position")
      }
      
      if(input$useH3 == TRUE){
        h3_address <- getH3CellIndex(vinv$latitude, vinv$longitude, input$h3res)
        qm <- h3jsr::cell_area(h3_address = h3_address, 'm2')
        return( paste( round(qm, 1), "m² ≈", round(qm/10000, 3), "ha") ) 
      }else{
        qm <- calRadius(input$radius)
        return( paste( round(qm, 1), "m² ≈", round(qm/10000, 3), "ha") ) 
      }
        
    })
    
    output$flexCoordinates <- renderText({
        
        coords <- input$hover_coordinates
        radius <- input$radius
        
        if(vinv$fixed) return ("")
        
        if(is.null(input$hover_coordinates)) {
            "Double click on map to select position"
        } else {
            
            paste0(
                "Latitude: ", coords[1],
                "\nLongitude: ", coords[2],
                "\nH3 Cell Index: ", getH3CellIndex(coords[1], coords[2], input$h3res)
            )        
        }
    })
    
    
    output$fixedCoordinates <- renderText({

        coords <- input$fix_cursor_coordinates

        if(is.null(coords) || is.null(coords[1]) || is.null(coords[2])){
            return (paste0(
                ""
            ))
        }
        
        if(input$useH3){
            h3_address <- getH3CellIndex(coords[1], coords[2], input$h3res)
            coordST <- getCenterFromH3(h3_address)
            coords <- st_coordinates(coordST)
        }
            
        
        vinv$latitude <- coords[1]
        vinv$longitude <- coords[2]
        vinv$fixed <- TRUE
        
        updateDistance()
        
        paste0(
            "Latitude: ", coords[1],
            "\nLongitude: ", coords[2],
            "\nH3 Cell Index: ", getH3CellIndex(coords[1], coords[2], input$h3res)
        )
        
    })
    observe({
        leafletProxy("map") %>%
            removeShape(layerId = "h3")
            
        if(input$useH3 && !is.null(vinv$latitude)){
            h3Index <- getH3CellIndex(vinv$latitude, vinv$longitude, input$h3res)
            polygon <- h3_to_polygon(input = h3Index, simple = FALSE)

            mxCoords <- st_coordinates(polygon)

            leafletProxy("map") %>%
              addMapPane("h3Cursor", zIndex = 200) %>%
                addPolygons(lat = mxCoords[,'X'] , lng = mxCoords[,'Y'], weight = 2, color = "#50b68d",
                            fillColor = "#50b68d", fillOpacity = 0.5, layerId = "h3",data = polygon,
                            options = pathOptions(pane = "h3Cursor"
                           ),
                )
        }
        
    })

    dataInput <- reactive({
        file <- input$vinvfile
        
        if(!is.null(file)){
           vinvSchema::fromFile(file$datapath, pretty = TRUE)
        }else{
            NULL
        }
    })
    
    observe({
        
        
        if(is.null(vinv$data)){
           
            vinv$data <- dataInput()
           
            
    
            if(!is.null(vinv$data)){
              vinv$hasData <- TRUE

                vinv$data$inventory$tree_status$speciesName <- sapply( vinv$data$inventory$tree_status$species, function(x){
                  treelist <- TreeSpeciesI18N::decode(x, 'en')
                  treelist[1]
                })
                
                vinv$data$inventory$tree_status$colorData <- sapply( vinv$data$inventory$tree_status$species, function(x){
                  TreeSpeciesI18N::getColorFromId(x)
                })
                vinv$data$inventory$tree_status$vol <- mapply( vinv$data$inventory$tree_status$height, vinv$data$inventory$tree_status$dbh, FUN = function(height, dbh){
                  calVol(dbh, height)
                })
                
                vinv$data$areas$plot$circles$area <- sapply( vinv$data$areas$plot$circles$radius, function(x){
                  calRadius(x)/10000
                })
              
                df <-  vinv$data$inventory$tree_status
                
                leafletProxy(mapId = "map", data = df) %>%
                  clearShapes() %>%
                  addMapPane("vinv_area", zIndex = 300) %>%
                  addMapPane("vinv_plots_circle", zIndex = 400) %>%
                  addMapPane("vinv_trees", zIndex = 1000) %>%
                  addCircles(
                    data=df,
                    lat = ~latitude,
                    lng = ~longitude,
                    radius = (~dbh /100 * 2),
                    weight = 3,
                    color = paste0('#', ~colorData),
                    fillColor = paste0('#', ~colorData),
                    fillOpacity = 1, 
                    popup = ~getPopup(id, height, dbh, speciesName),
                    options = pathOptions(pane = "vinv_trees"),
                    layerId = ~id
                  ) %>% # TREES
                  addPolygons(
                    data = vinv$data$areas$area_status$polygons,
                    fillColor = "#444444",
                    color = "#444444",
                    weight = 2,
                    opacity = 0,
                    fillOpacity = 0.2,
                    # highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                    # popup = getPolygonPlotPopup(vinv$data$areas$area_status$polygons@polygons[[1]]@ID),
                    options = pathOptions(pane = "vinv_area"),
                    # layerId = ~polygons@ID
                  )  %>% # PLOT
                  addCircles(
                    data = vinv$data$areas$plot$circles, 
                    lng = ~longitude, 
                    lat = ~latitude, 
                    weight = 1,
                    radius = ~radius,
                    popup = ~getPlotPopup(id, radius, longitude, latitude),
                    options = pathOptions(pane = "vinv_plots_circle"),
                    group = "vinv_plots_circle",
                    layerId = ~id
                  )
                
              if(length(vinv$data$areas$plot$polygons) > 0) {
                leafletProxy(mapId = "map", data = df) %>%
                  addMapPane("vinv_plots", zIndex = 200) %>%
                  addPolygons(
                    data = vinv$data$areas$plot$polygons,
                    color = "#444444",
                    weight = 2,
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    fillOpacity = 0.5,
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                    popup = getPolygonPlotPopup(vinv$data$areas$plot$polygons),
                    options = pathOptions(pane = "vinv_plots")
                  )
              }
            }
        }
    })
    
    
    
    output$treeTable <- DT::renderDataTable({

        df <-  vinv$data$inventory$tree_status

        df$speciesName <- sapply(df$species, function(x){
          
            treelist <- TreeSpeciesI18N::decode(x, 'en')
            
            treelist[1]
            
        })
        
        # df$h3 <- apply(df, 1, function(x){
        #     bth <- st_sfc(st_point(c(as.double(x["longitude"]), as.double(x["latitude"]))), crs = 4326)
        #     point_to_h3(bth, res = 15)
        # })
        
        if(!is.null(ncol(df))){
          vinv$hasData <- TRUE
        }else{
          return()
        }
          
        action <- DT::dataTableAjax(session, df, outputId = "treeTable")

        DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)

    })
}
