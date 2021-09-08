#' @import jsonvalidate
#' @import rjson
#' @import readr
#' @import sf

prettifyFd <- function(tree_status) {

  id <- c()
  longitude <- as.double(c())
  latitude <- as.double(c())
  altitude <- as.double(c())
  accuracy <- as.double(c())
  species <- c()
  dbh <- c()
  height <- c()
  birth <- as.Date(c())


  for ( key in names(tree_status) ) {


    id <- append(id, key)
    longitude <- append(longitude, tree_status[[key]][[1]][[1]][1] )
    latitude <- append(latitude, tree_status[[key]][[1]][[1]][2] )
    altitude <- append (altitude, tree_status[[key]][[1]][[1]][3])
    accuracy <- append (accuracy, tree_status[[key]][[1]][[2]])

    species <- append(species, unlist(tree_status[[key]][2]))
    dbh <- append(dbh, unlist(tree_status[[key]][3]))
    height <- append(height, unlist(tree_status[[key]][4]))
    birth <- append(birth, unlist(tree_status[[key]][7]))
  }

  df <- data.frame(
    id,
    longitude,
    latitude,
    altitude,
    accuracy,
    species,
    dbh,
    height,
    birth
  )

  return ( df )

}
prettifyPlotFd <- function(plot){


  vinvPolygonsAttributes <- data.frame()
  spatialPolygons <- list()

  vinvCircle <- data.frame(
    id = character(),
    longitude <- as.double(c()),
    latitude <- as.double(c()),
    altitude <- as.double(c()),
    accuracy <- as.double(c()),
    radius <- as.double(c())
  )

  for ( key in names(plot) ) {


    if(typeof(plot[key][[1]][[1]][[1]][1]) == 'list'){ # is polygon

      lon <- sapply(plot[key][[1]][[1]], function(e){
        as.numeric(e[[1]][1])
      })
      lat <- sapply(plot[key][[1]][[1]], function(e){
        as.numeric(e[[1]][2])
      })

      polys <- sp::Polygons(list(sp::Polygon(cbind(lon,lat))), key)
      spatialPolygons <- append(spatialPolygons, polys)

      # polygon <- sp::Polygon(coords = cbind(lat, lon))

      vinvPolygonsAttributes <- rbind(vinvPolygonsAttributes, data.frame(
        ID = key
      ))


    }else if(typeof(plot[key][[1]][[1]][[1]][1]) == 'double'){ # is circle
      # vinvCircle$id <- append(vinvCircle$id, key)

        vinvCircle <- rbind(vinvCircle, data.frame(
          id = key,
          longitude = as.double(plot[key][[1]][[1]][[1]][1]),
          latitude = as.double(plot[key][[1]][[1]][[1]][2]),
          altitude = as.double(plot[key][[1]][[1]][[1]][3]),
          accuracy = as.double(plot[key][[1]][[1]][[2]][1]),
          radius = as.double(plot[key][[1]][2])
        )
      )
    }
  }

  return ( list(
    polygons =  sp::SpatialPolygons(spatialPolygons),
    circles = vinvCircle
  ) )
}

fromFile <- function( file, pretty = TRUE ){

  jsonStr <- readr::read_file( file )

  return ( fromString(jsonStr, pretty) );

}
fromString <- function( jsonStr, pretty = TRUE ){

  json <- jsonlite::fromJSON( jsonStr )

  if (!exists("json")) {

    stop("Not valid json input file.")

  } else{

    if (is.null( json$v )){

      stop("Version code or $schema is required but does not exist.")

    }else{

      schemaUrl <- paste('https://schema.vinv.io/', json$v, '/vinv.min.json', sep="")

      schema <- readr::read_file( schemaUrl )

      jsonvalidate::json_validate(jsonStr, schema, verbose = TRUE, greedy = TRUE,
                                  error = TRUE, engine = "imjv")

      print( exists("json$areas") )

      if(pretty == TRUE)
        json$inventory$tree_status <- prettifyFd( json$inventory$tree_status )

      if(pretty == TRUE)
        json$areas$plots <- prettifyPlotFd( json$areas$plots )

      if(pretty == TRUE)
        json$areas$area_status <- prettifyPlotFd( json$areas$area_status )

      return ( json );
    }

  }

  return ( NULL )
}

export <- function(x){x * 5}
