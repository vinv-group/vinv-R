#' @import jsonvalidate
#' @import rjson
#' @import readr

prettify <- function(tree_status) {

  prettified <- list()

  id <- c()

  for ( key in names(tree_status) ) {

    id <- append(id, key)

    prettified[[length(prettified)+1]] <- list(
      id = key,
      location = list(
        coordinates = list(tree_status[[key]][[1]][[1]][1], tree_status[[key]][[1]][[1]][2]),
        accuracy = unlist(tree_status[[key]][[1]][2])
      ),
      species = unlist(tree_status[[key]][2]),
      dbh = unlist(tree_status[[key]][3]),
      height = unlist(tree_status[[key]][4]),
      trunk = tree_status[[key]][5],
      crown = tree_status[[key]][6],
      birth = if (is.null(unlist(tree_status[[key]][7])) ) ? NULL : as.Date(unlist(tree_status[[key]][7])),
      image = if (is.null(unlist(tree_status[[key]][8])) ) ? list() : as.list(unlist(tree_status[[key]][8][1]))
    )

  }

  return ( data.frame(
    id
  ) )

  return ( prettified )
}

prettifyFd <- function(tree_status) {

  id <- c()
  species <- c()
  dbh <- c()
  height <- c()
  birth <- as.Date(c())


  for ( key in names(tree_status) ) {

    id <- append(id, key)
    species <- append(species, unlist(tree_status[[key]][2]))
    dbh <- append(dbh, unlist(tree_status[[key]][3]))
    height <- append(height, unlist(tree_status[[key]][4]))
    birth <- append(birth, unlist(tree_status[[key]][7]))

  }

  df <- data.frame(
    id,
    species,
    dbh,
    height,
    birth
  )

  return ( df )

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

      if(pretty == TRUE)
        json$inventory$tree_status <- prettifyFd( json$inventory$tree_status )

      return ( json );
    }

  }

  return ( NULL )
}

export <- function(x){x * 5}
