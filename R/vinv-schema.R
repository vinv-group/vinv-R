#' @import jsonvalidate
#' @import rjson
#' @import readr

prettify <- function(tree_status) {

  prettified <- list()

  for ( key in names(tree_status) ) {

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
      birth = as.Date(unlist(tree_status[[key]][7])),
      image = as.list(unlist(tree_status[[key]][8][1]))
    )

  }

  return ( prettified )
}

import <- function( file, pretty = TRUE ){

  jsonStr <- readr::read_file( file )

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
        json$inventory$tree_status <- prettify( json$inventory$tree_status )

      return ( json );
    }

  }

  return ( NULL )
}

export <- function(x){x * 5}
