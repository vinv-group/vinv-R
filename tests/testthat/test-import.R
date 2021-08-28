test_that("from file", {

  data <- fromFile('/Users/b-mac/sites/vinv/vinv-schema-R/minimum.vinv', pretty = TRUE)

  print(data$inventory$tree_status)

  expect_type( data, "list")

})

test_that("from json", {

  raw <- readr::read_file( '/Users/b-mac/sites/vinv/vinv-schema-R/minimum.vinv' )

  data <- fromString(raw, pretty = TRUE)

  print(data$inventory$tree_status)

  expect_type( data, "list")

})

test_that("prettifyFd", {

  raw <- readr::read_file( '/Users/b-mac/sites/vinv/vinv-schema-R/minimum.vinv' )

  data <- fromString(raw, pretty = FALSE)

  print( prettifyFd(data$inventory$tree_status) )

  expect_type( data, "list")

})
