test_that("from file", {

  data <- fromFile('/Users/b-mac/sites/vinv/vinv-schema-R/try_to_break.vinv', pretty = TRUE)


  expect_type( data, "list")

  expect_type( data$areas$plot, "list")
})

# test_that("from json", {
#
#   raw <- readr::read_file( '/Users/b-mac/sites/vinv/vinv-schema-R/try_to_break.vinv' )
#
#   data <- fromString(raw, pretty = TRUE)
#
#   expect_type( data, "list")
#
# })
