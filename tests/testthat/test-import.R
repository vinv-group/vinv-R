test_that("import json", {

  data <- import('/Users/b-mac/sites/vinv/vinv-schema-R/minimum.vinv', pretty = TRUE)

  print(data)

  expect_type( data, "list")

})
