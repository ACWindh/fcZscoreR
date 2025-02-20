test_that("metricGA works", {
  expect_equal(round(metricGA("23+5"),2), 23.71)
  expect_equal(metricGA("30") , 30)
  expect_equal(round(metricGA(c("23+5", "30+0")),2), c(23.71, 30))
})
