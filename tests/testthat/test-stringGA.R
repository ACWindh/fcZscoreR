test_that("stringGA works", {
  expect_equal(stringGA(23.71), "23+5")
  expect_equal(stringGA(c(23.7, 30.23, 18.0)),
                        c("23+5", "30+2", "18+0"))
  expect_error(stringGA("23+1"),
               "Invalid input: input is not numeric")
})
