test_that("z_doppler works", {
  expect_equal(round(z_doppler(index="PI", value=1,
                      GA=36+4/7, return_value = "z"),4), 0.8734)
  expect_equal(round(z_doppler(index="PI", value=1,
                               GA=36+4/7, return_value = "centile"), 1), 80.9)

  })
