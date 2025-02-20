test_that("fetal_zscore works", {
  expect_equal(round(fetal_zscore(actual= 8.8, mod_value=36+6/7,
                card_param="PVA", moderator="EGA", out="z",
                method="Schneider"),2), 0.91)
})
